package com.wavesplatform.block

import com.wavesplatform.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.block.serialization.BlockSerializer
import com.wavesplatform.common.merkle.Merkle.{hash, mkProofs, verify}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.crypto.*
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.settings.GenesisSettings
import com.wavesplatform.state.*
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.TxValidationError.GenericError
import monix.eval.Coeval
import play.api.libs.json.*

import scala.util.Try

case class BlockHeader(
    version: Byte,
    timestamp: Long,
    reference: ByteStr,
    baseTarget: Long,
    generationSignature: ByteStr,
    generator: PublicKey,
    featureVotes: Seq[Short],
    rewardVote: Long,
    transactionsRoot: ByteStr,
    stateHash: Option[ByteStr],
    challengedHeader: Option[ChallengedHeader]
) {
  val score: Coeval[BigInt] = Coeval.evalOnce((BigInt("18446744073709551616") / baseTarget).ensuring(_ > 0))
}

case class ChallengedHeader(
    timestamp: Long,
    baseTarget: Long,
    generationSignature: ByteStr,
    featureVotes: Seq[Short],
    generator: PublicKey,
    rewardVote: Long,
    stateHash: Option[ByteStr],
    headerSignature: ByteStr
)

case class Block(
    header: BlockHeader,
    signature: ByteStr,
    transactionData: Seq[Transaction]
) {
  import Block.*

  val id: Coeval[ByteStr] = Coeval.evalOnce(Block.idFromHeader(header, signature))

  val sender: PublicKey = header.generator

  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(BlockSerializer.toBytes(this))
  val json: Coeval[JsObject]     = Coeval.evalOnce(BlockSerializer.toJson(this))

  val blockScore: Coeval[BigInt] = header.score

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    if (header.version < Block.ProtoBlockVersion) copy(signature = ByteStr.empty).bytes()
    else PBBlocks.protobuf(this).header.get.toByteArray
  }

  protected val signedDescendants: Coeval[Seq[Signed]] = Coeval.evalOnce(transactionData.flatMap(_.cast[Signed]))

  private[block] val transactionsMerkleTree: Coeval[TransactionsMerkleTree] = Coeval.evalOnce(mkMerkleTree(transactionData))

  private[block] val originalHeader: Coeval[BlockHeader] =
    Coeval.evalOnce(
      header.challengedHeader
        .map { ch =>
          header.copy(
            baseTarget = ch.baseTarget,
            timestamp = ch.timestamp,
            generationSignature = ch.generationSignature,
            generator = ch.generator,
            featureVotes = ch.featureVotes,
            rewardVote = ch.rewardVote,
            stateHash = ch.stateHash,
            challengedHeader = None
          )
        }
        .getOrElse(header)
    )

  val signatureValid: Coeval[Boolean] = Coeval.evalOnce {
    crypto.verify(signature, bodyBytes(), header.generator, checkWeakPk = true) &&
    (header.version < Block.ProtoBlockVersion || transactionsMerkleTree().transactionsRoot == header.transactionsRoot) &&
    header.challengedHeader.forall { ch =>
      crypto.verify(
        ch.headerSignature,
        PBBlocks.protobuf(originalHeader()).toByteArray,
        ch.generator,
        checkWeakPk = true
      )
    }
  }

  def toOriginal: Block =
    header.challengedHeader match {
      case Some(ch) => copy(header = originalHeader(), signature = ch.headerSignature)
      case _        => this
    }

  override def toString: String =
    s"Block(${id()},${header.reference},${header.generator.toAddress}," +
      s"${header.timestamp},${header.featureVotes.mkString("[", ",", "]")}${if (header.rewardVote >= 0) s",${header.rewardVote}" else ""})"
}

object Block {
  def idFromHeader(h: BlockHeader, signature: ByteStr): ByteStr =
    if (h.version >= ProtoBlockVersion) protoHeaderHash(h)
    else signature

  def protoHeaderHash(h: BlockHeader): ByteStr = {
    require(h.version >= ProtoBlockVersion)
    ByteStr(crypto.fastHash(PBBlocks.protobuf(h).toByteArray))
  }

  def referenceLength(version: Byte): Int =
    if (version >= ProtoBlockVersion) DigestLength
    else SignatureLength

  def validateReferenceLength(length: Int): Boolean =
    length == DigestLength || length == SignatureLength

  def create(
      version: Byte,
      timestamp: Long,
      reference: ByteStr,
      baseTarget: Long,
      generationSignature: ByteStr,
      generator: PublicKey,
      featureVotes: Seq[Short],
      rewardVote: Long,
      transactionData: Seq[Transaction],
      stateHash: Option[ByteStr],
      challengedHeader: Option[ChallengedHeader]
  ): Block = {
    val transactionsRoot = mkTransactionsRoot(version, transactionData)
    Block(
      BlockHeader(
        version,
        timestamp,
        reference,
        baseTarget,
        generationSignature,
        generator,
        featureVotes,
        rewardVote,
        transactionsRoot,
        stateHash,
        challengedHeader
      ),
      ByteStr.empty,
      transactionData
    )
  }

  def create(base: Block, transactionData: Seq[Transaction], signature: ByteStr, stateHash: Option[ByteStr]): Block =
    base.copy(
      signature = signature,
      transactionData = transactionData,
      header = base.header.copy(transactionsRoot = mkTransactionsRoot(base.header.version, transactionData), stateHash = stateHash)
    )

  def buildAndSign(
      version: Byte,
      timestamp: Long,
      reference: ByteStr,
      baseTarget: Long,
      generationSignature: ByteStr,
      txs: Seq[Transaction],
      signer: KeyPair,
      featureVotes: Seq[Short],
      rewardVote: Long,
      stateHash: Option[ByteStr],
      challengedHeader: Option[ChallengedHeader]
  ): Either[GenericError, Block] =
    create(
      version,
      timestamp,
      reference,
      baseTarget,
      generationSignature,
      signer.publicKey,
      featureVotes,
      rewardVote,
      txs,
      stateHash,
      challengedHeader
    ).validate
      .map(_.sign(signer.privateKey))

  def parseBytes(bytes: Array[Byte]): Try[Block] =
    BlockSerializer
      .parseBytes(bytes)
      .flatMap(_.validateToTry)

  def genesis(genesisSettings: GenesisSettings, rideV6Activated: Boolean, txStateSnapshotActivated: Boolean): Either[ValidationError, Block] = {
    import cats.instances.either.*
    import cats.instances.list.*
    import cats.syntax.traverse.*

    for {
      txs <- genesisSettings.transactions.toList.map { gts =>
        for {
          address <- Address.fromString(gts.recipient)
          tx      <- GenesisTransaction.create(address, gts.amount, genesisSettings.timestamp)
        } yield tx
      }.sequence
      baseTarget = genesisSettings.initialBaseTarget
      timestamp  = genesisSettings.blockTimestamp
      block = create(
        GenesisBlockVersion,
        timestamp,
        GenesisReference,
        baseTarget,
        GenesisGenerationSignature,
        GenesisGenerator.publicKey,
        Seq(),
        -1L,
        txs,
        None,
        None
      )
      signedBlock = genesisSettings.signature match {
        case None             => block.sign(GenesisGenerator.privateKey)
        case Some(predefined) => block.copy(signature = predefined)
      }
      signedBlockWithStateHash = signedBlock.copy(header =
        signedBlock.header.copy(stateHash = Option.when(txStateSnapshotActivated)(TxStateSnapshotHashBuilder.createGenesisStateHash(txs)))
      )
      validBlock <- signedBlockWithStateHash.validateGenesis(genesisSettings, rideV6Activated)
    } yield validBlock
  }

  type BlockId                = ByteStr
  type TransactionsMerkleTree = Seq[Seq[Array[Byte]]]
  case class TransactionProof(id: ByteStr, transactionIndex: Int, digests: Seq[Array[Byte]])

  val MaxTransactionsPerBlockVer1Ver2: Int = 100
  val MaxTransactionsPerBlockVer3: Int     = 6000
  val MaxFeaturesInBlock: Int              = 64
  val BaseTargetLength: Int                = 8
  val GenerationSignatureLength: Int       = 32
  val GenerationVRFSignatureLength: Int    = 96
  val BlockIdLength: Int                   = SignatureLength
  val TransactionSizeLength                = 4
  val HitSourceLength                      = 32

  val GenesisReference: BlockId           = ByteStr(Array.fill(SignatureLength)(-1: Byte))
  val GenesisGenerator: KeyPair           = KeyPair(ByteStr.empty)
  val GenesisGenerationSignature: BlockId = ByteStr(new Array[Byte](crypto.DigestLength))

  val GenesisBlockVersion: Byte = 1
  val PlainBlockVersion: Byte   = 2
  val NgBlockVersion: Byte      = 3
  val RewardBlockVersion: Byte  = 4
  val ProtoBlockVersion: Byte   = 5

  // Merkle
  implicit class BlockTransactionsRootOps(private val block: Block) extends AnyVal {
    def transactionProof(transaction: Transaction): Option[TransactionProof] =
      block.transactionData.indexWhere(transaction.id() == _.id()) match {
        case -1  => None
        case idx => Some(TransactionProof(transaction.id(), idx, mkProofs(idx, block.transactionsMerkleTree()).reverse))
      }

    def verifyTransactionProof(transactionProof: TransactionProof): Boolean =
      block.transactionData
        .lift(transactionProof.transactionIndex)
        .filter(tx => tx.id() == transactionProof.id)
        .exists(tx =>
          verify(
            hash(PBTransactions.protobuf(tx).toByteArray),
            transactionProof.transactionIndex,
            transactionProof.digests.reverse,
            block.header.transactionsRoot.arr
          )
        )
  }
}

case class SignedBlockHeader(header: BlockHeader, signature: ByteStr) {
  val id: Coeval[ByteStr] = Coeval.evalOnce(Block.idFromHeader(header, signature))
}
