package com.wavesplatform.block

import cats.Monoid
import com.wavesplatform.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.block.merkle.Merkle
import com.wavesplatform.block.serialization.BlockSerializer
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.crypto._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.settings.GenesisSettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction._
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Coeval
import play.api.libs.json._
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.hash.Digest32

import scala.util.{Failure, Try}

case class BlockHeader(
    version: Byte,
    timestamp: Long,
    reference: ByteStr,
    baseTarget: Long,
    generationSignature: ByteStr,
    generator: PublicKey,
    featureVotes: Seq[Short],
    rewardVote: Long,
    transactionsRoot: ByteStr
)

case class Block(
    header: BlockHeader,
    signature: ByteStr,
    transactionData: Seq[Transaction]
) {
  import Block._

  val uniqueId: ByteStr = signature
  val sender: PublicKey = header.generator

  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(BlockSerializer.toBytes(this))
  val json: Coeval[JsObject]     = Coeval.evalOnce(BlockSerializer.toJson(this))

  val blockScore: Coeval[BigInt] = Coeval.evalOnce((BigInt("18446744073709551616") / header.baseTarget).ensuring(_ > 0))

  val feesPortfolio: Coeval[Portfolio] = Coeval.evalOnce(Monoid[Portfolio].combineAll({
    val assetFees: Seq[(Asset, Long)] = transactionData.map(_.assetFee)
    assetFees
      .map { case (maybeAssetId, vol) => maybeAssetId -> vol }
      .groupBy(a => a._1)
      .mapValues((records: Seq[(Asset, Long)]) => records.map(_._2).sum)
  }.toList.map {
    case (assetId, feeVolume) =>
      assetId match {
        case Waves                  => Portfolio(feeVolume, LeaseBalance.empty, Map.empty)
        case asset @ IssuedAsset(_) => Portfolio(0L, LeaseBalance.empty, Map(asset -> feeVolume))
      }
  }))

  val prevBlockFeePart: Coeval[Portfolio] =
    Coeval.evalOnce(Monoid[Portfolio].combineAll(transactionData.map(tx => tx.feeDiff().minus(tx.feeDiff().multiply(CurrentBlockFeePart)))))

  private[block] val bytesWithoutSignature: Coeval[Array[Byte]] = Coeval.evalOnce {
    if (header.version < Block.ProtoBlockVersion) copy(signature = ByteStr.empty).bytes()
    else PBBlocks.protobuf(this).header.get.toByteArray
  }

  val signatureValid: Coeval[Boolean] = Coeval.evalOnce {
    val publicKey = header.generator
    !crypto.isWeakPublicKey(publicKey.arr) && crypto.verify(signature, ByteStr(bytesWithoutSignature()), publicKey)
  }

  protected val signedDescendants: Coeval[Seq[Signed]] = Coeval.evalOnce(transactionData.flatMap(_.cast[Signed]))

  private[block] val transactionsMerkleTree: Coeval[MerkleTree[Digest32]] = Coeval.evalOnce(Merkle.mkMerkleTree(transactionData))

  val transactionsRootValid: Coeval[Boolean] = Coeval.evalOnce {
    require(header.version >= Block.ProtoBlockVersion, "Block's version should be >= 5 to retrieve transactionsRoot")
    (transactionsMerkleTree().rootHash untag Digest32) sameElements header.transactionsRoot.arr
  }

  override def toString: String =
    s"Block($signature -> ${header.reference.trim}, " +
      s"txs=${transactionData.size}, features=${header.featureVotes}${if (header.rewardVote >= 0) s", rewardVote=${header.rewardVote}" else ""})"
}

object Block extends ScorexLogging {

  def create(
      version: Byte,
      timestamp: Long,
      reference: ByteStr,
      baseTarget: Long,
      generationSignature: ByteStr,
      generator: PublicKey,
      featureVotes: Seq[Short],
      rewardVote: Long,
      transactionData: Seq[Transaction]
  ): Block = {
    val transactionsRoot = mkTransactionsRoot(version, transactionData)
    Block(
      BlockHeader(version, timestamp, reference, baseTarget, generationSignature, generator, featureVotes, rewardVote, transactionsRoot),
      ByteStr.empty,
      transactionData
    )
  }

  def create(base: Block, transactionData: Seq[Transaction], signature: ByteStr): Block =
    base.copy(
      signature = signature,
      transactionData = transactionData,
      header = base.header.copy(transactionsRoot = mkTransactionsRoot(base.header.version, transactionData))
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
      rewardVote: Long
  ): Either[GenericError, Block] =
    create(version, timestamp, reference, baseTarget, generationSignature, signer, featureVotes, rewardVote, txs).validate.map(_.sign(signer))

  def parseBytes(bytes: Array[Byte]): Try[Block] =
    BlockSerializer
      .parseBytes(bytes)
      .flatMap(_.validateToTry)
      .recoverWith {
        case t: Throwable =>
          log.error("Error when parsing block", t)
          Failure(t)
      }

  def genesis(genesisSettings: GenesisSettings): Either[ValidationError, Block] = {
    import cats.instances.either._
    import cats.instances.list._
    import cats.syntax.traverse._

    for {
      txs <- genesisSettings.transactions.toList.map { gts =>
        for {
          address <- Address.fromString(gts.recipient)
          tx      <- GenesisTransaction.create(address, gts.amount, genesisSettings.timestamp)
        } yield tx
      }.sequence
      generator  = KeyPair(ByteStr.empty)
      baseTarget = genesisSettings.initialBaseTarget
      genSig     = ByteStr(Array.fill(crypto.DigestLength)(0: Byte))
      reference  = Array.fill(SignatureLength)(-1: Byte)
      timestamp  = genesisSettings.blockTimestamp
      block      = create(GenesisBlockVersion, timestamp, reference, baseTarget, genSig, generator, Seq(), -1L, txs)
      signedBlock = genesisSettings.signature match {
        case None             => block.sign(generator)
        case Some(predefined) => block.copy(signature = predefined)
      }
      validBlock <- signedBlock.validateGenesis(genesisSettings)
    } yield validBlock
  }

  private def mkTransactionsRoot(version: Byte, transactionData: Seq[Transaction]): ByteStr =
    if (version < ProtoBlockVersion) ByteStr.empty else ByteStr(Merkle.calcTransactionRoot(transactionData))

  case class Fraction(dividend: Int, divider: Int) {
    def apply(l: Long): Long = l / divider * dividend
  }

  val CurrentBlockFeePart: Fraction = Fraction(2, 5)

  type BlockId = ByteStr

  val MaxTransactionsPerBlockVer1Ver2: Int = 100
  val MaxTransactionsPerBlockVer3: Int     = 6000
  val MaxFeaturesInBlock: Int              = 64
  val BaseTargetLength: Int                = 8
  val GenerationSignatureLength: Int       = 32
  val GenerationVRFSignatureLength: Int    = 96
  val BlockIdLength: Int                   = SignatureLength
  val TransactionSizeLength                = 4
  val HitSourceLength                      = 32

  val GenesisBlockVersion: Byte = 1
  val PlainBlockVersion: Byte   = 2
  val NgBlockVersion: Byte      = 3
  val RewardBlockVersion: Byte  = 4
  val ProtoBlockVersion: Byte   = 5
}

case class SignedBlockHeader(header: BlockHeader, signature: ByteStr)
