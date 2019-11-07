package com.wavesplatform.block

import cats.Monoid
import com.wavesplatform.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.block.serialization.{BlockHeaderSerializer, BlockSerializer}
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

import scala.util.{Failure, Try}

case class BlockHeader(
    version: Byte,
    timestamp: Long,
    reference: ByteStr,
    baseTarget: Long,
    generationSignature: ByteStr,
    generator: PublicKey,
    featureVotes: Set[Short],
    rewardVote: Long
) {
  private[block] val json: Coeval[JsObject] = Coeval.evalOnce(BlockHeaderSerializer.toJson(this))
}

case class Block private[block] (
    header: BlockHeader,
    signature: ByteStr,
    transactionData: Seq[Transaction]
) extends Signed {
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

  private val bytesToSign: Coeval[Array[Byte]] = Coeval.evalOnce {
    if (header.version < ProtoBlockVersion) bytes().dropRight(SignatureLength)
    else PBBlocks.protobuf(this.copy(signature = ByteStr.empty)).toByteArray
    // else PBBlocks.protobuf(this).header.get.toByteArray // todo: (NODE-1927) only header when merkle proofs will be added
  }

  override val signatureValid: Coeval[Boolean] = Coeval.evalOnce {
    val publicKey = header.generator
    !crypto.isWeakPublicKey(publicKey) && crypto.verify(signature.arr, bytesToSign(), publicKey)
  }

  protected override val signedDescendants: Coeval[Seq[Signed]] = Coeval.evalOnce(transactionData.flatMap(_.cast[Signed]))

  override def toString: String =
    s"Block($signature -> ${header.reference.trim}, " +
      s"txs=${transactionData.size}, features=${header.featureVotes}${if (header.rewardVote >= 0) s", rewardVote=${header.rewardVote}" else ""})"
}

object Block extends ScorexLogging {

  def build(
      version: Byte,
      timestamp: Long,
      reference: ByteStr,
      baseTarget: Long,
      generationSignature: ByteStr,
      txs: Seq[Transaction],
      generator: PublicKey,
      signature: ByteStr,
      featureVotes: Set[Short],
      rewardVote: Long
  ): Either[GenericError, Block] =
    validate(
      Block(BlockHeader(version, timestamp, reference, baseTarget, generationSignature, generator, featureVotes, rewardVote), signature, txs)
    )

  def buildAndSign(
      version: Byte,
      timestamp: Long,
      reference: ByteStr,
      baseTarget: Long,
      generationSignature: ByteStr,
      txs: Seq[Transaction],
      signer: KeyPair,
      featureVotes: Set[Short],
      rewardVote: Long
  ): Either[GenericError, Block] =
    build(version, timestamp, reference, baseTarget, generationSignature, txs, signer, ByteStr.empty, featureVotes, rewardVote)
      .map(unsigned => unsigned.copy(signature = crypto.sign(signer, unsigned.bytesToSign())))

  def parseBytes(bytes: Array[Byte]): Try[Block] =
    BlockSerializer
      .parseBytes(bytes)
      .flatMap(validate(_).asTry)
      .recoverWith {
        case t: Throwable =>
          log.error("Error when parsing block", t)
          Failure(t)
      }

  def genesis(genesisSettings: GenesisSettings): Either[ValidationError, Block] = {
    import cats.instances.either._
    import cats.instances.list._
    import cats.syntax.traverse._

    // format: off
    def validateGenesis(block: Block): Either[ValidationError, Block] =
      for {
        // Common validation
        _ <- validate(block)
        // Verify signature
        _ <- Either.cond(crypto.verify(block.signature, block.bytesToSign(), block.header.generator), (), GenericError("Passed genesis signature is not valid"))
        // Verify initial balance
        txsSum = block.transactionData.collect { case tx: GenesisTransaction => tx.amount }.reduce(Math.addExact(_: Long, _: Long))
        _ <- Either.cond(txsSum == genesisSettings.initialBalance, (), GenericError(s"Initial balance ${genesisSettings.initialBalance} did not match the distributions sum $txsSum"))
      } yield block
    // format: on

    for {
      txs <- genesisSettings.transactions.toList.map { gts =>
        for {
          address <- Address.fromString(gts.recipient)
          tx      <- GenesisTransaction.create(address, gts.amount, genesisSettings.timestamp)
        } yield tx
      }.sequence
      generator  = KeyPair(ByteStr.empty)
      baseTarget = genesisSettings.initialBaseTarget
      genSig     = ByteStr(Array.fill(crypto.DigestSize)(0: Byte))
      reference  = Array.fill(SignatureLength)(-1: Byte)
      timestamp  = genesisSettings.blockTimestamp
      block <- build(GenesisBlockVersion, timestamp, reference, baseTarget, genSig, txs, generator, ByteStr.empty, Set(), -1L)
      signedBlock = genesisSettings.signature match {
        case None             => block.copy(signature = crypto.sign(generator, ByteStr(block.bytesToSign())))
        case Some(predefined) => block.copy(signature = predefined)
      }
      validBlock <- validateGenesis(signedBlock)
    } yield validBlock
  }

  // format: off
  private def validate(b: Block): Either[GenericError, Block] =
    (for {
      _ <- Either.cond(b.header.reference.arr.length == SignatureLength, (), "Incorrect reference")
      genSigLength = if (b.header.version < ProtoBlockVersion) GenerationSignatureLength else GenerationVRFSignatureLength
      _ <- Either.cond(b.header.generationSignature.arr.length == genSigLength, (), "Incorrect generationSignature")
      _ <- Either.cond(b.header.generator.length == KeyLength, (), "Incorrect signer")
      _ <- Either.cond(b.header.version > 2 || b.header.featureVotes.isEmpty,(),s"Block version ${b.header.version} could not contain feature votes")
      _ <- Either.cond(b.header.featureVotes.size <= MaxFeaturesInBlock, (), s"Block could not contain more than $MaxFeaturesInBlock feature votes")
    } yield b).left.map(GenericError(_))
  // format: on

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
  val GenerationInputLength                = 32

  val GenesisBlockVersion: Byte = 1
  val PlainBlockVersion: Byte   = 2
  val NgBlockVersion: Byte      = 3
  val RewardBlockVersion: Byte  = 4
  val ProtoBlockVersion: Byte   = 5 // todo: (NODE-1927) relevant name
}
