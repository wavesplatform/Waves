package com.wavesplatform.block

import java.nio.ByteBuffer

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import com.wavesplatform.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.crypto
import com.wavesplatform.crypto._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.serialization.Deser.ByteBufferOps
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
  private[block] val json: Coeval[JsObject] = Coeval.evalOnce {
    val featuresJson =
      if (version < Block.NgBlockVersion) JsObject.empty else Json.obj("features" -> JsArray(featureVotes.map(id => JsNumber(id.toInt)).toSeq))

    val rewardJson =
      if (version < Block.RewardBlockVersion) JsObject.empty else Json.obj("desiredReward" -> JsNumber(rewardVote))

    val generatorJson = Json.obj("generator" -> generator.stringRepr, "generatorPublicKey" -> generator.toString)

    Json.obj(
      "version"              -> version,
      "timestamp"            -> timestamp,
      "reference"            -> reference.toString,
      "base-target"          -> baseTarget,
      "generation-signature" -> generationSignature.toString
    ) ++ featuresJson ++ rewardJson ++ generatorJson
  }
}

object BlockHeader extends ScorexLogging {
  def json(header: BlockHeader, blockSize: Int, transactionCount: Int): JsObject =
    header.json() ++ Json.obj("blocksize" -> blockSize, "transactionCount" -> transactionCount)
}

case class Block private[block] (
    header: BlockHeader,
    signature: ByteStr,
    transactionData: Seq[Transaction]
) extends Signed {
  import Block._

  val uniqueId: ByteStr = signature
  val sender: PublicKey = header.generator

  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val consensusBytes        = writeConsensusBytes(header.baseTarget, header.generationSignature)
    val transactionsDataBytes = writeTransactionData(header.version, transactionData)

    val featureVotesBytes = header.version match {
      case v if v < NgBlockVersion => Array.empty[Byte]
      case _ =>
        val featuresBuf = ByteBuffer.allocate(Ints.BYTES + header.featureVotes.size * Shorts.BYTES)
        featuresBuf.putInt(header.featureVotes.size).asShortBuffer().put(header.featureVotes.toArray)
        featuresBuf.array
    }

    val rewardVoteBytes = header.version match {
      case v if v < RewardBlockVersion => Array.empty[Byte]
      case _                           => Longs.toByteArray(header.rewardVote)
    }

    Bytes.concat(
      Array(header.version),
      Longs.toByteArray(header.timestamp),
      header.reference.arr,
      Ints.toByteArray(consensusBytes.length),
      consensusBytes,
      Ints.toByteArray(transactionsDataBytes.length),
      transactionsDataBytes,
      featureVotesBytes,
      rewardVoteBytes,
      header.generator.arr,
      signature.arr
    )
  }

  val json: Coeval[JsObject] = Coeval.evalOnce {
    BlockHeader.json(header, bytes().length, transactionData.length) ++
      Json.obj("fee"          -> transactionData.map(_.assetFee).collect { case (Waves, feeAmt) => feeAmt }.sum) ++
      Json.obj("transactions" -> JsArray(transactionData.map(_.json())))
  }

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

  override val signatureValid: Coeval[Boolean] = Coeval.evalOnce {
    val publicKey = header.generator
    !crypto.isWeakPublicKey(publicKey) && crypto.verify(signature.arr, bytes().dropRight(SignatureLength), publicKey)
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
      .map(unsigned => unsigned.copy(signature = crypto.sign(signer, ByteStr(unsigned.bytes()))))

  def parseBytes(bytes: Array[Byte]): Try[Block] =
    Try {
      val buf = ByteBuffer.wrap(bytes).asReadOnlyBuffer()

      val version   = buf.get
      val timestamp = buf.getLong
      val reference = ByteStr(buf.getByteArray(SignatureLength))

      val cBytesLength        = buf.getInt
      val baseTarget          = Longs.fromByteArray(buf.getByteArray(Block.BaseTargetLength))
      val generationSignature = ByteStr(buf.getByteArray(cBytesLength - Longs.BYTES))

      buf.getInt()

      val transactionData = readTransactionData(version, buf)

      val featureVotes =
        if (version > Block.PlainBlockVersion) {
          val featureSize = buf.getInt
          buf.getShortArray(featureSize).toSet
        } else Set.empty[Short]

      val rewardVote = if (version >= Block.RewardBlockVersion) buf.getLong else -1L

      val generator = buf.getPublicKey
      val signature = ByteStr(buf.getByteArray(SignatureLength))

      val header = BlockHeader(version, timestamp, reference, baseTarget, generationSignature, generator, featureVotes, rewardVote)
      Block(header, signature, transactionData)
    }.flatMap(validate(_).asTry)
      .recoverWith {
        case t: Throwable =>
          log.error("Error when parsing block", t)
          Failure(t)
      }

  def genesis(genesisSettings: GenesisSettings): Either[ValidationError, Block] = {
    import com.wavesplatform.common.utils.EitherExt2
    import genesisSettings.initialBalance

    val genesisSigner = KeyPair(ByteStr.empty)

    val transactionGenesisData = genesisSettings.transactions.map { ts =>
      val acc = Address.fromString(ts.recipient).explicitGet()
      GenesisTransaction.create(acc, ts.amount, genesisSettings.timestamp).explicitGet()
    }

    val consensusGenesisData = NxtLikeConsensusBlockData(genesisSettings.initialBaseTarget, ByteStr(Array.fill(crypto.DigestSize)(0: Byte)))
    val txBytes              = writeTransactionData(GenesisBlockVersion, transactionGenesisData)
    val cBytes               = writeConsensusBytes(consensusGenesisData.baseTarget, consensusGenesisData.generationSignature)

    val reference = Array.fill(SignatureLength)(-1: Byte)

    val timestamp = genesisSettings.blockTimestamp
    val toSign: Array[Byte] =
      Bytes.concat(
        Array(GenesisBlockVersion),
        Longs.toByteArray(timestamp),
        reference,
        Ints.toByteArray(cBytes.length),
        cBytes,
        Ints.toByteArray(txBytes.length),
        txBytes,
        genesisSigner.publicKey.arr
      )

    val signature = genesisSettings.signature.fold(crypto.sign(genesisSigner, ByteStr(toSign)))(_.arr)

    for {
      // Verify signature
      _ <- Either.cond(crypto.verify(signature, ByteStr(toSign), genesisSigner.publicKey), (), GenericError("Passed genesis signature is not valid"))
      // Verify initial balance
      txsSum = transactionGenesisData.map(_.amount).reduce(Math.addExact(_: Long, _: Long))
      _ <- Either.cond(txsSum == initialBalance, (), GenericError(s"Initial balance $initialBalance did not match the distributions sum $txsSum"))
    } yield Block(
      BlockHeader(
        GenesisBlockVersion,
        timestamp,
        reference,
        consensusGenesisData.baseTarget,
        consensusGenesisData.generationSignature,
        genesisSigner,
        Set.empty,
        -1L
      ),
      signature,
      transactionGenesisData
    )
  }

  // format: off
  private def validate(b: Block): Either[GenericError, Block] =
    (for {
      _ <- Either.cond(b.header.reference.arr.length == SignatureLength, (), "Incorrect reference")
      _ <- Either.cond(b.header.generationSignature.arr.length == GeneratorSignatureLength, (), "Incorrect generationSignature")
      _ <- Either.cond(b.header.generator.length == KeyLength, (), "Incorrect signer")
      _ <- Either.cond(b.header.version > 2 || b.header.featureVotes.isEmpty,(),s"Block version ${b.header.version} could not contain feature votes")
      _ <- Either.cond(b.header.featureVotes.size <= MaxFeaturesInBlock, (), s"Block could not contain more than $MaxFeaturesInBlock feature votes")
    } yield b).left.map(GenericError(_))
  // format: on

  private def writeConsensusBytes(baseTarget: Long, generationSignature: ByteStr): Array[Byte] =
    Bytes.concat(
      Longs.toByteArray(baseTarget),
      generationSignature.arr
    )

  case class Fraction(dividend: Int, divider: Int) {
    def apply(l: Long): Long = l / divider * dividend
  }

  val CurrentBlockFeePart: Fraction = Fraction(2, 5)

  type BlockId = ByteStr

  val MaxTransactionsPerBlockVer1Ver2: Int = 100
  val MaxTransactionsPerBlockVer3: Int     = 6000
  val MaxFeaturesInBlock: Int              = 64
  val BaseTargetLength: Int                = 8
  val GeneratorSignatureLength: Int        = 32
  val BlockIdLength: Int                   = SignatureLength
  val TransactionSizeLength                = 4

  val GenesisBlockVersion: Byte = 1
  val PlainBlockVersion: Byte   = 2
  val NgBlockVersion: Byte      = 3
  val RewardBlockVersion: Byte  = 4
}
