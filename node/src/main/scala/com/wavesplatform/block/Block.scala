package com.wavesplatform.block

import java.nio.ByteBuffer

import cats._
import com.google.common.io.ByteStreams
import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.block.fields.{FeaturesBlockField, RewardBlockField}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.nxt.{NxtConsensusBlockField, NxtLikeConsensusBlockData}
import com.wavesplatform.crypto
import com.wavesplatform.crypto._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.GenesisSettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction._
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.{Failure, Try}

case class BlockHeader private[block] (
    signature: ByteStr, // todo: (NODE-1927) ???
    version: Byte,
    timestamp: Long,
    reference: ByteStr,
    baseTarget: Long,
    generationSignature: ByteStr,
    generator: PublicKey,
    featureVotes: Set[Short],
    rewardVote: Long,
    transactionCount: Int
) {
  private[block] val versionField: ByteBlockField                = ByteBlockField("version", version)
  private[block] val timestampField: LongBlockField              = LongBlockField("timestamp", timestamp)
  private[block] val referenceField: ByteStrBlockField           = ByteStrBlockField("reference", reference.arr)
  private[block] val baseTargetField: LongBlockField             = LongBlockField("base-target", baseTarget)
  private[block] val generationSignatureField: ByteStrBlockField = ByteStrBlockField("generation-signature", generationSignature)
  private[block] val supportedFeaturesField: FeaturesBlockField  = FeaturesBlockField(version, featureVotes)
  private[block] val rewardVoteField: RewardBlockField           = RewardBlockField(version, rewardVote)
  private[block] val generatorField: GeneratorBlockField         = GeneratorBlockField(generator)
  private[block] val signatureField: ByteStrBlockField           = ByteStrBlockField("signature", signature) // todo: (NODE-1927) ???

  private[block] val headerJson: Coeval[JsObject] = Coeval.evalOnce(
    versionField.json() ++
      timestampField.json() ++
      referenceField.json() ++
      baseTargetField.json() ++
      generationSignatureField.json() ++
      supportedFeaturesField.json() ++
      rewardVoteField.json() ++
      generatorField.json() ++
      signatureField.json()
  )
}

object BlockHeader extends ScorexLogging {
  def writeHeaderOnly(bh: BlockHeader): Array[Byte] = {
    val ndo = ByteStreams.newDataOutput()

    ndo.writeByte(bh.version)
    ndo.writeLong(bh.timestamp)
    ndo.write(bh.reference.arr)
    ndo.writeLong(bh.baseTarget)
    ndo.write(bh.generationSignature.arr)

    if (bh.version == Block.GenesisBlockVersion | bh.version == Block.PlainBlockVersion)
      ndo.writeByte(bh.transactionCount)
    else
      ndo.writeInt(bh.transactionCount)

    ndo.writeInt(bh.featureVotes.size)
    bh.featureVotes.foreach(s => ndo.writeShort(s))

    if (bh.version > Block.NgBlockVersion)
      ndo.writeLong(bh.rewardVote)

    ndo.write(bh.generator.arr)
    ndo.write(bh.signature.arr)

    ndo.toByteArray
  }

  def readHeaderOnly(bytes: Array[Byte]): BlockHeader = {
    val ndi = ByteStreams.newDataInput(bytes)

    val version   = ndi.readByte()
    val timestamp = ndi.readLong()

    val reference = new Array[Byte](SignatureLength)
    ndi.readFully(reference)

    val baseTarget = ndi.readLong()

    val generationSignature = new Array[Byte](Block.GeneratorSignatureLength)
    ndi.readFully(generationSignature)

    val transactionCount = {
      if (version == Block.GenesisBlockVersion || version == Block.PlainBlockVersion) ndi.readByte()
      else ndi.readInt()
    }
    val featureVotesCount = ndi.readInt()
    val featureVotes      = List.fill(featureVotesCount)(ndi.readShort()).toSet

    val rewardVote = if (version > Block.NgBlockVersion) ndi.readLong() else -1L

    val generator = new Array[Byte](KeyLength)
    ndi.readFully(generator)

    val signature = new Array[Byte](SignatureLength)
    ndi.readFully(signature)

    BlockHeader(
      ByteStr(signature),
      version,
      timestamp,
      ByteStr(reference),
      baseTarget,
      ByteStr(generationSignature),
      PublicKey(generator),
      featureVotes,
      rewardVote,
      transactionCount
    )
  }

  def parseBytes(bytes: Array[Byte]): Try[(BlockHeader, Array[Byte])] =
    Try {

      val version = bytes.head

      var position = 1

      val timestamp = Longs.fromByteArray(bytes.slice(position, position + 8))
      position += 8

      val reference = ByteStr(bytes.slice(position, position + SignatureLength))
      position += SignatureLength

      val cBytesLength = Ints.fromByteArray(bytes.slice(position, position + 4))
      position += 4
      val cBytes              = bytes.slice(position, position + cBytesLength)
      val baseTarget          = Longs.fromByteArray(cBytes.take(Block.BaseTargetLength))
      val generationSignature = ByteStr(cBytes.takeRight(Block.GeneratorSignatureLength))
      position += cBytesLength

      val tBytesLength = Ints.fromByteArray(bytes.slice(position, position + 4))
      position += 4
      val tBytes = bytes.slice(position, position + tBytesLength)

      val transactionCount = version match {
        case Block.GenesisBlockVersion | Block.PlainBlockVersion => tBytes.head
        case Block.NgBlockVersion | Block.RewardBlockVersion     => ByteBuffer.wrap(tBytes, 0, 4).getInt()
      }

      position += tBytesLength

      var featureVotes = Set.empty[Short]

      if (version > Block.PlainBlockVersion) {
        val featuresCount = Ints.fromByteArray(bytes.slice(position, position + 4))
        position += 4

        val buffer = ByteBuffer.wrap(bytes.slice(position, position + featuresCount * 2)).asShortBuffer
        val arr    = new Array[Short](featuresCount)
        buffer.get(arr)
        position += featuresCount * 2
        featureVotes = arr.toSet
      }

      var rewardVote = -1L

      if (version >= Block.RewardBlockVersion) {
        rewardVote = Longs.fromByteArray(bytes.slice(position, position + 8))
        position += 8
      }

      val generator = PublicKey(bytes.slice(position, position + KeyLength))
      position += KeyLength

      val signature = ByteStr(bytes.slice(position, position + SignatureLength))
      position += SignatureLength

      val blockHeader =
        BlockHeader(signature, version, timestamp, reference, baseTarget, generationSignature, generator, featureVotes, rewardVote, transactionCount)
      (blockHeader, tBytes)
    }.recoverWith {
      case t: Throwable =>
        log.error("Error when parsing block", t)
        Failure(t)
    }

  def json(bh: BlockHeader, blockSize: Int): JsObject =
    bh.headerJson() ++
      Json.obj(
        "blocksize"        -> blockSize,
        "transactionCount" -> bh.transactionCount
      )
}

case class Block private[block] (
    header: BlockHeader,
    signature: ByteStr,
    transactionData: Seq[Transaction]
) extends Signed {
  import Block._

  val uniqueId: ByteStr = signature
  val sender: PublicKey = header.generator

  private val transactionField = TransactionsBlockField(header.version.toInt, transactionData)

  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val h           = header
    val txBytesSize = transactionField.bytes().length
    val txBytes     = Bytes.ensureCapacity(Ints.toByteArray(txBytesSize), 4, 0) ++ transactionField.bytes()

    val cBytesSize = h.baseTargetField.bytes().length + h.generationSignatureField.bytes().length
    val cBytes     = Bytes.ensureCapacity(Ints.toByteArray(cBytesSize), 4, 0) ++ h.baseTargetField.bytes() ++ h.generationSignatureField.bytes()

    h.versionField.bytes() ++
      h.timestampField.bytes() ++
      h.referenceField.bytes() ++
      cBytes ++
      txBytes ++
      h.supportedFeaturesField.bytes() ++
      h.rewardVoteField.bytes() ++
      h.generatorField.bytes() ++
      h.signatureField.bytes()
  }

  val json: Coeval[JsObject] = Coeval.evalOnce(
    BlockHeader.json(header, bytes().length) ++
      Json.obj("fee" -> transactionData.map(_.assetFee).collect { case (Waves, feeAmt) => feeAmt }.sum) ++
      transactionField.json()
  )

  val bytesWithoutSignature: Coeval[Array[Byte]] = Coeval.evalOnce(bytes().dropRight(SignatureLength))

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
    !crypto.isWeakPublicKey(publicKey.arr) && crypto.verify(signature, ByteStr(bytesWithoutSignature()), publicKey)
  }

  protected override val signedDescendants: Coeval[Seq[Signed]] = Coeval.evalOnce(transactionData.flatMap(_.cast[Signed]))

  override def toString: String =
    s"Block($signature -> ${header.reference.trim}, " +
      s"txs=${transactionData.size}, features=${header.featureVotes}${if (header.rewardVote >= 0) s", rewardVote=${header.rewardVote}" else ""})"
}

object Block extends ScorexLogging {
  case class Fraction(dividend: Int, divider: Int) {
    def apply(l: Long): Long = l / divider * dividend
  }

  val CurrentBlockFeePart: Fraction = Fraction(2, 5)

  type BlockIds = Seq[ByteStr]
  type BlockId  = ByteStr

  val MaxTransactionsPerBlockVer1Ver2: Int = 100
  val MaxTransactionsPerBlockVer3: Int     = 6000
  val MaxFeaturesInBlock: Int              = 64
  val BaseTargetLength: Int                = 8
  val GeneratorSignatureLength: Int        = 32

  val BlockIdLength: Int = SignatureLength

  val TransactionSizeLength = 4

  def transParseBytes(version: Int, bytes: Array[Byte]): Try[Seq[Transaction]] = Try {
    if (bytes.isEmpty) {
      Seq.empty
    } else {
      val v: (Array[Byte], Int) = version match {
        case Block.GenesisBlockVersion | Block.PlainBlockVersion => (bytes.tail, bytes.head) //  127 max, won't work properly if greater
        case Block.NgBlockVersion | Block.RewardBlockVersion =>
          val size = ByteBuffer.wrap(bytes, 0, 4).getInt()
          (bytes.drop(4), size)
        case _ => throw new NotImplementedError(s"Unknown block version $version")
      }

      val txs = Seq.newBuilder[Transaction]
      (1 to v._2).foldLeft(0) {
        case (pos, _) =>
          val transactionLengthBytes = v._1.slice(pos, pos + TransactionSizeLength)
          val transactionLength      = Ints.fromByteArray(transactionLengthBytes)
          val transactionBytes       = v._1.slice(pos + TransactionSizeLength, pos + TransactionSizeLength + transactionLength)
          txs += TransactionParsers.parseBytes(transactionBytes).get
          pos + TransactionSizeLength + transactionLength
      }

      txs.result()
    }
  }

  def parseBytes(bytes: Array[Byte]): Try[Block] =
    for {
      (blockHeader, transactionBytes) <- BlockHeader.parseBytes(bytes)
      transactionsData                <- transParseBytes(blockHeader.version, transactionBytes)
      block                           <- build(blockHeader, transactionsData).left.map(ve => new IllegalArgumentException(ve.toString)).toTry
    } yield block

  def fromHeaderAndTransactions(h: BlockHeader, txs: Seq[Transaction]): Either[GenericError, Block] = build(h, txs)

  private def build(header: BlockHeader, transactionData: Seq[Transaction]): Either[GenericError, Block] = {
    (for {
      _ <- Either.cond(header.reference.arr.length == SignatureLength, (), "Incorrect reference")
      _ <- Either.cond(header.generationSignature.arr.length == GeneratorSignatureLength, (), "Incorrect consensusData.generationSignature")
      _ <- Either.cond(header.generator.arr.length == KeyLength, (), "Incorrect signer")
      _ <- Either.cond(header.version > 2 || header.featureVotes.isEmpty, (), s"Block version ${header.version} could not contain feature votes")
      _ <- Either.cond(header.featureVotes.size <= MaxFeaturesInBlock, (), s"Block could not contain more than $MaxFeaturesInBlock feature votes")
    } yield Block(header, header.signature, transactionData)).left.map(GenericError(_))
  }

  // formatter: off
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
    build(
      BlockHeader(ByteStr.empty, version, timestamp, reference, baseTarget, generationSignature, signer, featureVotes, rewardVote, txs.length),
      txs
    ).map { unsigned =>
      val signature = crypto.sign(signer, ByteStr(unsigned.bytes()))
      unsigned.copy(signature = signature, header = unsigned.header.copy(signature = signature))
    }
  // formatter: on

  def genesisTransactions(gs: GenesisSettings): Seq[GenesisTransaction] = {
    gs.transactions.map { ts =>
      val acc = Address.fromString(ts.recipient).explicitGet()
      GenesisTransaction.create(acc, ts.amount, gs.timestamp).explicitGet()
    }
  }

  def genesis(genesisSettings: GenesisSettings): Either[ValidationError, Block] = {
    val genesisSigner = KeyPair(ByteStr.empty)

    val transactionGenesisData      = genesisTransactions(genesisSettings)
    val transactionGenesisDataField = TransactionsBlockFieldVersion1or2(transactionGenesisData)
    val consensusGenesisData        = NxtLikeConsensusBlockData(genesisSettings.initialBaseTarget, ByteStr(Array.fill(crypto.DigestSize)(0: Byte)))
    val consensusGenesisDataField   = NxtConsensusBlockField(consensusGenesisData)
    val txBytesSize                 = transactionGenesisDataField.bytes().length
    val txBytes                     = Bytes.ensureCapacity(Ints.toByteArray(txBytesSize), 4, 0) ++ transactionGenesisDataField.bytes()
    val cBytesSize                  = consensusGenesisDataField.bytes().length
    val cBytes                      = Bytes.ensureCapacity(Ints.toByteArray(cBytesSize), 4, 0) ++ consensusGenesisDataField.bytes()

    val reference = Array.fill(SignatureLength)(-1: Byte)

    val timestamp = genesisSettings.blockTimestamp
    val toSign: Array[Byte] = Array(GenesisBlockVersion) ++
      Bytes.ensureCapacity(Longs.toByteArray(timestamp), 8, 0) ++
      reference ++
      cBytes ++
      txBytes ++
      genesisSigner.publicKey.arr

    val signature = genesisSettings.signature.fold(crypto.sign(genesisSigner, ByteStr(toSign)))(_.arr)

    for {
      // Verify signature
      _ <- Either.cond(crypto.verify(signature, ByteStr(toSign), genesisSigner.publicKey), (), GenericError("Passed genesis signature is not valid"))
      // Verify initial balance
      genesisTransactionsSum = transactionGenesisData.map(_.amount).reduce(Math.addExact(_: Long, _: Long))
      _ <- Either.cond(
        genesisTransactionsSum == genesisSettings.initialBalance,
        (),
        GenericError(s"Initial balance ${genesisSettings.initialBalance} did not match the distributions sum $genesisTransactionsSum")
      )
    } yield Block(
      BlockHeader(
        signature,
        GenesisBlockVersion,
        timestamp,
        reference,
        consensusGenesisData.baseTarget,
        consensusGenesisData.generationSignature,
        genesisSigner,
        Set.empty,
        -1L,
        transactionGenesisData.length
      ),
      signature,
      transactionGenesisData
    )
  }

  val GenesisBlockVersion: Byte = 1
  val PlainBlockVersion: Byte   = 2
  val NgBlockVersion: Byte      = 3
  val RewardBlockVersion: Byte  = 4
}
