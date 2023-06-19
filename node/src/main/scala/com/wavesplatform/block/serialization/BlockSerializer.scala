package com.wavesplatform.block.serialization

import java.nio.ByteBuffer
import com.google.common.io.ByteStreams.newDataOutput
import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import com.wavesplatform.account.PublicKey
import com.wavesplatform.block.Block.{NgBlockVersion, ProtoBlockVersion, RewardBlockVersion}
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.serialization.ByteBufferOps
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Transaction
import play.api.libs.json.{JsArray, JsNumber, JsObject, Json}

import scala.util.Try

object BlockHeaderSerializer {
  def toBytes(header: BlockHeader): Array[Byte] =
    if (header.version >= Block.ProtoBlockVersion) {
      PBUtils.encodeDeterministic(PBBlocks.protobuf(header))
    } else {
      val ndo = newDataOutput()

      ndo.writeByte(header.version)
      ndo.write(header.reference.arr)
      ndo.writeLong(header.baseTarget)
      ndo.write(header.generationSignature.arr)
      ndo.writeLong(header.timestamp)
      ndo.write(header.generator.arr)

      ndo.writeInt(header.featureVotes.size)
      header.featureVotes.foreach(s => ndo.writeShort(s))

      if (header.version > Block.NgBlockVersion)
        ndo.writeLong(header.rewardVote)

      ndo.toByteArray
    }

  def toJson(blockHeader: BlockHeader): JsObject = {
    def createFeaturesJson(featureVotes: Seq[Short]): JsObject =
      if (blockHeader.version < NgBlockVersion) JsObject.empty
      else Json.obj("features" -> JsArray(featureVotes.map(id => JsNumber(id.toInt))))

    def createGeneratorJson(generator: PublicKey): JsObject =
      Json.obj("generator" -> generator.toAddress.toString, "generatorPublicKey" -> generator)

    def createRewardVoteJson(rewardVote: Long): JsObject =
      if (blockHeader.version < RewardBlockVersion) JsObject.empty
      else Json.obj("desiredReward" -> JsNumber(rewardVote))

    def createStateHashJson(stateHash: Option[ByteStr]): JsObject =
      stateHash match {
        case Some(sh) => Json.obj("stateHash" -> sh.toString)
        case None     => JsObject.empty
      }

    val consensusJson =
      Json.obj(
        "nxt-consensus" -> Json.obj(
          "base-target"          -> blockHeader.baseTarget,
          "generation-signature" -> blockHeader.generationSignature.toString
        )
      ) ++ (if (blockHeader.version >= ProtoBlockVersion)
              Json.obj("transactionsRoot" -> blockHeader.transactionsRoot.toString, "id" -> Block.protoHeaderHash(blockHeader).toString)
            else Json.obj())

    val featuresJson  = createFeaturesJson(blockHeader.featureVotes)
    val rewardJson    = createRewardVoteJson(blockHeader.rewardVote)
    val generatorJson = createGeneratorJson(blockHeader.generator)
    val stateHashJson = createStateHashJson(blockHeader.stateHash)

    val challengedHeaderJson =
      blockHeader.challengedHeader match {
        case Some(ch) =>
          Json.obj(
            "challengedHeader" -> (Json.obj(
              "headerSignature" -> ch.headerSignature.toString
            ) ++ createFeaturesJson(ch.featureVotes) ++ createGeneratorJson(ch.generator) ++ createRewardVoteJson(
              ch.rewardVote
            ) ++ createStateHashJson(ch.stateHash))
          )
        case None => JsObject.empty
      }

    Json.obj(
      "version"   -> blockHeader.version,
      "timestamp" -> blockHeader.timestamp,
      "reference" -> blockHeader.reference.toString
    ) ++ consensusJson ++ featuresJson ++ rewardJson ++ generatorJson ++ stateHashJson ++ challengedHeaderJson
  }

  def toJson(header: BlockHeader, blockSize: Int, transactionCount: Int, signature: ByteStr): JsObject =
    toJson(header) ++ Json.obj(
      "signature"        -> signature.toString,
      "blocksize"        -> blockSize,
      "transactionCount" -> transactionCount
    ) ++ (if (header.version < ProtoBlockVersion) Json.obj("id" -> signature.toString) else Json.obj())
}

object BlockSerializer {
  def toBytes(block: Block): Array[Byte] =
    if (block.header.version >= ProtoBlockVersion)
      PBUtils.encodeDeterministic(PBBlocks.protobuf(block))
    else
      Bytes.concat(
        mkPrefixBytes(block.header),
        mkTxsDataBytes(block.header, block.transactionData),
        mkSuffixBytes(block.header, block.signature)
      )

  def mkPrefixBytes(header: BlockHeader): Array[Byte] = {
    val consensusBytes = writeConsensusBytes(header.baseTarget, header.generationSignature)
    Bytes.concat(
      Array(header.version),
      Longs.toByteArray(header.timestamp),
      header.reference.arr,
      Ints.toByteArray(consensusBytes.length),
      consensusBytes
    )
  }

  def mkTxsDataBytes(header: BlockHeader, transactions: Seq[Transaction]): Array[Byte] = {
    val transactionsDataBytes = writeTransactionData(header.version, transactions)
    Bytes.concat(
      Ints.toByteArray(transactionsDataBytes.length),
      transactionsDataBytes
    )
  }

  def mkSuffixBytes(header: BlockHeader, signature: ByteStr): Array[Byte] = {
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
      featureVotesBytes,
      rewardVoteBytes,
      header.generator.arr,
      signature.arr
    )
  }

  def parseBytes(bytes: Array[Byte]): Try[Block] =
    Try {
      val buf = ByteBuffer.wrap(bytes).asReadOnlyBuffer()

      val Prefix(version, timestamp, reference, baseTarget, generationSignature)   = parsePrefix(buf)
      val transactionData                                                          = parseTxs(buf, version)
      val Suffix(generator, featureVotes, rewardVote, transactionsRoot, signature) = parseSuffix(buf, version)

      val header =
        BlockHeader(version, timestamp, reference, baseTarget, generationSignature, generator, featureVotes, rewardVote, transactionsRoot, None, None)

      Block(header, signature, transactionData)
    }

  def parsePrefix(buf: ByteBuffer): Prefix = {
    val version   = buf.getByte
    val timestamp = buf.getLong
    val reference = ByteStr(buf.getByteArray(SignatureLength))

    val consensusBytesLength = buf.getInt
    val baseTarget           = Longs.fromByteArray(buf.getByteArray(Block.BaseTargetLength))
    val generationSignature  = ByteStr(buf.getByteArray(consensusBytesLength - Longs.BYTES))
    Prefix(version, timestamp, reference, baseTarget, generationSignature)
  }

  def parseTxs(buf: ByteBuffer, version: Byte): Seq[Transaction] = {
    buf.getInt // transactions data size
    readTransactionData(version, buf)
  }

  def parseSuffix(buf: ByteBuffer, version: Byte): Suffix = {
    val featureVotes     = if (version > Block.PlainBlockVersion) buf.getShortArray(buf.getInt).toSeq else Seq.empty[Short]
    val rewardVote       = if (version > Block.NgBlockVersion) buf.getLong else -1L
    val generator        = buf.getPublicKey
    val transactionsRoot = ByteStr.empty
    val signature        = ByteStr(buf.getByteArray(SignatureLength))
    Suffix(generator, featureVotes, rewardVote, transactionsRoot, signature)
  }

  case class Prefix(
      version: Byte,
      timestamp: Long,
      reference: ByteStr,
      baseTarget: Long,
      generationSignature: ByteStr
  )

  case class Suffix(
      generator: PublicKey,
      featureVotes: Seq[Short],
      rewardVote: Long,
      transactionsRoot: ByteStr,
      signature: ByteStr
  )

  def transactionField(transactions: Seq[Transaction]): JsObject = Json.obj(
    "fee"          -> transactions.map(_.assetFee).collect { case (Waves, feeAmt) => feeAmt }.sum,
    "transactions" -> JsArray(transactions.map(_.json()))
  )

  def toJson(block: Block): JsObject =
    BlockHeaderSerializer.toJson(block.header, block.bytes().length, block.transactionData.length, block.signature) ++
      transactionField(block.transactionData)
}
