package com.wavesplatform.block.serialization

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import com.wavesplatform.block.Block.{NgBlockVersion, ProtoBlockVersion, RewardBlockVersion}
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.serialization.ByteBufferOps
import com.wavesplatform.transaction.Asset.Waves
import play.api.libs.json.{JsArray, JsNumber, JsObject, Json}

import scala.util.Try

object BlockHeaderSerializer {
  def toJson(blockHeader: BlockHeader): JsObject = {
    val consensusJson =
      if (blockHeader.version < ProtoBlockVersion)
        Json.obj(
          "nxt-consensus" -> Json.obj(
            "base-target"          -> blockHeader.baseTarget,
            "generation-signature" -> blockHeader.generationSignature.toString
          )
        )
      else
        Json.obj(
          "baseTarget"          -> blockHeader.baseTarget,
          "generationSignature" -> blockHeader.generationSignature.toString,
          "transactionsRoot"    -> blockHeader.transactionsRoot.toString
        )

    val featuresJson =
      if (blockHeader.version < NgBlockVersion) JsObject.empty
      else Json.obj("features" -> JsArray(blockHeader.featureVotes.map(id => JsNumber(id.toInt))))

    val rewardJson =
      if (blockHeader.version < RewardBlockVersion) JsObject.empty
      else Json.obj("desiredReward" -> JsNumber(blockHeader.rewardVote))

    val generatorJson =
      Json.obj("generator" -> blockHeader.generator.stringRepr, "generatorPublicKey" -> blockHeader.generator.toString)

    Json.obj(
      "version"   -> blockHeader.version,
      "timestamp" -> blockHeader.timestamp,
      "reference" -> blockHeader.reference.toString
    ) ++ consensusJson ++ featuresJson ++ rewardJson ++ generatorJson
  }

  def toJson(header: BlockHeader, blockSize: Int, transactionCount: Int, signature: ByteStr): JsObject =
    toJson(header) ++ Json.obj("signature" -> signature.toString, "blocksize" -> blockSize, "transactionCount" -> transactionCount)
}

object BlockSerializer {
  def toBytes(block: Block): Array[Byte] = {
    import block._

    // TODO: "Proto"block serialized manually??
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

    val transactionsRootBytes = header.version match {
      case v if v < ProtoBlockVersion => Array.empty[Byte]
      case _                          => Bytes.concat(Ints.toByteArray(header.transactionsRoot.arr.length), header.transactionsRoot.arr)
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
      transactionsRootBytes,
      signature.arr
    )
  }

  def parseBytes(bytes: Array[Byte]): Try[Block] =
    Try {
      val buf = ByteBuffer.wrap(bytes).asReadOnlyBuffer()

      val version   = buf.get
      val timestamp = buf.getLong
      val reference = ByteStr(buf.getByteArray(SignatureLength))

      val consensusBytesLength = buf.getInt
      val baseTarget           = Longs.fromByteArray(buf.getByteArray(Block.BaseTargetLength))
      val generationSignature  = ByteStr(buf.getByteArray(consensusBytesLength - Longs.BYTES))

      buf.getInt

      val transactionData = readTransactionData(version, buf)

      val featureVotes = if (version > Block.PlainBlockVersion) buf.getShortArray(buf.getInt).toSeq else Seq.empty[Short]
      val rewardVote   = if (version > Block.NgBlockVersion) buf.getLong else -1L

      val generator = buf.getPublicKey

      val transactionsRoot = if (version < Block.ProtoBlockVersion) ByteStr.empty else ByteStr(buf.getByteArray(buf.getInt()))

      val signature = ByteStr(buf.getByteArray(SignatureLength))

      val header = BlockHeader(version, timestamp, reference, baseTarget, generationSignature, generator, featureVotes, rewardVote, transactionsRoot)

      Block(header, signature, transactionData)
    }

  def toJson(block: Block): JsObject =
    BlockHeaderSerializer.toJson(block.header, block.bytes().length, block.transactionData.length, block.signature) ++
      Json.obj("fee"          -> block.transactionData.map(_.assetFee).collect { case (Waves, feeAmt) => feeAmt }.sum) ++
      Json.obj("transactions" -> JsArray(block.transactionData.map(_.json())))
}
