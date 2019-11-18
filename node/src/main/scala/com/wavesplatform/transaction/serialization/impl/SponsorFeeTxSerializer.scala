package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.serialization.ByteBufferOps
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.assets.SponsorFeeTransaction
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object SponsorFeeTxSerializer {
  def toJson(tx: SponsorFeeTransaction): JsObject = {
    import tx._
    BaseTxJson.toJson(tx) ++ Json.obj(
      "assetId"              -> asset.id.toString,
      "minSponsoredAssetFee" -> minSponsoredAssetFee
    )
  }

  def bodyBytes(tx: SponsorFeeTransaction): Array[Byte] = {
    import tx._
    Bytes.concat(
      Array(builder.typeId, version),
      sender,
      asset.id,
      Longs.toByteArray(minSponsoredAssetFee.getOrElse(0)),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    )
  }

  def toBytes(tx: SponsorFeeTransaction): Array[Byte] = {
    import tx._
    Bytes.concat(Array(0: Byte, builder.typeId, version), bodyBytes(), proofs.bytes()) // [0, typeId, version] appears twice
  }

  def parseBytes(bytes: Array[Byte]): Try[SponsorFeeTransaction] = Try {
    require(bytes.length > 2, "buffer underflow while parsing transaction")

    val buf = ByteBuffer.wrap(bytes)
    require(buf.getByte == 0 && buf.getByte == SponsorFeeTransaction.typeId && buf.getByte == TxVersion.V1, "transaction type mismatch")
    require(buf.getByte == 0 && buf.getByte == SponsorFeeTransaction.typeId && buf.getByte == TxVersion.V1, "transaction type mismatch")

    val sender               = buf.getPublicKey
    val asset                = buf.getIssuedAsset
    val minSponsoredAssetFee = buf.getLong
    val fee                  = buf.getLong
    val timestamp            = buf.getLong
    val proofs               = buf.getProofs
    SponsorFeeTransaction(TxVersion.V1, sender, asset, Some(minSponsoredAssetFee).filterNot(_ == 0), fee, timestamp, proofs)
  }
}
