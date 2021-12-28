package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer

import scala.util.Try

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.protobuf.transaction.PBOrders
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.serialization.ByteBufferOps
import com.wavesplatform.transaction.assets.exchange.*
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.utils.EthEncoding
import play.api.libs.json.{JsObject, Json}

object OrderSerializer {
  def toJson(order: Order): JsObject = {
    import order.*
    Json.obj(
      "version"          -> version,
      "id"               -> idStr(),
      "sender"           -> senderPublicKey.toAddress.toString,
      "senderPublicKey"  -> senderPublicKey,
      "matcherPublicKey" -> matcherPublicKey,
      "assetPair"        -> assetPair.json,
      "orderType"        -> orderType.toString,
      "amount"           -> amount,
      "price"            -> price,
      "timestamp"        -> timestamp,
      "expiration"       -> expiration,
      "matcherFee"       -> matcherFee,
      "signature"        -> proofs.toSignature.toString,
      "proofs"           -> proofs.proofs.map(_.toString)
    ) ++ (if (version >= Order.V3) Json.obj("matcherFeeAssetId" -> matcherFeeAssetId) else JsObject.empty) ++
      (if (version >= Order.V4) Json.obj("eip712Signature" -> eip712Signature.map(bs => EthEncoding.toHexString(bs.arr)), "priceMode" -> priceMode)
       else JsObject.empty) // TODO: Should it be hex or base58?
  }

  def bodyBytes(order: Order): Array[Byte] = {
    import order.*

    version match {
      case Order.V1 =>
        Bytes.concat(
          senderPublicKey.arr,
          matcherPublicKey.arr,
          assetPair.bytes,
          orderType.bytes,
          Longs.toByteArray(price),
          Longs.toByteArray(amount),
          Longs.toByteArray(timestamp),
          Longs.toByteArray(expiration),
          Longs.toByteArray(matcherFee)
        )

      case Order.V2 =>
        Bytes.concat(
          Array(version),
          senderPublicKey.arr,
          matcherPublicKey.arr,
          assetPair.bytes,
          orderType.bytes,
          Longs.toByteArray(price),
          Longs.toByteArray(amount),
          Longs.toByteArray(timestamp),
          Longs.toByteArray(expiration),
          Longs.toByteArray(matcherFee)
        )

      case Order.V3 =>
        Bytes.concat(
          Array(version),
          senderPublicKey.arr,
          matcherPublicKey.arr,
          assetPair.bytes,
          orderType.bytes,
          Longs.toByteArray(price),
          Longs.toByteArray(amount),
          Longs.toByteArray(timestamp),
          Longs.toByteArray(expiration),
          Longs.toByteArray(matcherFee),
          matcherFeeAssetId.byteRepr
        )

      case _ =>
        PBUtils.encodeDeterministic(PBOrders.protobuf(order.withProofs(Proofs.empty)))
    }
  }

  def toBytes(ord: Order): Array[Byte] = {
    import ord.*
    (version: @unchecked) match {
      case Order.V1            => Bytes.concat(this.bodyBytes(ord), proofs.toSignature.arr)
      case Order.V2 | Order.V3 => Bytes.concat(this.bodyBytes(ord), proofs.bytes())
    }
  }

  def parseBytes(version: Order.Version, bytes: Array[Byte]): Try[Order] = Try {
    def parseCommonPart(buf: ByteBuffer): Order = {
      val sender     = buf.getPublicKey
      val matcher    = buf.getPublicKey
      val assetPair  = AssetPair(buf.getAsset, buf.getAsset)
      val orderType  = OrderType(buf.get())
      val price      = buf.getLong
      val amount     = buf.getLong
      val timestamp  = buf.getLong
      val expiration = buf.getLong
      val matcherFee = buf.getLong
      Order(
        version,
        OrderAuthentication(sender),
        matcher,
        assetPair,
        orderType,
        amount,
        price,
        timestamp,
        expiration,
        matcherFee,
        priceMode = OrderPriceMode.Default
      )
    }

    version match {
      case Order.V1 =>
        val buf = ByteBuffer.wrap(bytes)
        parseCommonPart(buf).withProofs(Proofs(buf.getSignature))

      case Order.V2 =>
        require(bytes(0) == version, "order version mismatch")
        val buf = ByteBuffer.wrap(bytes, 1, bytes.length - 1)
        parseCommonPart(buf).withProofs(buf.getProofs)

      case Order.V3 =>
        require(bytes(0) == version, "order version mismatch")
        val buf = ByteBuffer.wrap(bytes, 1, bytes.length - 1)
        parseCommonPart(buf).copy(matcherFeeAssetId = buf.getAsset).withProofs(buf.getProofs)

      case _ =>
        throw new IllegalArgumentException(s"Unsupported order version: $version")
    }
  }
}
