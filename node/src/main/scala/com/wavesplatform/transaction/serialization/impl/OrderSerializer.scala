package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.protobuf.transaction.PBOrders
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.serialization.ByteBufferOps
import com.wavesplatform.transaction.assets.exchange.*
import com.wavesplatform.transaction.{Proofs, TxExchangeAmount, TxMatcherFee, TxOrderPrice}
import com.wavesplatform.utils.{byteStrFormat, EthEncoding}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

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
      "amount"           -> amount.value,
      "price"            -> price.value,
      "timestamp"        -> timestamp,
      "expiration"       -> expiration,
      "matcherFee"       -> matcherFee.value,
      "signature"        -> proofs.toSignature.toString,
      "proofs"           -> proofs.proofs.map(_.toString)
    ) ++ (if (version >= Order.V3) Json.obj("matcherFeeAssetId" -> matcherFeeAssetId) else JsObject.empty) ++
      (if (version >= Order.V4) Json.obj("eip712Signature" -> eip712Signature.map(bs => EthEncoding.toHexString(bs.arr)), "priceMode" -> priceMode)
       else JsObject.empty) ++
      attachment.map(attach => Json.obj("attachment" -> attach)).getOrElse(JsObject.empty)
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
          Longs.toByteArray(price.value),
          Longs.toByteArray(amount.value),
          Longs.toByteArray(timestamp),
          Longs.toByteArray(expiration),
          Longs.toByteArray(matcherFee.value)
        )

      case Order.V2 =>
        Bytes.concat(
          Array(version),
          senderPublicKey.arr,
          matcherPublicKey.arr,
          assetPair.bytes,
          orderType.bytes,
          Longs.toByteArray(price.value),
          Longs.toByteArray(amount.value),
          Longs.toByteArray(timestamp),
          Longs.toByteArray(expiration),
          Longs.toByteArray(matcherFee.value)
        )

      case Order.V3 =>
        Bytes.concat(
          Array(version),
          senderPublicKey.arr,
          matcherPublicKey.arr,
          assetPair.bytes,
          orderType.bytes,
          Longs.toByteArray(price.value),
          Longs.toByteArray(amount.value),
          Longs.toByteArray(timestamp),
          Longs.toByteArray(expiration),
          Longs.toByteArray(matcherFee.value),
          matcherFeeAssetId.byteRepr
        )

      case _ =>
        val orderWithoutProofs = order.orderAuthentication match {
          case OrderAuthentication.OrderProofs(_, _)  => order.withProofs(Proofs.empty)
          case OrderAuthentication.Eip712Signature(_) => order // Keep original signature
        }
        PBUtils.encodeDeterministic(PBOrders.protobuf(orderWithoutProofs))
    }
  }

  def toBytes(ord: Order): Array[Byte] = {
    import ord.*
    version match {
      case Order.V1            => Bytes.concat(this.bodyBytes(ord), proofs.toSignature.arr)
      case Order.V2 | Order.V3 => Bytes.concat(this.bodyBytes(ord), proofs.bytes())
      case other               => throw new IllegalArgumentException(s"Couldn't serialize OrderV$other")
    }
  }

  def parseBytes(version: Order.Version, bytes: Array[Byte]): Try[Order] = Try {
    def parseCommonPart(buf: ByteBuffer): Order = {
      val sender     = buf.getPublicKey
      val matcher    = buf.getPublicKey
      val assetPair  = AssetPair(buf.getAsset, buf.getAsset)
      val orderType  = OrderType(buf.get())
      val price      = TxOrderPrice.unsafeFrom(buf.getLong)
      val amount     = TxExchangeAmount.unsafeFrom(buf.getLong)
      val timestamp  = buf.getLong
      val expiration = buf.getLong
      val matcherFee = TxMatcherFee.unsafeFrom(buf.getLong)
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
