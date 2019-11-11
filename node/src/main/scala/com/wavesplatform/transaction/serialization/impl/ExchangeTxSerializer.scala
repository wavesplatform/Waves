package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.wavesplatform.transaction.{Proofs, TxVersion}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object ExchangeTxSerializer {
  def toJson(tx: ExchangeTransaction): JsObject = {
    import tx._
    ProvenTxJson.toJson(tx) ++ Json.obj(
      "order1"         -> buyOrder.json(),
      "order2"         -> sellOrder.json(),
      "amount"         -> amount,
      "price"          -> price,
      "buyMatcherFee"  -> buyMatcherFee,
      "sellMatcherFee" -> sellMatcherFee
    )
  }

  def bodyBytes(tx: ExchangeTransaction): Array[Byte] = {
    import tx._

    version match {
      case TxVersion.V1 =>
        Bytes.concat(
          Array(builder.typeId),
          Ints.toByteArray(buyOrder.bytes().length),
          Ints.toByteArray(sellOrder.bytes().length),
          buyOrder.bytes(),
          sellOrder.bytes(),
          Longs.toByteArray(price),
          Longs.toByteArray(amount),
          Longs.toByteArray(buyMatcherFee),
          Longs.toByteArray(sellMatcherFee),
          Longs.toByteArray(fee),
          Longs.toByteArray(timestamp)
        )

      case TxVersion.V2 =>
        def orderMark(version: TxVersion): Array[Byte] =
          if (version == 1) Array(1: Byte) else Array.emptyByteArray

        Bytes.concat(
          Array(0: Byte, builder.typeId, version),
          Ints.toByteArray(buyOrder.bytes().length),
          orderMark(buyOrder.version),
          buyOrder.bytes(),
          Ints.toByteArray(sellOrder.bytes().length),
          orderMark(sellOrder.version),
          sellOrder.bytes(),
          Longs.toByteArray(price),
          Longs.toByteArray(amount),
          Longs.toByteArray(buyMatcherFee),
          Longs.toByteArray(sellMatcherFee),
          Longs.toByteArray(fee),
          Longs.toByteArray(timestamp)
        )
    }
  }

  def toBytes(tx: ExchangeTransaction): Array[Byte] = {
    import tx._
    version match {
      case TxVersion.V1 => Bytes.concat(this.bodyBytes(tx), proofs.toSignature)
      case TxVersion.V2 => Bytes.concat(this.bodyBytes(tx), proofs.bytes())
    }
  }

  def parseBytes(bytes: Array[Byte]): Try[ExchangeTransaction] = Try {
    def parseV1(buf: ByteBuffer): ExchangeTransaction = {
      val buyLength      = buf.getInt
      val sellLength     = buf.getInt
      val buy            = Order.fromBytes(TxVersion.V1, buf.getByteArray(buyLength))
      val sell           = Order.fromBytes(TxVersion.V1, buf.getByteArray(sellLength))
      val price          = buf.getLong
      val amount         = buf.getLong
      val buyMatcherFee  = buf.getLong
      val sellMatcherFee = buf.getLong
      val fee            = buf.getLong
      val timestamp      = buf.getLong
      ExchangeTransaction(buy, sell, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, Proofs.empty)
    }

    def parseV2(buf: ByteBuffer): ExchangeTransaction = {
      val buy            = buf.getVersionedOrder
      val sell           = buf.getVersionedOrder
      val price          = buf.getLong
      val amount         = buf.getLong
      val buyMatcherFee  = buf.getLong
      val sellMatcherFee = buf.getLong
      val fee            = buf.getLong
      val timestamp      = buf.getLong
      ExchangeTransaction(buy, sell, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, Proofs.empty)
    }

    require(bytes.length > 2, "buffer underflow while parsing transfer transaction")

    if (bytes(0) == 0) {
      require(bytes(1) == ExchangeTransaction.typeId, "transaction type mismatch")
      val buf = ByteBuffer.wrap(bytes, 3, bytes.length - 3)
      parseV2(buf).copy(proofs = buf.getProofs)
    } else {
      require(bytes(0) == ExchangeTransaction.typeId, "transaction type mismatch")
      val buf = ByteBuffer.wrap(bytes, 1, bytes.length - 1)
      parseV1(buf).copy(proofs = Proofs(buf.getSignature))
    }
  }
}
