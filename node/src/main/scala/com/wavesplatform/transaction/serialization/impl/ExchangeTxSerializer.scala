package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer
import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.serialization.ByteBufferOps
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.wavesplatform.transaction.{Proofs, TxPositiveAmount, TxExchangeAmount, TxExchangePrice, TxVersion}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object ExchangeTxSerializer {
  def toJson(tx: ExchangeTransaction): JsObject = {
    import tx.*
    BaseTxJson.toJson(tx) ++ Json.obj(
      "order1"         -> order1.json(),
      "order2"         -> order2.json(),
      "amount"         -> amount.value,
      "price"          -> price.value,
      "buyMatcherFee"  -> buyMatcherFee,
      "sellMatcherFee" -> sellMatcherFee
    )
  }

  def bodyBytes(tx: ExchangeTransaction): Array[Byte] = {
    import tx.*

    version match {
      case TxVersion.V1 =>
        Bytes.concat(
          Array(tpe.id.toByte),
          Ints.toByteArray(order1.bytes().length),
          Ints.toByteArray(order2.bytes().length),
          order1.bytes(),
          order2.bytes(),
          Longs.toByteArray(price.value),
          Longs.toByteArray(amount.value),
          Longs.toByteArray(buyMatcherFee),
          Longs.toByteArray(sellMatcherFee),
          Longs.toByteArray(fee.value),
          Longs.toByteArray(timestamp)
        )

      case TxVersion.V2 =>
        def orderMark(version: TxVersion): Array[Byte] =
          if (version == 1) Array(1: Byte) else Array.emptyByteArray

        Bytes.concat(
          Array(0: Byte, tpe.id.toByte, version),
          Ints.toByteArray(order1.bytes().length),
          orderMark(order1.version),
          order1.bytes(),
          Ints.toByteArray(order2.bytes().length),
          orderMark(order2.version),
          order2.bytes(),
          Longs.toByteArray(price.value),
          Longs.toByteArray(amount.value),
          Longs.toByteArray(buyMatcherFee),
          Longs.toByteArray(sellMatcherFee),
          Longs.toByteArray(fee.value),
          Longs.toByteArray(timestamp)
        )

      case _ =>
        PBTransactionSerializer.bodyBytes(tx)
    }
  }

  def toBytes(tx: ExchangeTransaction): Array[Byte] = {
    tx.version match {
      case TxVersion.V1 => Bytes.concat(this.bodyBytes(tx), tx.proofs.toSignature.arr)
      case TxVersion.V2 => Bytes.concat(this.bodyBytes(tx), tx.proofs.bytes())
      case _            => PBTransactionSerializer.bytes(tx)
    }
  }

  def parseBytes(bytes: Array[Byte]): Try[ExchangeTransaction] = Try {
    def parseV1(buf: ByteBuffer): ExchangeTransaction = {
      val order1Length   = buf.getInt
      val order2Length   = buf.getInt
      val order1         = Order.parseBytes(TxVersion.V1, buf.getByteArray(order1Length)).get
      val order2         = Order.parseBytes(TxVersion.V1, buf.getByteArray(order2Length)).get
      val price          = TxExchangePrice.unsafeFrom(buf.getLong)
      val amount         = TxExchangeAmount.unsafeFrom(buf.getLong)
      val buyMatcherFee  = buf.getLong
      val sellMatcherFee = buf.getLong
      val fee            = TxPositiveAmount.unsafeFrom(buf.getLong)
      val timestamp      = buf.getLong
      ExchangeTransaction(
        TxVersion.V1,
        order1,
        order2,
        amount,
        price,
        buyMatcherFee,
        sellMatcherFee,
        fee,
        timestamp,
        Proofs.empty,
        AddressScheme.current.chainId
      )
    }

    def parseV2(buf: ByteBuffer): ExchangeTransaction = {
      val order1         = buf.getVersionedOrder
      val order2         = buf.getVersionedOrder
      val price          = TxExchangePrice.unsafeFrom(buf.getLong)
      val amount         = TxExchangeAmount.unsafeFrom(buf.getLong)
      val buyMatcherFee  = buf.getLong
      val sellMatcherFee = buf.getLong
      val fee            = TxPositiveAmount.unsafeFrom(buf.getLong)
      val timestamp      = buf.getLong
      ExchangeTransaction(
        TxVersion.V2,
        order1,
        order2,
        amount,
        price,
        buyMatcherFee,
        sellMatcherFee,
        fee,
        timestamp,
        Proofs.empty,
        AddressScheme.current.chainId
      )
    }

    require(bytes.length > 2, "buffer underflow while parsing transaction")

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
