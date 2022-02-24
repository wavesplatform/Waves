package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer
import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.serialization._
import com.wavesplatform.transaction.{PaymentTransaction, TxAmount}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object PaymentTxSerializer {
  def toJson(tx: PaymentTransaction): JsObject = {
    import tx._
    BaseTxJson.toJson(tx) ++ Json.obj("recipient" -> recipient.stringRepr, "amount" -> amount.value)
  }

  def hashBytes(tx: PaymentTransaction): Array[Byte] = {
    import tx._
    Bytes.concat(
      Array(builder.typeId),
      Longs.toByteArray(timestamp),
      sender.arr,
      recipient.bytes,
      Longs.toByteArray(amount.value),
      Longs.toByteArray(fee.value)
    )
  }

  def bodyBytes(tx: PaymentTransaction): Array[Byte] = {
    import tx._
    Bytes.concat(
      Ints.toByteArray(builder.typeId), // 4 bytes
      Longs.toByteArray(timestamp),
      sender.arr,
      recipient.bytes,
      Longs.toByteArray(amount.value),
      Longs.toByteArray(fee.value)
    )
  }

  def toBytes(tx: PaymentTransaction): Array[Byte] = {
    Bytes.concat(this.hashBytes(tx), tx.signature.arr)
  }

  def parseBytes(bytes: Array[Byte]): Try[PaymentTransaction] = Try {
    val buf = ByteBuffer.wrap(bytes)
    require(buf.getByte == PaymentTransaction.typeId, "transaction type mismatch")
    val timestamp = buf.getLong
    val sender    = buf.getPublicKey
    val recipient = buf.getAddress
    val amount    = TxAmount.unsafeFrom(buf.getLong)
    val fee       = TxAmount.unsafeFrom(buf.getLong)
    val signature = buf.getSignature
    PaymentTransaction(sender, recipient, amount, fee, timestamp, signature, recipient.chainId)
  }
}
