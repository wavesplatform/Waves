package com.wavesplatform.transaction.serialization.impl

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.serialization.*
import com.wavesplatform.transaction.{GenesisTransaction, TxNonNegativeAmount}
import play.api.libs.json.{JsObject, Json}

import java.nio.ByteBuffer
import scala.util.Try

object GenesisTxSerializer {
  private val BaseLength: Int = Address.AddressLength + Longs.BYTES * 2

  def toJson(tx: GenesisTransaction): JsObject = {
    import tx.*
    Json.obj(
      "type"      -> tpe.id,
      "id"        -> id().toString,
      "fee"       -> 0,
      "timestamp" -> timestamp,
      "signature" -> signature.toString,
      "recipient" -> recipient.toString,
      "amount"    -> amount.value
    )
  }

  def toBytes(tx: GenesisTransaction): Array[Byte] = {
    import tx.*
    val typeBytes      = Array(tpe.id.toByte)
    val timestampBytes = Longs.toByteArray(timestamp)
    val rcpBytes       = recipient.bytes
    val amountBytes    = Longs.toByteArray(amount.value)
    require(rcpBytes.length == Address.AddressLength)
    val res = Bytes.concat(typeBytes, timestampBytes, rcpBytes, amountBytes)
    require(res.length == BaseLength + 1)
    res
  }

  def parseBytes(bytes: Array[Byte]): Try[GenesisTransaction] = Try {
    val buf = ByteBuffer.wrap(bytes)
    require(buf.getByte == GenesisTransaction.typeId, "transaction type mismatch")
    val timestamp = buf.getLong
    val recipient = buf.getAddress(None)
    val amount    = TxNonNegativeAmount.unsafeFrom(buf.getLong)
    GenesisTransaction(
      recipient,
      amount,
      timestamp,
      ByteStr(GenesisTransaction.generateSignature(recipient, amount.value, timestamp)),
      recipient.chainId
    )
  }
}
