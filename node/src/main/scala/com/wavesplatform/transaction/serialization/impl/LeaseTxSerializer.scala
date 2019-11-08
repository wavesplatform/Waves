package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.{Proofs, TxVersion}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object LeaseTxSerializer {
  def toJson(tx: LeaseTransaction): JsObject = {
    import tx._
    ProvenTxJson.toJson(tx) ++ Json.obj(
      "version"   -> version,
      "amount"    -> amount,
      "recipient" -> recipient.stringRepr,
      "fee"       -> fee,
      "timestamp" -> timestamp
    )
  }

  def bodyBytes(tx: LeaseTransaction): Array[Byte] = {
    import tx._
    val baseBytes = Bytes.concat(sender, recipient.bytes.arr, Longs.toByteArray(amount), Longs.toByteArray(fee), Longs.toByteArray(timestamp))

    version match {
      case TxVersion.V1 => Bytes.concat(Array(typeId), baseBytes)
      case TxVersion.V2 => Bytes.concat(Array(typeId, version), Waves.byteRepr, baseBytes)
    }
  }

  def toBytes(tx: LeaseTransaction): Array[Byte] = {
    import tx._
    version match {
      case TxVersion.V1 => Bytes.concat(Array(typeId), proofs.toSignature, this.bodyBytes(tx))
      case TxVersion.V2 => Bytes.concat(Array(0: Byte), this.bodyBytes(tx), proofs.bytes())
    }
  }

  def parseBytes(bytes: Array[Byte]): Try[LeaseTransaction] = Try {
    def parseCommonPart(version: TxVersion, buf: ByteBuffer): LeaseTransaction = {
      val sender    = buf.getPublicKey
      val recipient = buf.getAddressOrAlias
      val amount    = buf.getLong
      val fee       = buf.getLong
      val timestamp = buf.getLong
      LeaseTransaction(version, sender, recipient, amount, fee, timestamp, Nil)
    }

    require(bytes.length > 2, "buffer underflow while parsing transfer transaction")

    if (bytes(0) == 0) {
      require(bytes(1) == LeaseTransaction.typeId, "transaction type mismatch")
      val buf = ByteBuffer.wrap(bytes, 3, bytes.length - 3)
      require(buf.getAsset == Waves, "Leasing assets is not supported yet")
      parseCommonPart(TxVersion.V2, buf).copy(proofs = buf.getProofs)
    } else {
      require(bytes(0) == LeaseTransaction.typeId, "transaction type mismatch")
      val buf = ByteBuffer.wrap(bytes, 1, bytes.length - 1)
      parseCommonPart(TxVersion.V1, buf).copy(proofs = Proofs(buf.getSignature))
    }
  }
}
