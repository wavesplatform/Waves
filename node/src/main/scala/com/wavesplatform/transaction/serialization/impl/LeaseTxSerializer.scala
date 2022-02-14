package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.serialization.ByteBufferOps
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.{Proofs, TxVersion}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object LeaseTxSerializer {
  def toJson(tx: LeaseTransaction): JsObject = {
    import tx._
    BaseTxJson.toJson(tx) ++ Json.obj(
      "amount"    -> amount,
      "recipient" -> recipient.stringRepr
    )
  }

  def bodyBytes(tx: LeaseTransaction): Array[Byte] = {
    import tx._
    val baseBytes = Bytes.concat(sender.arr, recipient.bytes, Longs.toByteArray(amount), Longs.toByteArray(fee), Longs.toByteArray(timestamp))

    version match {
      case TxVersion.V1 => Bytes.concat(Array(typeId), baseBytes)
      case TxVersion.V2 => Bytes.concat(Array(typeId, version), Waves.byteRepr, baseBytes)
      case _            => PBTransactionSerializer.bodyBytes(tx)
    }
  }

  def toBytes(tx: LeaseTransaction): Array[Byte] = tx.version match {
    case TxVersion.V1 => Bytes.concat(this.bodyBytes(tx), tx.proofs.toSignature.arr)
    case TxVersion.V2 => Bytes.concat(Array(0: Byte), this.bodyBytes(tx), tx.proofs.bytes())
    case _            => PBTransactionSerializer.bytes(tx)
  }

  def parseBytes(bytes: Array[Byte]): Try[LeaseTransaction] = Try {
    def parseCommonPart(version: TxVersion, buf: ByteBuffer): LeaseTransaction = {
      val sender    = buf.getPublicKey
      val recipient = buf.getAddressOrAlias
      val amount    = buf.getLong
      val fee       = buf.getLong
      val timestamp = buf.getLong
      LeaseTransaction(version, sender, recipient, amount, fee, timestamp, Nil, recipient.chainId)
    }

    require(bytes.length > 2, "buffer underflow while parsing transaction")

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
