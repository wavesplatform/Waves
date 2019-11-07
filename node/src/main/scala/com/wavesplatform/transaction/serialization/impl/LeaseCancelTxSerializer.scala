package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.crypto
import com.wavesplatform.transaction.lease.LeaseCancelTransaction
import com.wavesplatform.transaction.{Proofs, TxVersion}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object LeaseCancelTxSerializer {
  def toJson(tx: LeaseCancelTransaction): JsObject = {
    import tx._
    ProvenTxJson.toJson(tx) ++ Json.obj(
      "version" -> version,
      "leaseId" -> leaseId.toString,
      "chainId" -> chainByte
    )
  }

  def bodyBytes(tx: LeaseCancelTransaction): Array[Byte] = {
    import tx._
    val baseBytes = Bytes.concat(sender, Longs.toByteArray(fee), Longs.toByteArray(timestamp), leaseId)

    version match {
      case TxVersion.V1 => Bytes.concat(Array(typeId), baseBytes)
      case TxVersion.V2 => Bytes.concat(Array(typeId, version, chainByte.getOrElse(AddressScheme.current.chainId)), baseBytes)
    }
  }

  def toBytes(tx: LeaseCancelTransaction): Array[Byte] = {
    import tx._
    version match {
      case TxVersion.V1 => Bytes.concat(Array(typeId), this.bodyBytes(tx), proofs.toSignature)
      case TxVersion.V2 => Bytes.concat(Array(0: Byte), this.bodyBytes(tx), proofs.bytes())
    }
  }

  def parseBytes(bytes: Array[Byte]): Try[LeaseCancelTransaction] = Try {
    def parseCommonPart(version: TxVersion, buf: ByteBuffer): LeaseCancelTransaction = {
      val sender    = buf.getPublicKey
      val fee       = buf.getLong
      val timestamp = buf.getLong
      val leaseId   = buf.getByteArray(crypto.DigestLength)
      LeaseCancelTransaction(version, sender, leaseId, fee, timestamp, Nil)
    }

    require(bytes.length > 2, "buffer underflow while parsing transfer transaction")

    if (bytes(0) == 0) {
      require(bytes(1) == LeaseCancelTransaction.typeId, "transaction type mismatch")
      val buf    = ByteBuffer.wrap(bytes, 3, bytes.length - 3)
      val tx     = parseCommonPart(TxVersion.V2, buf)
      val proofs = buf.getProofs
      tx.copy(proofs = proofs)
    } else {
      require(bytes(0) == LeaseCancelTransaction.typeId, "transaction type mismatch")
      val buf = ByteBuffer.wrap(bytes, 1, bytes.length - 1)
      require(buf.get == LeaseCancelTransaction.typeId, "transaction type mismatch")
      val transaction = parseCommonPart(TxVersion.V1, buf)
      val signature   = buf.getSignature
      transaction.copy(proofs = Proofs(signature))
    }
  }
}
