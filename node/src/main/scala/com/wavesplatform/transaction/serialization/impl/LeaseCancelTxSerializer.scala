package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.crypto
import com.wavesplatform.serialization.ByteBufferOps
import com.wavesplatform.transaction.lease.LeaseCancelTransaction
import com.wavesplatform.transaction.{Proofs, TxVersion}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object LeaseCancelTxSerializer {
  def toJson(tx: LeaseCancelTransaction): JsObject =
    BaseTxJson.toJson(tx) ++ Json.obj("leaseId" -> tx.leaseId.toString, "chainId" -> tx.chainByte)

  def bodyBytes(tx: LeaseCancelTransaction): Array[Byte] = {
    import tx._
    val baseBytes = Bytes.concat(sender, Longs.toByteArray(fee), Longs.toByteArray(timestamp), leaseId)

    version match {
      case TxVersion.V1 => Bytes.concat(Array(typeId), baseBytes)
      case TxVersion.V2 => Bytes.concat(Array(typeId, version, chainByte.getOrElse(AddressScheme.current.chainId)), baseBytes)
      case TxVersion.V3 => PBTransactionSerializer.bodyBytes(tx)
    }
  }

  def toBytes(tx: LeaseCancelTransaction): Array[Byte] = {
    import tx._
    version match {
      case TxVersion.V1 => Bytes.concat(this.bodyBytes(tx), proofs.toSignature)
      case TxVersion.V2 => Bytes.concat(Array(0: Byte), this.bodyBytes(tx), proofs.bytes())
      case TxVersion.V3 => throw new IllegalArgumentException("Should be serialized with protobuf")
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

    require(bytes.length > 2, "buffer underflow while parsing transaction")

    if (bytes(0) == 0) {
      require(bytes(1) == LeaseCancelTransaction.typeId, "transaction type mismatch")
      require(bytes(2) == TxVersion.V2, "transaction version mismatch")
      require(bytes(3) == AddressScheme.current.chainId, "transaction chainId mismatch")
      val buf = ByteBuffer.wrap(bytes, 4, bytes.length - 4)
      parseCommonPart(TxVersion.V2, buf).copy(proofs = buf.getProofs)
    } else {
      require(bytes(0) == LeaseCancelTransaction.typeId, "transaction type mismatch")
      val buf = ByteBuffer.wrap(bytes, 1, bytes.length - 1)
      parseCommonPart(TxVersion.V1, buf).copy(proofs = Proofs(buf.getSignature))
    }
  }
}
