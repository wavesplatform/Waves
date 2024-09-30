package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.serialization.ByteBufferOps
import com.wavesplatform.transaction.lease.LeaseCancelTransaction
import com.wavesplatform.transaction.{Proofs, TxPositiveAmount, TxVersion}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object LeaseCancelTxSerializer {
  def toJson(tx: LeaseCancelTransaction): JsObject =
    BaseTxJson.toJson(tx) ++ Json.obj("leaseId" -> tx.leaseId.toString) ++
      (if (tx.version == TxVersion.V2) Json.obj("chainId" -> tx.chainId) else Json.obj())

  def bodyBytes(tx: LeaseCancelTransaction): Array[Byte] = {
    import tx.*
    val baseBytes = Bytes.concat(sender.arr, Longs.toByteArray(fee.value), Longs.toByteArray(timestamp), leaseId.arr)

    version match {
      case TxVersion.V1 => Bytes.concat(Array(tpe.id.toByte), baseBytes)
      case TxVersion.V2 => Bytes.concat(Array(tpe.id.toByte, version, chainId), baseBytes)
      case _            => PBTransactionSerializer.bodyBytes(tx)
    }
  }

  def toBytes(tx: LeaseCancelTransaction): Array[Byte] = {
    tx.version match {
      case TxVersion.V1 => Bytes.concat(this.bodyBytes(tx), tx.proofs.toSignature.arr)
      case TxVersion.V2 => Bytes.concat(Array(0: Byte), this.bodyBytes(tx), tx.proofs.bytes())
      case _            => PBTransactionSerializer.bytes(tx)
    }
  }

  def parseBytes(bytes: Array[Byte]): Try[LeaseCancelTransaction] = Try {
    def parseCommonPart(version: TxVersion, buf: ByteBuffer): LeaseCancelTransaction = {
      val sender    = buf.getPublicKey
      val fee       = TxPositiveAmount.unsafeFrom(buf.getLong)
      val timestamp = buf.getLong
      val leaseId   = buf.getByteArray(crypto.DigestLength)
      LeaseCancelTransaction(version, sender, ByteStr(leaseId), fee, timestamp, Nil, AddressScheme.current.chainId)
    }

    require(bytes.length > 2, "buffer underflow while parsing transaction")

    if (bytes(0) == 0) {
      require(bytes(1) == LeaseCancelTransaction.typeId, "transaction type mismatch")
      require(bytes(2) == TxVersion.V2, "transaction version mismatch")
      val buf = ByteBuffer.wrap(bytes, 4, bytes.length - 4)
      parseCommonPart(TxVersion.V2, buf).copy(proofs = buf.getProofs)
    } else {
      require(bytes(0) == LeaseCancelTransaction.typeId, "transaction type mismatch")
      val buf = ByteBuffer.wrap(bytes, 1, bytes.length - 1)
      parseCommonPart(TxVersion.V1, buf).copy(proofs = Proofs(buf.getSignature))
    }
  }
}
