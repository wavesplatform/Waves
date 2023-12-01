package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.serialization.{ByteBufferOps, Deser}
import com.wavesplatform.transaction.{PBSince, TxPositiveAmount, TxVersion}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object SetScriptTxSerializer {
  def toJson(tx: SetScriptTransaction): JsObject = {
    import tx._
    BaseTxJson.toJson(tx) ++ Json.obj(
      "script" -> script.map(_.bytes().base64)
    ) ++ (if (tx.version == TxVersion.V1) Json.obj("chainId" -> chainId) else Json.obj())
  }

  def bodyBytes(tx: SetScriptTransaction): Array[Byte] = {
    import tx._
    version match {
      case TxVersion.V1 =>
        Bytes.concat(
          Array(tpe.id.toByte, version, chainId),
          sender.arr,
          Deser.serializeOptionOfArrayWithLength(script)(s => s.bytes().arr),
          Longs.toByteArray(fee.value),
          Longs.toByteArray(timestamp)
        )

      case _ =>
        PBTransactionSerializer.bodyBytes(tx)
    }
  }

  def toBytes(tx: SetScriptTransaction): Array[Byte] = {
    if (PBSince.affects(tx)) PBTransactionSerializer.bytes(tx)
    else Bytes.concat(Array(0: Byte), this.bodyBytes(tx), tx.proofs.bytes())
  }

  def parseBytes(bytes: Array[Byte]): Try[SetScriptTransaction] = Try {
    require(bytes.length > 2, "buffer underflow while parsing transaction")

    val buf = ByteBuffer.wrap(bytes)
    require(buf.getByte == 0 && buf.getByte == SetScriptTransaction.typeId && buf.getByte == TxVersion.V1, "transaction type mismatch")
    require(buf.getByte == AddressScheme.current.chainId, "transaction chainId mismatch")

    val sender    = buf.getPublicKey
    val script    = buf.getScript
    val fee       = TxPositiveAmount.unsafeFrom(buf.getLong)
    val timestamp = buf.getLong
    val proofs    = buf.getProofs
    SetScriptTransaction(TxVersion.V1, sender, script, fee, timestamp, proofs, AddressScheme.current.chainId)
  }
}
