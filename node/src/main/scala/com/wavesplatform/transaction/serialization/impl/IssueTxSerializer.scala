package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer
import com.google.common.primitives.{Bytes, Longs}
import com.google.protobuf.ByteString
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.serialization.{ByteBufferOps, Deser}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.{Proofs, TxPositiveAmount, TxDecimals, TxVersion}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object IssueTxSerializer {
  def toJson(tx: IssueTransaction): JsObject = {
    import tx.*
    BaseTxJson.toJson(tx) ++ Json.obj(
      "assetId"     -> id().toString,
      "name"        -> name.toStringUtf8,
      "quantity"    -> quantity.value,
      "reissuable"  -> reissuable,
      "decimals"    -> decimals.value,
      "description" -> description.toStringUtf8
    ) ++ (if (version >= TxVersion.V2) Json.obj("script" -> script.map(_.bytes().base64)) else JsObject.empty) ++
      (if (version == TxVersion.V2) Json.obj("chainId" -> chainId) else JsObject.empty)
  }

  def bodyBytes(tx: IssueTransaction): Array[Byte] = {
    import tx._
    lazy val baseBytes = Bytes.concat(
      sender.arr,
      Deser.serializeArrayWithLength(name.toByteArray),
      Deser.serializeArrayWithLength(description.toByteArray),
      Longs.toByteArray(quantity.value),
      Array(decimals.value),
      Deser.serializeBoolean(reissuable),
      Longs.toByteArray(fee.value),
      Longs.toByteArray(timestamp)
    )

    version match {
      case TxVersion.V1 => Bytes.concat(Array(tpe.id.toByte), baseBytes)
      case TxVersion.V2 =>
        Bytes.concat(Array(tpe.id.toByte, version, chainId), baseBytes, Deser.serializeOptionOfArrayWithLength(script)(_.bytes().arr))
      case _ => PBTransactionSerializer.bodyBytes(tx)
    }
  }

  def toBytes(tx: IssueTransaction): Array[Byte] =
    tx.version match {
      case TxVersion.V1 =>
        Bytes.concat(Array(tx.tpe.id.toByte), tx.proofs.toSignature.arr, this.bodyBytes(tx)) // Signature before body, typeId appears twice
      case TxVersion.V2 => Bytes.concat(Array(0: Byte), this.bodyBytes(tx), tx.proofs.bytes())
      case _            => PBTransactionSerializer.bytes(tx)
    }

  def parseBytes(bytes: Array[Byte]): Try[IssueTransaction] = Try {
    def parseCommonPart(version: TxVersion, buf: ByteBuffer): IssueTransaction = {
      val sender      = buf.getPublicKey
      val name        = Deser.parseArrayWithLength(buf)
      val description = Deser.parseArrayWithLength(buf)
      val quantity    = TxPositiveAmount.unsafeFrom(buf.getLong)
      val decimals    = TxDecimals.unsafeFrom(buf.getByte)
      val reissuable  = buf.getBoolean
      val fee         = TxPositiveAmount.unsafeFrom(buf.getLong)
      val timestamp   = buf.getLong

      IssueTransaction(
        version,
        sender,
        ByteString.copyFrom(name),
        ByteString.copyFrom(description),
        quantity,
        decimals,
        reissuable,
        None,
        fee,
        timestamp,
        Proofs.empty,
        AddressScheme.current.chainId
      )
    }

    require(bytes.length > 2, "buffer underflow while parsing transaction")

    if (bytes(0) == 0) {
      require(bytes(1) == IssueTransaction.typeId, "transaction type mismatch")
      val buf = ByteBuffer.wrap(bytes, 4, bytes.length - 4)
      parseCommonPart(TxVersion.V2, buf).copy(script = buf.getScript, proofs = buf.getProofs)
    } else {
      require(bytes(0) == IssueTransaction.typeId, "transaction type mismatch")
      val buf       = ByteBuffer.wrap(bytes, 1, bytes.length - 1)
      val signature = buf.getSignature
      require(buf.getByte == IssueTransaction.typeId, "transaction type mismatch")
      parseCommonPart(TxVersion.V1, buf).copy(proofs = Proofs(signature))
    }
  }
}
