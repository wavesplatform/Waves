package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.serialization.ByteBufferOps
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.{Proofs, TxVersion}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object IssueTxSerializer {
  def toJson(tx: IssueTransaction): JsObject = {
    import tx._
    BaseTxJson.toJson(tx) ++ Json.obj(
      "assetId"     -> id().toString,
      "name"        -> new String(name, StandardCharsets.UTF_8),
      "quantity"    -> quantity,
      "reissuable"  -> reissuable,
      "decimals"    -> decimals,
      "description" -> new String(description, StandardCharsets.UTF_8)
    ) ++ (if (version >= TxVersion.V2) Json.obj("chainId" -> chainByte, "script" -> script.map(_.bytes().base64)) else JsObject.empty)
  }

  def bodyBytes(tx: IssueTransaction): Array[Byte] = {
    import tx._
    lazy val baseBytes = Bytes.concat(
      sender,
      Deser.serializeArrayWithLength(name),
      Deser.serializeArrayWithLength(description),
      Longs.toByteArray(quantity),
      Array(decimals),
      Deser.serializeBoolean(reissuable),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    )

    version match {
      case TxVersion.V1 =>
        Bytes.concat(Array(typeId), baseBytes)

      case TxVersion.V2 =>
        Bytes.concat(
          Array(builder.typeId, version, chainByte.get),
          baseBytes,
          Deser.serializeOptionOfArrayWithLength(script)(_.bytes())
        )

      case TxVersion.V3 =>
        PBTransactionSerializer.bodyBytes(tx)
    }
  }

  def toBytes(tx: IssueTransaction): Array[Byte] = {
    import tx._
    version match {
      case TxVersion.V1 => Bytes.concat(Array(typeId), proofs.toSignature, this.bodyBytes(tx)) // Signature before body, typeId appears twice
      case TxVersion.V2 => Bytes.concat(Array(0: Byte), this.bodyBytes(tx), proofs.bytes())
      case TxVersion.V3 => PBTransactionSerializer.toBytesPrefixed(tx)
    }
  }

  def parseBytes(bytes: Array[Byte]): Try[IssueTransaction] = Try {
    def parseCommonPart(version: TxVersion, buf: ByteBuffer): IssueTransaction = {
      val sender      = buf.getPublicKey
      val name        = Deser.parseArrayWithLength(buf)
      val description = Deser.parseArrayWithLength(buf)
      val quantity    = buf.getLong
      val decimals    = buf.getByte
      val reissuable  = buf.getBoolean
      val fee         = buf.getLong
      val timestamp   = buf.getLong
      IssueTransaction(version, sender, name, description, quantity, decimals, reissuable, None, fee, timestamp, Nil)
    }

    require(bytes.length > 2, "buffer underflow while parsing transaction")

    if (bytes(0) == 0) {
      require(bytes(1) == IssueTransaction.typeId, "transaction type mismatch")
      bytes(2) match {
        case TxVersion.V2 =>
          val buf = ByteBuffer.wrap(bytes, 4, bytes.length - 4)
          parseCommonPart(TxVersion.V2, buf).copy(script = buf.getScript, proofs = buf.getProofs)

        case TxVersion.V3 =>
          PBTransactionSerializer.fromBytesAs(bytes.drop(3), IssueTransaction)
      }
    } else {
      require(bytes(0) == IssueTransaction.typeId, "transaction type mismatch")
      val buf       = ByteBuffer.wrap(bytes, 1, bytes.length - 1)
      val signature = buf.getSignature
      require(buf.getByte == IssueTransaction.typeId, "transaction type mismatch")
      parseCommonPart(TxVersion.V1, buf).copy(proofs = Proofs(signature))
    }
  }
}
