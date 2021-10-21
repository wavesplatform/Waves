package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.serialization.{ByteBufferOps, Deser}
import com.wavesplatform.transaction.{CreateAliasTransaction, Proofs, Transaction, TxVersion}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object CreateAliasTxSerializer {
  def toJson(tx: CreateAliasTransaction): JsObject = {
    import tx._
    BaseTxJson.toJson(tx) ++ Json.obj("alias" -> aliasName)
  }

  def bodyBytes(tx: CreateAliasTransaction): Array[Byte] = {
    import tx._

    lazy val base = Bytes.concat(
      sender.arr,
      Deser.serializeArrayWithLength(alias.bytes),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    )

    version match {
      case TxVersion.V1 => Bytes.concat(Array(tpe.id.toByte), base)
      case TxVersion.V2 => Bytes.concat(Array(tpe.id.toByte, version), base)
      case _            => PBTransactionSerializer.bodyBytes(tx)
    }
  }

  def toBytes(tx: CreateAliasTransaction): Array[Byte] = tx.version match {
    case TxVersion.V1 => Bytes.concat(this.bodyBytes(tx), tx.signature.arr)
    case TxVersion.V2 => Bytes.concat(Array(0: Byte), this.bodyBytes(tx), tx.proofs.bytes())
    case _            => PBTransactionSerializer.bytes(tx)
  }

  def parseBytes(bytes: Array[Byte]): Try[CreateAliasTransaction] = Try {
    require(bytes.length > 3, "buffer underflow while parsing transaction")
    (bytes.take(3): @unchecked) match {
      case Array(CreateAliasTransaction.typeId, _, _) =>
        val buf       = ByteBuffer.wrap(bytes, 1, bytes.length - 1)
        val sender    = buf.getPublicKey
        val alias     = buf.getAlias
        val fee       = buf.getLong
        val timestamp = buf.getLong
        val signature = buf.getSignature
        CreateAliasTransaction(Transaction.V1, sender, alias.name, fee, timestamp, Proofs(signature), alias.chainId)

      case Array(0, CreateAliasTransaction.typeId, 2) =>
        val buf       = ByteBuffer.wrap(bytes, 3, bytes.length - 3)
        val sender    = buf.getPublicKey
        val alias     = buf.getAlias
        val fee       = buf.getLong
        val timestamp = buf.getLong
        val proofs    = buf.getProofs
        CreateAliasTransaction(Transaction.V2, sender, alias.name, fee, timestamp, proofs, alias.chainId)

      case Array(b1, b2, b3) => throw new IllegalArgumentException(s"Invalid tx header bytes: $b1, $b2, $b3")
    }
  }
}
