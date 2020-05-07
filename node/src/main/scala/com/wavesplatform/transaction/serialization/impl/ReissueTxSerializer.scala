package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.serialization._
import com.wavesplatform.transaction.assets.ReissueTransaction
import com.wavesplatform.transaction.{Proofs, TxVersion}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object ReissueTxSerializer {
  def toJson(tx: ReissueTransaction): JsObject = {
    import tx._
    BaseTxJson.toJson(tx) ++ Json.obj(
      "assetId"    -> asset.id.toString,
      "quantity"   -> quantity,
      "reissuable" -> reissuable
    ) ++ (if (tx.version == TxVersion.V2) Json.obj("chainId" -> chainId) else Json.obj())
  }

  def bodyBytes(tx: ReissueTransaction): Array[Byte] = {
    import tx._
    lazy val baseBytes = Bytes.concat(
      sender.arr,
      asset.id.arr,
      Longs.toByteArray(quantity),
      if (reissuable) Array(1: Byte) else Array(0: Byte),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    )

    version match {
      case TxVersion.V1 =>
        Bytes.concat(Array(typeId), baseBytes)

      case TxVersion.V2 =>
        Bytes.concat(
          Array(builder.typeId, version, chainId),
          baseBytes
        )

      case _ =>
        PBTransactionSerializer.bodyBytes(tx)
    }
  }

  def toBytes(tx: ReissueTransaction): Array[Byte] = {
    tx.version match {
      case TxVersion.V1 => Bytes.concat(Array(tx.typeId), tx.proofs.toSignature.arr, this.bodyBytes(tx)) // Signature before body, typeId appears twice
      case TxVersion.V2 => Bytes.concat(Array(0: Byte), this.bodyBytes(tx), tx.proofs.bytes())
      case _            => PBTransactionSerializer.bytes(tx)
    }
  }

  def parseBytes(bytes: Array[Byte]): Try[ReissueTransaction] = Try {
    def parseCommonPart(version: TxVersion, buf: ByteBuffer): ReissueTransaction = {
      val sender     = buf.getPublicKey
      val asset      = buf.getIssuedAsset
      val quantity   = buf.getLong
      val reissuable = buf.getBoolean
      val fee        = buf.getLong
      val timestamp  = buf.getLong
      ReissueTransaction(version, sender, asset, quantity, reissuable, fee, timestamp, Nil, AddressScheme.current.chainId)
    }

    require(bytes.length > 2, "buffer underflow while parsing transaction")

    if (bytes(0) == 0) {
      require(bytes(1) == ReissueTransaction.typeId, "transaction type mismatch")
      val buf = ByteBuffer.wrap(bytes, 4, bytes.length - 4)
      parseCommonPart(TxVersion.V2, buf).copy(proofs = buf.getProofs)
    } else {
      require(bytes(0) == ReissueTransaction.typeId, "transaction type mismatch")
      val buf       = ByteBuffer.wrap(bytes, 1, bytes.length - 1)
      val signature = buf.getSignature
      require(buf.getByte == ReissueTransaction.typeId, "transaction type mismatch")
      parseCommonPart(TxVersion.V1, buf).copy(proofs = Proofs(signature))
    }
  }
}
