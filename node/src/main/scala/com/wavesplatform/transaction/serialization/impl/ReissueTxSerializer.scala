package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.serialization.*
import com.wavesplatform.transaction.assets.ReissueTransaction
import com.wavesplatform.transaction.{Proofs, TxPositiveAmount, TxVersion}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object ReissueTxSerializer {
  def toJson(tx: ReissueTransaction): JsObject = {
    import tx.*
    BaseTxJson.toJson(tx) ++ Json.obj(
      "assetId"    -> asset.id.toString,
      "quantity"   -> quantity.value,
      "reissuable" -> reissuable
    ) ++ (if (tx.version == TxVersion.V2) Json.obj("chainId" -> chainId) else Json.obj())
  }

  def bodyBytes(tx: ReissueTransaction): Array[Byte] = {
    import tx.*
    lazy val baseBytes = Bytes.concat(
      sender.arr,
      asset.id.arr,
      Longs.toByteArray(quantity.value),
      if (reissuable) Array(1: Byte) else Array(0: Byte),
      Longs.toByteArray(fee.value),
      Longs.toByteArray(timestamp)
    )

    version match {
      case TxVersion.V1 =>
        Bytes.concat(Array(tpe.id.toByte), baseBytes)

      case TxVersion.V2 =>
        Bytes.concat(
          Array(tpe.id.toByte, version, chainId),
          baseBytes
        )

      case _ =>
        PBTransactionSerializer.bodyBytes(tx)
    }
  }

  def toBytes(tx: ReissueTransaction): Array[Byte] = {
    tx.version match {
      case TxVersion.V1 =>
        Bytes.concat(Array(tx.tpe.id.toByte), tx.proofs.toSignature.arr, this.bodyBytes(tx)) // Signature before body, typeId appears twice
      case TxVersion.V2 => Bytes.concat(Array(0: Byte), this.bodyBytes(tx), tx.proofs.bytes())
      case _            => PBTransactionSerializer.bytes(tx)
    }
  }

  def parseBytes(bytes: Array[Byte]): Try[ReissueTransaction] = Try {
    def parseCommonPart(version: TxVersion, buf: ByteBuffer): ReissueTransaction = {
      val sender     = buf.getPublicKey
      val asset      = buf.getIssuedAsset
      val quantity   = TxPositiveAmount.unsafeFrom(buf.getLong)
      val reissuable = buf.getBoolean
      val fee        = TxPositiveAmount.unsafeFrom(buf.getLong)
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
