package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.serialization.TxSerializer
import com.wavesplatform.transaction.transfer.TransferTransaction
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object TransferTxSerializer extends TxSerializer[TransferTransaction] {
  override def toJson(tx: TransferTransaction): JsObject = {
    import tx._
    ProvenTxJson.toJson(tx) ++ Json.obj(
      "version"    -> version,
      "recipient"  -> recipient.stringRepr,
      "assetId"    -> assetId.maybeBase58Repr,
      "feeAsset"   -> feeAssetId.maybeBase58Repr, // legacy v0.11.1 compat
      "amount"     -> amount,
      "attachment" -> Base58.encode(attachment)
    )
  }

  override def bodyBytes(tx: TransferTransaction): Array[Byte] = {
    import tx._
    val baseBytes = {
      Bytes.concat(
        sender,
        assetId.byteRepr,
        feeAssetId.byteRepr,
        Longs.toByteArray(timestamp),
        Longs.toByteArray(amount),
        Longs.toByteArray(fee),
        recipient.bytes.arr,
        Deser.serializeArray(attachment)
      )
    }

    version match {
      case 1 => Array(typeId) ++ baseBytes
      case 2 => Array(typeId, version) ++ baseBytes
    }
  }

  override def toBytes(tx: TransferTransaction): Array[Byte] = {
    import tx._
    version match {
      case 1 => Bytes.concat(Array(typeId), proofs.proofs.head, this.bodyBytes(tx))
      case 2 => Bytes.concat(Array(0: Byte), this.bodyBytes(tx), proofs.bytes())
    }
  }

  override def parseBytes(bytes: Array[Byte]): Try[TransferTransaction] = Try {
    def parseCommonPart(version: TxVersion, buf: ByteBuffer): TransferTransaction = {
      val sender     = buf.getPublicKey
      val assetId    = buf.getAsset
      val feeAssetId = buf.getAsset
      val ts         = buf.getLong
      val amount     = buf.getLong
      val fee        = buf.getLong
      val recipient  = buf.getAddressOrAlias
      val attachment = buf.getPrefixedByteArray

      TransferTransaction(version, sender, recipient, assetId, amount, feeAssetId, fee, attachment, ts, Proofs.empty)
    }

    require(bytes.length > 2, "buffer underflow while parsing transfer transaction")

    if (bytes(0) == 0) {
      require(bytes(1) == TransferTransaction.typeId, "transaction type mismatch")
      val buf    = ByteBuffer.wrap(bytes, 3, bytes.length - 3)
      val tx     = parseCommonPart(2, buf)
      val proofs = buf.getProofs
      tx.copy(proofs = proofs)
    } else {
      require(bytes(0) == TransferTransaction.typeId, "transaction type mismatch")
      val buf       = ByteBuffer.wrap(bytes, 1, bytes.length - 1)
      val signature = buf.getSignature
      require(buf.get == TransferTransaction.typeId, "transaction type mismatch")
      parseCommonPart(1, buf).copy(proofs = Proofs(signature))
    }
  }
}
