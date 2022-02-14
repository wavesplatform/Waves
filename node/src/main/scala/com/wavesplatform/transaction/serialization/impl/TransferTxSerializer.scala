package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.serialization.{ByteBufferOps, Deser}
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Proofs, TxVersion}
import com.wavesplatform.utils.byteStrFormat
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object TransferTxSerializer {
  def toJson(tx: TransferTransaction): JsObject = {
    import tx._
    BaseTxJson.toJson(tx) ++ Json.obj(
      "recipient"  -> recipient.stringRepr,
      "assetId"    -> assetId.maybeBase58Repr,
      "feeAsset"   -> feeAssetId.maybeBase58Repr, // legacy v0.11.1 compat
      "amount"     -> amount,
      "attachment" -> attachment
    )
  }

  def bodyBytes(tx: TransferTransaction): Array[Byte] = {
    import tx._
    lazy val baseBytes =
      Bytes.concat(
        sender.arr,
        assetId.byteRepr,
        feeAssetId.byteRepr,
        Longs.toByteArray(timestamp),
        Longs.toByteArray(amount),
        Longs.toByteArray(fee),
        recipient.bytes,
        Deser.serializeArrayWithLength(attachment.arr)
      )

    version match {
      case TxVersion.V1 => Bytes.concat(Array(typeId), baseBytes)
      case TxVersion.V2 => Bytes.concat(Array(typeId, version), baseBytes)
      case _            => PBTransactionSerializer.bodyBytes(tx)
    }
  }

  def toBytes(tx: TransferTransaction): Array[Byte] = tx.version match {
    case TxVersion.V1 => Bytes.concat(Array(tx.typeId), tx.proofs.toSignature.arr, this.bodyBytes(tx))
    case TxVersion.V2 => Bytes.concat(Array(0: Byte), this.bodyBytes(tx), tx.proofs.bytes())
    case _            => PBTransactionSerializer.bytes(tx)
  }

  def parseBytes(bytes: Array[Byte]): Try[TransferTransaction] = Try {
    def parseCommonPart(version: TxVersion, buf: ByteBuffer): TransferTransaction = {
      val sender     = buf.getPublicKey
      val assetId    = buf.getAsset
      val feeAssetId = buf.getAsset
      val ts         = buf.getLong
      val amount     = buf.getLong
      val fee        = buf.getLong
      val recipient  = buf.getAddressOrAlias
      val attachment = buf.getByteArrayWithLength

      TransferTransaction(version, sender, recipient, assetId, amount, feeAssetId, fee, ByteStr(attachment), ts, Proofs.empty, recipient.chainId)
    }

    require(bytes.length > 2, "buffer underflow while parsing transaction")

    if (bytes(0) == 0) {
      require(bytes(1) == TransferTransaction.typeId, "transaction type mismatch")
      val buf    = ByteBuffer.wrap(bytes, 3, bytes.length - 3)
      val tx     = parseCommonPart(TxVersion.V2, buf)
      val proofs = buf.getProofs
      tx.copy(proofs = proofs)
    } else {
      require(bytes(0) == TransferTransaction.typeId, "transaction type mismatch")
      val buf       = ByteBuffer.wrap(bytes, 1, bytes.length - 1)
      val signature = buf.getSignature
      require(buf.get == TransferTransaction.typeId, "transaction type mismatch")
      parseCommonPart(TxVersion.V1, buf).copy(proofs = Proofs(signature))
    }
  }
}
