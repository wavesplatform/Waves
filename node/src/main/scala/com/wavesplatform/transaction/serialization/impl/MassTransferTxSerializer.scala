package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer
import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.serialization.*
import com.wavesplatform.transaction.{PBSince, TxNonNegativeAmount, TxPositiveAmount, TxVersion}
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.{ParsedTransfer, Transfer}
import com.wavesplatform.utils.byteStrFormat
import play.api.libs.json.{JsObject, JsValue, Json}

import scala.util.Try

object MassTransferTxSerializer {
  def transfersJson(transfers: Seq[ParsedTransfer]): JsValue =
    Json.toJson(transfers.map { case ParsedTransfer(address, amount) => Transfer(address.toString, amount.value) })

  def toJson(tx: MassTransferTransaction): JsObject = {
    import tx.*
    BaseTxJson.toJson(tx) ++ Json.obj(
      "assetId"       -> assetId.maybeBase58Repr,
      "attachment"    -> attachment,
      "transferCount" -> transfers.size,
      "totalAmount"   -> transfers.map(_.amount.value).sum,
      "transfers"     -> transfersJson(transfers)
    )
  }

  def bodyBytes(tx: MassTransferTransaction): Array[Byte] = {
    import tx.*
    version match {
      case TxVersion.V1 =>
        val transferBytes = transfers.map { case ParsedTransfer(recipient, amount) => Bytes.concat(recipient.bytes, Longs.toByteArray(amount.value)) }

        Bytes.concat(
          Array(tpe.id.toByte, version),
          sender.arr,
          assetId.byteRepr,
          Shorts.toByteArray(transfers.size.toShort),
          Bytes.concat(transferBytes*),
          Longs.toByteArray(timestamp),
          Longs.toByteArray(fee.value),
          Deser.serializeArrayWithLength(attachment.arr)
        )

      case _ =>
        PBTransactionSerializer.bodyBytes(tx)
    }
  }

  def toBytes(tx: MassTransferTransaction): Array[Byte] =
    if (PBSince.affects(tx)) PBTransactionSerializer.bytes(tx)
    else Bytes.concat(this.bodyBytes(tx), tx.proofs.bytes()) // No zero mark

  def parseBytes(bytes: Array[Byte]): Try[MassTransferTransaction] = Try {
    def parseTransfers(buf: ByteBuffer): Seq[MassTransferTransaction.ParsedTransfer] = {
      def readTransfer(buf: ByteBuffer): ParsedTransfer = {
        val addressOrAlias = buf.getAddressOrAlias(None)
        val amount         = TxNonNegativeAmount.unsafeFrom(buf.getLong)
        ParsedTransfer(addressOrAlias, amount)
      }

      val entryCount = buf.getShort
      require(entryCount >= 0 && buf.remaining() > entryCount, s"Broken array size ($entryCount entries while ${buf.remaining()} bytes available)")
      Vector.fill(entryCount)(readTransfer(buf))
    }

    val buf = ByteBuffer.wrap(bytes)
    require(buf.getByte == MassTransferTransaction.typeId && buf.getByte == TxVersion.V1, "transaction type mismatch")

    val sender     = buf.getPublicKey
    val assetId    = buf.getAsset
    val transfers  = parseTransfers(buf)
    val timestamp  = buf.getLong // Timestamp before fee
    val fee        = TxPositiveAmount.unsafeFrom(buf.getLong)
    val attachment = Deser.parseArrayWithLength(buf)
    val proofs     = buf.getProofs
    MassTransferTransaction(TxVersion.V1, sender, assetId, transfers, fee, timestamp, ByteStr(attachment), proofs, AddressScheme.current.chainId)
  }
}
