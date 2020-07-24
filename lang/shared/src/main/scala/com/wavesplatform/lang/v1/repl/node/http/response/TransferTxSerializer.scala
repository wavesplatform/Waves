package com.wavesplatform.lang.v1.repl.node.http.response

import com.google.common.base.Charsets
import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.google.protobuf.ByteString
import com.wavesplatform.lang.v1.evaluator.ctx.impl.EnvironmentFunctions
import com.wavesplatform.lang.v1.traits.domain.Recipient.{Address, Alias}
import com.wavesplatform.lang.v1.traits.domain.{Recipient => LangRecipient}
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction._
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.protobuf.utils.PBUtils

object TransferTxSerializer {
  private val typeId: Byte = 4

  def bodyBytes(
      sender: Array[Byte],
      assetId: Option[Array[Byte]],
      feeAssetId: Option[Array[Byte]],
      timestamp: Long,
      amount: Long,
      fee: Long,
      recipient: LangRecipient,
      attachment: Array[Byte],
      version: Byte,
      chainId: Byte,
      proofs: List[Array[Byte]]
  ): Array[Byte] = {
    val baseBytes =
      Bytes.concat(
        sender,
        assetId.fold(Array(0: Byte))(bytes => (1: Byte) +: bytes),
        feeAssetId.fold(Array(0: Byte))(bytes => (1: Byte) +: bytes),
        Longs.toByteArray(timestamp),
        Longs.toByteArray(amount),
        Longs.toByteArray(fee),
        toBytes(recipient, chainId),
        serializeArrayWithLength(attachment)
      )

    version match {
      case 1 => Bytes.concat(Array(typeId), baseBytes)
      case 2 => Bytes.concat(Array(typeId, version), baseBytes)
      case _ =>
        protobufBytes(sender, chainId, amount, assetId.map(b => b), fee, feeAssetId.map(b => b), timestamp, version, proofs.map(p => p), attachment, recipient)
    }
  }

  private def toBytes(recipient: LangRecipient, chainId: Byte): Array[Byte] =
    recipient match {
      case Address(bytes) => bytes.arr
      case Alias(name)    => Array(EnvironmentFunctions.AliasVersion, chainId) ++ serializeArrayWithLength(name.getBytes(Charsets.UTF_8))
    }

  private def toProtobufRecipient(recipient: LangRecipient): Recipient =
    recipient match {
      case Address(bytes) => Recipient().withPublicKeyHash(ByteString.copyFrom(publicKeyHash(bytes.arr)))
      case Alias(name)    => Recipient().withAlias(name)
    }

  private def publicKeyHash(arr: Array[Byte]): Array[Byte] =
    arr.slice(2, arr.length - EnvironmentFunctions.ChecksumLength)

  private def protobufBytes(
     sender: ByteString,
     chainId: Byte,
     amount: Long,
     assetId: Option[ByteString],
     fee: Long ,
     feeAssetId: Option[ByteString],
     timestamp: Long,
     version: Int,
     proofs: Seq[ByteString],
     attachment: ByteString,
     recipient: LangRecipient
  ): Array[Byte] = {
    val pbAmount    = Amount().withAmount(amount)
    val assetAmount = assetId.fold(pbAmount)(pbAmount.withAssetId)
    val data = Data.Transfer(TransferTransactionData(Some(toProtobufRecipient(recipient)), Some(assetAmount), attachment))
    val feeAmount      = Amount().withAmount(fee)
    val feeAssetAmount = feeAssetId.fold(feeAmount)(feeAmount.withAssetId)
    val transaction =
      new SignedTransaction(
        Some(Transaction(chainId, sender, Some(feeAssetAmount), timestamp, version, data)),
        proofs
      )
    PBUtils.encodeDeterministic(transaction.getTransaction)
  }

  private implicit def byteString(array: Array[Byte]): ByteString =
    ByteString.copyFrom(array)

  private def serializeArrayWithLength(b: Array[Byte]): Array[Byte] = {
    val length = b.length
    if (length.isValidShort)
      Bytes.concat(Shorts.toByteArray(length.toShort), b)
    else
      throw new IllegalArgumentException(s"Attempting to serialize array with size, but the size($length) exceeds MaxShort(${Short.MaxValue})")
  }
}
