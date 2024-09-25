package com.wavesplatform.lang.v1.repl.node.http.response

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import com.google.protobuf.ByteString
import com.wavesplatform.lang.v1.evaluator.ctx.impl.EnvironmentFunctions
import com.wavesplatform.lang.v1.traits.domain.Recipient.{Address, Alias}
import com.wavesplatform.lang.v1.traits.domain.{Recipient => LangRecipient}
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction._
import com.wavesplatform.protobuf.transaction.Transaction.Data

object TransferTxSerializer {
  import ByteString.{copyFrom => bs}

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
      sender ++
        assetId.fold(Array(0: Byte))(bytes => (1: Byte) +: bytes) ++
        feeAssetId.fold(Array(0: Byte))(bytes => (1: Byte) +: bytes) ++
        ByteBuffer.allocate(8).putLong(timestamp).array ++
        ByteBuffer.allocate(8).putLong(amount).array ++
        ByteBuffer.allocate(8).putLong(fee).array ++
        toBytes(recipient, chainId) ++
        serializeArrayWithLength(attachment)

    version match {
      case 1 => Array(typeId) ++ baseBytes
      case 2 => Array(typeId, version) ++ baseBytes
      case _ =>
        protobufBytes(
          bs(sender),
          chainId,
          amount,
          assetId.map(bs),
          fee,
          feeAssetId.map(bs),
          timestamp,
          version,
          proofs.map(bs),
          bs(attachment),
          recipient
        )
    }
  }

  private def toBytes(recipient: LangRecipient, chainId: Byte): Array[Byte] =
    recipient match {
      case Address(bytes) => bytes.arr
      case Alias(name)    => Array(EnvironmentFunctions.AliasVersion, chainId) ++ serializeArrayWithLength(name.getBytes(StandardCharsets.UTF_8))
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
      fee: Long,
      feeAssetId: Option[ByteString],
      timestamp: Long,
      version: Int,
      proofs: Seq[ByteString],
      attachment: ByteString,
      recipient: LangRecipient
  ): Array[Byte] = {
    val pbAmount       = Amount().withAmount(amount)
    val assetAmount    = assetId.fold(pbAmount)(pbAmount.withAssetId)
    val data           = Data.Transfer(TransferTransactionData(Some(toProtobufRecipient(recipient)), Some(assetAmount), attachment))
    val feeAmount      = Amount().withAmount(fee)
    val feeAssetAmount = feeAssetId.fold(feeAmount)(feeAmount.withAssetId)
    val transaction =
      new SignedTransaction(
        SignedTransaction.Transaction.WavesTransaction(Transaction(chainId, sender, Some(feeAssetAmount), timestamp, version, data)),
        proofs
      )
    transaction.getWavesTransaction.toByteArray
  }

  private def serializeArrayWithLength(b: Array[Byte]): Array[Byte] = {
    val length = b.length
    if (length.isValidShort)
      ByteBuffer.allocate(2).putShort(length.toShort).array ++ b
    else
      throw new IllegalArgumentException(s"Attempting to serialize array with size, but the size($length) exceeds MaxShort(${Short.MaxValue})")
  }
}
