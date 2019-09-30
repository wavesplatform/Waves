package com.wavesplatform.lang.v1.repl.node.http.response

import java.nio.ByteBuffer

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.repl.global
import com.wavesplatform.lang.v1.repl.node.http.response.model.{AssetInfoResponse, BlockInfoResponse, ByteString, TransferTransaction, TransferTransactionV1, TransferTransactionV2}
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain.Tx.{Header, Proven, Transfer}
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, ScriptAssetInfo}

private[node] class ChainDependentMapper(chainId: Byte) {
  def toRideModel(tx: TransferTransaction): Transfer =
    Transfer(
      proven(tx),
      tx.feeAssetId.map(_.byteStr),
      tx.assetId.map(_.byteStr),
      tx.amount,
      Address(tx.recipient.byteStr),
      tx.attachment.byteStr
    )

  private def proven(tx: TransferTransaction): Proven =
    Proven(
      Header(tx.id.byteStr, tx.fee, tx.timestamp, tx.version),
      Address(pkToAddress(tx.senderPublicKey)),
      ByteStr(bodyBytes(tx)),
      tx.senderPublicKey.byteStr,
      tx.proofs.map(_.byteStr).toIndexedSeq
    )

  def toRideModel(a: AssetInfoResponse): ScriptAssetInfo =
    ScriptAssetInfo(
      a.assetId.byteStr,
      a.quantity,
      a.decimals,
      Address(a.issuer.byteStr),
      pkToAddress(a.issuer),
      a.reissuable,
      a.scripted,
      a.sponsored
    )

  def toRideModel(b: BlockInfoResponse): BlockInfo =
    BlockInfo(
      b.timestamp,
      b.height,
      b.`nxt-consensus`.`base-target`,
      b.`nxt-consensus`.`generation-signature`.byteStr,
      pkToAddress(b.generator),
      b.generator.byteStr
    )


  private val AddressVersion = 1
  private val ChecksumLength = 4
  private val HashLength     = 20
  private val AddressLength  = 1 + 1 + HashLength + ChecksumLength

  private def pkToAddress(publicKey: ByteString): ByteStr = {
    val withoutChecksum =
      ByteBuffer.allocate(1 + 1 + HashLength)
        .put(AddressVersion.toByte)
        .put(chainId)
        .put(global.secureHash(publicKey.bytes), 0, HashLength)
        .array()

    val checksum =
      global.secureHash(withoutChecksum).take(ChecksumLength)

    val bytes =
      ByteBuffer.allocate(AddressLength)
        .put(withoutChecksum)
        .put(checksum, 0, ChecksumLength)
        .array()

    ByteStr(bytes)
  }

  private val typeId: Byte = 4

  private def bodyBytes(tx: TransferTransaction): Array[Byte] =
    tx match {
      case _: TransferTransactionV1 => typeId +: bytesBase(tx)
      case _: TransferTransactionV2 => Array(typeId, tx.version) ++ bytesBase(tx)
    }

  private def bytesBase(tx: TransferTransaction): Array[Byte] =
    Seq(
      tx.senderPublicKey.bytes,
      bytes(tx.assetId),
      bytes(tx.feeAssetId),
      bytes(tx.timestamp),
      bytes(tx.amount),
      bytes(tx.fee),
      tx.recipient.bytes,
      serializeArray(tx.attachment.bytes)
    ).reduce(_ ++ _)

  private def bytes(id: Option[ByteString]): Array[Byte] =
    id.map(_.bytes).getOrElse(Array[Byte](0))

  private def bytes(l: Long): Array[Byte] =
    ByteBuffer.allocate(8).putLong(l).array()

  private def bytes(s: Short): Array[Byte] =
    ByteBuffer.allocate(2).putShort(s).array()

  private def serializeArray(b: Array[Byte]): Array[Byte] =
    bytes(b.length.toShort) ++ b
}
