package com.wavesplatform.lang.v1.repl.node.http.response

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.repl.global
import com.wavesplatform.lang.v1.repl.node.http.response.model._
import com.wavesplatform.lang.v1.traits.domain.Recipient.{Address, Alias}
import com.wavesplatform.lang.v1.traits.domain.Tx.{Header, Proven, Transfer}
import com.wavesplatform.lang.v1.traits.domain._

private[node] class ChainDependentMapper(chainId: Byte) {
  def toRideModel(tx: TransferTransaction): Transfer =
    Transfer(
      proven(tx),
      tx.feeAssetId.map(_.byteStr),
      tx.assetId.map(_.byteStr),
      tx.amount,
      tx.recipient,
      (tx.attachment match {
        case ANothing => EmptyAttachment
        case AStr(v) => StringValue(v)
        case ABoolean(v) => BooleanValue(v)
        case ABytes(v) => ByteStrValue(v.byteStr)
        case AInt(v) => IntValue(v)
      })
    )

  def toRideModelO(tx: TransferTransaction): Option[Transfer] =
    if(tx.succeed) {
      Some(toRideModel(tx))
    } else {
      None
    }

  private def proven(tx: TransferTransaction): Proven =
    Proven(
      Header(tx.id.byteStr, tx.fee, tx.timestamp, tx.version),
      Address(pkToAddress(tx.senderPublicKey)),
      tx.bodyBytes.byteStr,
      tx.senderPublicKey.byteStr,
      tx.proofs.map(_.byteStr).toIndexedSeq
    )

  def toRideModel(a: AssetInfoResponse): ScriptAssetInfo =
    ScriptAssetInfo(
      a.assetId.byteStr,
      a.name,
      a.description,
      a.quantity,
      a.decimals,
      Address(a.issuer.byteStr),
      a.issuerPublicKey.byteStr,
      a.reissuable,
      a.scripted,
      a.minSponsoredAssetFee
    )

  def toRideModel(b: BlockInfoResponse): BlockInfo =
    BlockInfo(
      b.timestamp,
      b.height,
      b.`nxt-consensus`.`base-target`,
      b.`nxt-consensus`.`generation-signature`.byteStr,
      pkToAddress(b.generator),
      b.generator.byteStr,
      b.VRF.map(_.byteStr)
    )


  private val AddressVersion: Byte = 1
  private val AliasVersion  : Byte = 2

  private val ChecksumLength = 4
  private val HashLength     = 20
  private val AddressLength  = 1 + 1 + HashLength + ChecksumLength

  private def pkToAddress(publicKey: ByteString): ByteStr = {
    val withoutChecksum =
      ByteBuffer.allocate(1 + 1 + HashLength)
        .put(AddressVersion)
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
      tx.recipient match {
        case Address(bytes) => bytes.arr
        case Alias(name)    => AliasVersion +: chainId +: serializeArray(name.getBytes(StandardCharsets.UTF_8))
      },
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
