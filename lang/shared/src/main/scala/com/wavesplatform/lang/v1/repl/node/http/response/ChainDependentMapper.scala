package com.wavesplatform.lang.v1.repl.node.http.response

import java.nio.ByteBuffer

import com.wavesplatform.lang.v1.evaluator.ctx.impl.EnvironmentFunctions._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.lang.v1.repl.global
import com.wavesplatform.lang.v1.repl.node.http.response.model._
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
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
      tx.attachment.byteStr
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
      ByteStr(bodyBytes(tx)),
      tx.senderPublicKey.byteStr,
      tx.proofs.map(_.byteStr).toIndexedSeq
    )

  private def bodyBytes(tx: TransferTransaction): Array[Byte] =
    TransferTxSerializer.bodyBytes(
      tx.senderPublicKey.bytes,
      tx.assetId.map(_.bytes),
      tx.feeAssetId.map(_.bytes),
      tx.timestamp,
      tx.amount,
      tx.fee,
      tx.recipient,
      tx.attachment.bytes,
      tx.version,
      chainId,
      tx.proofs.map(_.bytes)
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
      timestamp = b.timestamp,
      height = b.height,
      baseTarget = b.`nxt-consensus`.`base-target`,
      generationSignature = b.`nxt-consensus`.`generation-signature`.byteStr,
      generator = b.generator.byteStr,
      generatorPublicKey = b.generatorPublicKey.byteStr,
      vrf = b.VRF.map(_.byteStr)
    )

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

  def addressFromString(addressStr: String): Either[String, Address] = {
    val base58String = if (addressStr.startsWith(AddressPrefix)) addressStr.drop(AddressPrefix.length) else addressStr
    for {
      _ <- Either.cond(
        base58String.length <= AddressStringLength,
        (),
        s"Wrong address string length: max=$AddressStringLength, actual: ${base58String.length}"
      )
      byteArray <- Base58.tryDecodeWithLimit(base58String).toEither.left.map(ex => s"Unable to decode base58: ${ex.getMessage}")
      address   <- addressFromBytes(byteArray)
    } yield address
  }

  private def addressFromBytes(addressBytes: Array[Byte]): Either[String, Address] = {
    val Array(version, network, _*) = addressBytes
    for {
      _ <- Either.cond(
        addressBytes.length == AddressLength,
        (),
        s"Wrong addressBytes length: expected: $AddressLength, actual: ${addressBytes.length}"
      )
      _ <- Either.cond(
        version == AddressVersion,
        (),
        s"Unknown address version: $version"
      )
      _ <- Either.cond(
        network == chainId,
        (),
        s"Data from other network: expected: $chainId(${chainId.toChar}), actual: $network(${network.toChar})"
      )
      checkSum          = addressBytes.takeRight(ChecksumLength)
      checkSumGenerated = global.secureHash(addressBytes.dropRight(ChecksumLength)).take(ChecksumLength)
      _ <- Either.cond(
        java.util.Arrays.equals(checkSum, checkSumGenerated),
        (),
        s"Bad address checksum"
      )
    } yield Address(ByteStr(addressBytes))
  }
}
