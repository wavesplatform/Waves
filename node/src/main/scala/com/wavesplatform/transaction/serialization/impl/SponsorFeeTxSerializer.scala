package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.serialization.ByteBufferOps
import com.wavesplatform.transaction.{PBSince, TxPositiveAmount, TxVersion}
import com.wavesplatform.transaction.assets.SponsorFeeTransaction
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object SponsorFeeTxSerializer {
  def toJson(tx: SponsorFeeTransaction): JsObject = {
    import tx._
    BaseTxJson.toJson(tx) ++ Json.obj(
      "assetId"              -> asset.id.toString,
      "minSponsoredAssetFee" -> minSponsoredAssetFee.map(_.value)
    )
  }

  def bodyBytes(tx: SponsorFeeTransaction): Array[Byte] = {
    import tx._
    version match {
      case TxVersion.V1 =>
        Bytes.concat(
          Array(tpe.id.toByte, version),
          sender.arr,
          asset.id.arr,
          Longs.toByteArray(minSponsoredAssetFee.map(_.value).getOrElse(0)),
          Longs.toByteArray(fee.value),
          Longs.toByteArray(timestamp)
        )

      case _ =>
        PBTransactionSerializer.bodyBytes(tx)
    }
  }

  def toBytes(tx: SponsorFeeTransaction): Array[Byte] = {
    if (PBSince.affects(tx)) PBTransactionSerializer.bytes(tx)
    else Bytes.concat(Array(0: Byte, tx.tpe.id.toByte, tx.version), this.bodyBytes(tx), tx.proofs.bytes()) // [typeId, version] appears twice
  }

  def parseBytes(bytes: Array[Byte]): Try[SponsorFeeTransaction] = Try {
    require(bytes.length > 2, "buffer underflow while parsing transaction")

    val buf = ByteBuffer.wrap(bytes)
    require(buf.getByte == 0 && buf.getByte == SponsorFeeTransaction.typeId && buf.getByte == TxVersion.V1, "transaction type mismatch")
    require(buf.getByte == SponsorFeeTransaction.typeId && buf.getByte == TxVersion.V1, "transaction type mismatch")

    val sender               = buf.getPublicKey
    val asset                = buf.getIssuedAsset
    val minSponsoredAssetFee = Some(buf.getLong).filter(_ != 0).map(TxPositiveAmount.unsafeFrom)
    val fee                  = TxPositiveAmount.unsafeFrom(buf.getLong)
    val timestamp            = buf.getLong
    val proofs               = buf.getProofs
    SponsorFeeTransaction(TxVersion.V1, sender, asset, minSponsoredAssetFee, fee, timestamp, proofs, AddressScheme.current.chainId)
  }
}
