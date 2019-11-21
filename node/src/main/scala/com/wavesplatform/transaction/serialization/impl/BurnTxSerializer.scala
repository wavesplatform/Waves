package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.serialization._
import com.wavesplatform.transaction.assets.BurnTransaction
import com.wavesplatform.transaction.{Proofs, TxVersion}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object BurnTxSerializer {
  def toJson(tx: BurnTransaction): JsObject = {
    import tx._
    BaseTxJson.toJson(tx) ++ Json.obj(
      "assetId" -> asset.id.toString,
      if (version < TxVersion.V3) "amount"  -> quantity else "quantity"  -> quantity,
    ) ++ (if (version >= TxVersion.V2) Json.obj("chainId" -> chainByte) else JsObject.empty)
  }

  def bodyBytes(tx: BurnTransaction): Array[Byte] = {
    import tx._
    lazy val baseBytes = Bytes.concat(
      sender,
      asset.id.arr,
      Longs.toByteArray(quantity),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    )

    version match {
      case TxVersion.V1 =>
        Bytes.concat(Array(typeId), baseBytes)

      case TxVersion.V2 =>
        Bytes.concat(
          Array(builder.typeId, version, chainByte.get),
          baseBytes
        )

      case _ =>
        PBTransactionSerializer.bodyBytes(tx)
    }
  }

  def toBytes(tx: BurnTransaction): Array[Byte] = {
    import tx._
    require(!isProtobufVersion, "Should be serialized with protobuf")
    version match {
      case TxVersion.V1 => Bytes.concat(this.bodyBytes(tx), proofs.toSignature)
      case TxVersion.V2 => Bytes.concat(Array(0: Byte), this.bodyBytes(tx), proofs.bytes())
    }
  }

  def parseBytes(bytes: Array[Byte]): Try[BurnTransaction] = Try {
    def parseCommonPart(version: TxVersion, buf: ByteBuffer): BurnTransaction = {
      val sender    = buf.getPublicKey
      val asset     = buf.getIssuedAsset
      val quantity  = buf.getLong
      val fee       = buf.getLong
      val timestamp = buf.getLong
      BurnTransaction(version, sender, asset, quantity, fee, timestamp, Nil)
    }

    require(bytes.length > 2, "buffer underflow while parsing transaction")

    if (bytes(0) == 0) {
      require(bytes(1) == BurnTransaction.typeId, "transaction type mismatch")
      val buf = ByteBuffer.wrap(bytes, 4, bytes.length - 4)
      parseCommonPart(TxVersion.V2, buf).copy(proofs = buf.getProofs)
    } else {
      require(bytes(0) == BurnTransaction.typeId, "transaction type mismatch")
      val buf = ByteBuffer.wrap(bytes, 1, bytes.length - 1)
      parseCommonPart(TxVersion.V1, buf).copy(proofs = Proofs(buf.getSignature))
    }
  }
}
