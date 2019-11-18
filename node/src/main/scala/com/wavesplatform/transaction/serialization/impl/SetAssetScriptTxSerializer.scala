package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.serialization.{ByteBufferOps, Deser}
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.assets.SetAssetScriptTransaction
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object SetAssetScriptTxSerializer {
  def toJson(tx: SetAssetScriptTransaction): JsObject = {
    import tx._
    BaseTxJson.toJson(tx) ++ Json.obj(
      "assetId" -> asset.id.toString,
      "chainId" -> chainByte,
      "script"  -> script.map(_.bytes().base64)
    )
  }

  def bodyBytes(tx: SetAssetScriptTransaction): Array[Byte] = {
    import tx._
    version match {
      case TxVersion.V1 =>
        Bytes.concat(
          Array(builder.typeId, version, chainByte.get),
          sender,
          asset.id.arr,
          Longs.toByteArray(fee),
          Longs.toByteArray(timestamp),
          Deser.serializeOptionOfArrayWithLength(script)(s => s.bytes().arr)
        )

      case TxVersion.V2 =>
        PBTransactionSerializer.bodyBytes(tx)
    }
  }

  def toBytes(tx: SetAssetScriptTransaction): Array[Byte] = tx.version match {
    case TxVersion.V1 =>
      Bytes.concat(Array(0: Byte), this.bodyBytes(tx), tx.proofs.bytes())

    case TxVersion.V2 =>
      PBTransactionSerializer.toBytesPrefixed(tx)
  }

  def parseBytes(bytes: Array[Byte]): Try[SetAssetScriptTransaction] = Try {
    require(bytes.length > 2, "buffer underflow while parsing transaction")

    val buf = ByteBuffer.wrap(bytes)
    require(buf.getByte == 0 && buf.getByte == SetAssetScriptTransaction.typeId, "transaction type mismatch")

    buf.getByte match {
      case TxVersion.V1 =>
        require(buf.getByte == AddressScheme.current.chainId, "transaction chainId mismatch")
        val sender    = buf.getPublicKey
        val asset     = buf.getIssuedAsset
        val fee       = buf.getLong
        val timestamp = buf.getLong
        val script    = buf.getScript
        val proofs    = buf.getProofs
        SetAssetScriptTransaction(TxVersion.V1, sender, asset, script, fee, timestamp, proofs)

      case TxVersion.V2 =>
        PBTransactionSerializer.fromBytesAs(buf.getByteArray(buf.remaining()), SetAssetScriptTransaction)
    }
  }
}
