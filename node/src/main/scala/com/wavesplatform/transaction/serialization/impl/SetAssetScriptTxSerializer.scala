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
    Bytes.concat(
      Array(builder.typeId, version, chainByte.get),
      sender,
      asset.id.arr,
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp),
      Deser.serializeOptionOfArrayWithLength(script)(s => s.bytes().arr)
    )
  }

  def toBytes(tx: SetAssetScriptTransaction): Array[Byte] = {
    import tx._
    Bytes.concat(Array(0: Byte), bodyBytes(tx), proofs.bytes())
  }

  def parseBytes(bytes: Array[Byte]): Try[SetAssetScriptTransaction] = Try {
    require(bytes.length > 2, "buffer underflow while parsing transaction")

    val buf = ByteBuffer.wrap(bytes)
    require(buf.getByte == 0 && buf.getByte == SetAssetScriptTransaction.typeId && buf.getByte == TxVersion.V1, "transaction type mismatch")
    require(buf.getByte == AddressScheme.current.chainId, "transaction chainId mismatch")

    val sender    = buf.getPublicKey
    val asset     = buf.getIssuedAsset
    val fee       = buf.getLong
    val timestamp = buf.getLong
    val script    = buf.getScript
    val proofs    = buf.getProofs
    SetAssetScriptTransaction(TxVersion.V1, sender, asset, script, fee, timestamp, proofs)
  }
}
