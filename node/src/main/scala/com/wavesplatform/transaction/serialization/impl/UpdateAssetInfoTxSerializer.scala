package com.wavesplatform.transaction.serialization.impl

import java.nio.charset.StandardCharsets

import com.wavesplatform.transaction.assets.UpdateAssetInfoTransaction
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object UpdateAssetInfoTxSerializer {
  def toJson(tx: UpdateAssetInfoTransaction): JsObject =
    BaseTxJson.toJson(tx) ++ Json.obj(
      "assetId"     -> tx.asset.id.toString,
      "name"        -> new String(tx.name, StandardCharsets.UTF_8),
      "description" -> new String(tx.description, StandardCharsets.UTF_8),
      "chainId"     -> tx.chainByte
    )

  def bodyBytes(tx: UpdateAssetInfoTransaction): Array[Byte] = PBTransactionSerializer.bodyBytes(tx)

  def toBytes(tx: UpdateAssetInfoTransaction): Array[Byte] = {
    require(!tx.isProtobufVersion, "Should be serialized with protobuf")
    Array.emptyByteArray
  }

  def parseBytes(bytes: Array[Byte]): Try[UpdateAssetInfoTransaction] = ???
}
