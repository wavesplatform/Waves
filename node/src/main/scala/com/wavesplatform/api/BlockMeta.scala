package com.wavesplatform.api

import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import play.api.libs.json.{JsObject, Json}

case class BlockMeta(header: BlockHeader, size: Int, transactionCount: Int, signature: ByteStr, height: Int) {
  val json = header.json.map {
    _ ++ BlockMeta.json(size, transactionCount, signature) ++ Json.obj("height" -> height)
  }
}

object BlockMeta {
  def json(size: Int, transactionCount: Int, signature: ByteStr): JsObject =
    Json.obj("signature" -> signature.toString, "blocksize" -> size, "transactionCount" -> transactionCount, "totalFee" -> 0)
}
