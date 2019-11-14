package com.wavesplatform.api

import com.wavesplatform.block.serialization.BlockHeaderSerializer
import com.wavesplatform.block.{BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

case class BlockMeta(header: BlockHeader, size: Int, transactionCount: Int, signature: ByteStr, height: Int) {
  def toSignedHeader: SignedBlockHeader = SignedBlockHeader(header, signature)

  val json = Coeval.evalOnce {
    BlockHeaderSerializer.toJson(header, size, transactionCount, signature) ++ Json.obj("height" -> height)
  }
}

object BlockMeta {
  def json(size: Int, transactionCount: Int, signature: ByteStr): JsObject =
    Json.obj("signature" -> signature.toString, "blocksize" -> size, "transactionCount" -> transactionCount, "totalFee" -> 0)
}
