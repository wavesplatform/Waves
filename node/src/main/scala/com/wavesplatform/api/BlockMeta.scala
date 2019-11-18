package com.wavesplatform.api

import com.wavesplatform.block.serialization.BlockHeaderSerializer
import com.wavesplatform.block.{BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import monix.eval.Coeval
import play.api.libs.json.Json

case class BlockMeta(
    header: BlockHeader,
    signature: ByteStr,
    height: Int,
    size: Int,
    transactionCount: Int,
    totalFeeInWaves: Long,
    reward: Option[Long]
) {
  def toSignedHeader: SignedBlockHeader = SignedBlockHeader(header, signature)

  val json = Coeval.evalOnce {
    BlockHeaderSerializer.toJson(header, size, transactionCount, signature) ++
      Json.obj("height" -> height, "totalFee" -> totalFeeInWaves) ++ reward.fold(Json.obj())(r => Json.obj("reward" -> r))
  }
}
