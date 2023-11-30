package com.wavesplatform.api

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block.protoHeaderHash
import com.wavesplatform.block.serialization.BlockHeaderSerializer
import com.wavesplatform.block.{Block, BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.block.PBBlocks
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

case class BlockMeta(
    header: BlockHeader,
    signature: ByteStr,
    headerHash: Option[ByteStr],
    height: Int,
    size: Int,
    transactionCount: Int,
    totalFeeInWaves: Long,
    reward: Option[Long],
    rewardShares: Seq[(Address, Long)],
    vrf: Option[ByteStr]
) {
  def toSignedHeader: SignedBlockHeader = SignedBlockHeader(header, signature)
  def id: ByteStr                       = headerHash.getOrElse(signature)

  val json: Coeval[JsObject] = Coeval.evalOnce {
    BlockHeaderSerializer.toJson(header, size, transactionCount, signature) ++
      Json.obj("height" -> height, "totalFee" -> totalFeeInWaves) ++
      reward.fold(Json.obj())(r =>
        Json.obj(
          "reward" -> r,
          "rewardShares" -> Json.obj(rewardShares.map[(String, Json.JsValueWrapper)] { case (addrName, reward) =>
            addrName.toString -> reward
          }*)
        )
      ) ++
      vrf.fold(Json.obj())(v => Json.obj("VRF" -> v.toString)) ++
      headerHash.fold(Json.obj())(h => Json.obj("id" -> h.toString))
  }
}

object BlockMeta {
  def fromBlock(block: Block, height: Int, totalFee: Long, reward: Option[Long], vrf: Option[ByteStr]): BlockMeta =
    BlockMeta(
      block.header,
      block.signature,
      if (block.header.version >= Block.ProtoBlockVersion) Some(protoHeaderHash(block.header)) else None,
      height,
      block.bytes().length,
      block.transactionData.length,
      totalFee,
      reward,
      Seq.empty,
      vrf
    )

  def fromPb(pbMeta: com.wavesplatform.database.protobuf.BlockMeta): Option[BlockMeta] = {
    pbMeta.header.map { pbHeader =>
      BlockMeta(
        PBBlocks.vanilla(pbHeader),
        pbMeta.signature.toByteStr,
        if (pbMeta.headerHash.isEmpty) None else Some(pbMeta.headerHash.toByteStr),
        pbMeta.height,
        pbMeta.size,
        pbMeta.transactionCount,
        pbMeta.totalFeeInWaves,
        Some(pbMeta.reward),
        Seq(),
        if (pbMeta.vrf.isEmpty) None
        else Some(pbMeta.vrf.toByteStr)
      )
    }
  }
}
