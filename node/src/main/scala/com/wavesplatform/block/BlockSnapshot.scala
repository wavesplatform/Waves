package com.wavesplatform.block

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.network.BlockSnapshotResponse
import com.wavesplatform.state.{StateSnapshot, TxMeta}

case class BlockSnapshot(blockId: BlockId, snapshots: Seq[(StateSnapshot, TxMeta.Status)])

object BlockSnapshot {
  def fromResponse(response: BlockSnapshotResponse): BlockSnapshot = ???
//    BlockSnapshot(response.blockId, response.snapshots.map(StateSnapshot.fromProtobuf))
}
