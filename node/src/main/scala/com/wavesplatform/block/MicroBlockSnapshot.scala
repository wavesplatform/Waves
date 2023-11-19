package com.wavesplatform.block

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.network.MicroBlockSnapshotResponse
import com.wavesplatform.state.{StateSnapshot, TxMeta}

case class MicroBlockSnapshot(totalBlockId: BlockId, snapshots: Seq[(StateSnapshot, TxMeta.Status)])

object MicroBlockSnapshot {
  def fromResponse(response: MicroBlockSnapshotResponse): MicroBlockSnapshot = ???
//    MicroBlockSnapshot(response.totalBlockId, response.snapshots.map(StateSnapshot.fromProtobuf))
}
