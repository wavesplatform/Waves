package com.wavesplatform.block

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.state.{StateSnapshot, TxMeta}

case class BlockSnapshot(blockId: BlockId, snapshots: Seq[(StateSnapshot, TxMeta.Status)])
