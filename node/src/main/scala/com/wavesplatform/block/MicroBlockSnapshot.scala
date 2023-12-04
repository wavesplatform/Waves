package com.wavesplatform.block

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.state.{StateSnapshot, TxMeta}

case class MicroBlockSnapshot(totalBlockId: BlockId, snapshots: Seq[(StateSnapshot, TxMeta.Status)])
