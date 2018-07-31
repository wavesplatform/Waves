package com.wavesplatform.state

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData

case class BlockMinerInfo(consensus: NxtLikeConsensusBlockData, timestamp: Long, blockId: BlockId)
