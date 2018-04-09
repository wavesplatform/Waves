package com.wavesplatform.state

import scorex.block.Block.BlockId
import scorex.consensus.nxt.NxtLikeConsensusBlockData

case class BlockMinerInfo(consensus: NxtLikeConsensusBlockData, timestamp: Long, blockId: BlockId)
