package com.wavesplatform.state

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr

case class BlockMinerInfo(baseTarget: Long, generationSignature: ByteStr, timestamp: Long, blockId: BlockId)
