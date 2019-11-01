package com.wavesplatform.api

import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr

case class BlockMeta(header: BlockHeader, size: Int, transactionCount: Int, signature: ByteStr, height: Int)
