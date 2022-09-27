package com.wavesplatform.ride.input

import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr

case class RunnerBlockInfo(
    blockHeader: SignedBlockHeader,
    VRF: Option[ByteStr] = None
)
