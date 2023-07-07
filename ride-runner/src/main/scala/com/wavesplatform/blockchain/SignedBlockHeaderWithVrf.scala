package com.wavesplatform.blockchain

import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr

case class SignedBlockHeaderWithVrf(header: SignedBlockHeader, vrf: ByteStr, blockReward: Long)
