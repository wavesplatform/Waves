package com.wavesplatform.ride.input

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr

case class RunnerBlockInfo(
    timestamp: Long,
    baseTarget: Long,
    generationSignature: ByteStr,
    generatorPublicKey: PublicKey,
    VRF: Option[ByteStr] = None
)
