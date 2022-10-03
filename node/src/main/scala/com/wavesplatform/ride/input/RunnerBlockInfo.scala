package com.wavesplatform.ride.input

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr

case class RunnerBlockInfo(
    timestamp: Long = System.currentTimeMillis(),
    baseTarget: Long = 130,
    generationSignature: ByteStr = ByteStr(new Array[Byte](64)),
    generatorPublicKey: PublicKey = EmptyPublicKey,
    VRF: Option[ByteStr] = None
)
