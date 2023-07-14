package com.wavesplatform.ride.runner.input

import com.wavesplatform.account.PublicKey
import com.wavesplatform.account.PublicKeys.EmptyPublicKey
import com.wavesplatform.common.state.ByteStr

case class RideRunnerBlock(
    timestamp: Long = System.currentTimeMillis(),
    baseTarget: Long = 130,
    generationSignature: ByteStr = ByteStr(new Array[Byte](64)),
    generatorPublicKey: PublicKey = EmptyPublicKey,
    VRF: Option[ByteStr] = None,
    blockReward: Long = 600_000_000L // 6 WAVES
)
