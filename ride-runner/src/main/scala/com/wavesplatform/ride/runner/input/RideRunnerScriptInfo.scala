package com.wavesplatform.ride.runner.input

import com.wavesplatform.account.PublicKey
import com.wavesplatform.account.PublicKeys.EmptyPublicKey
import com.wavesplatform.lang.script.Script

case class RideRunnerScriptInfo(
    publicKey: PublicKey = EmptyPublicKey,
    script: Script
)
