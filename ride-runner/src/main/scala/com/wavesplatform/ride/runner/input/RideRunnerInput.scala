package com.wavesplatform.ride.runner.input

import com.wavesplatform.account.{Address, DefaultAddressScheme}
import play.api.libs.json.JsObject

case class RideRunnerInput(
    address: Address,
    request: JsObject,
    chainId: Char = DefaultAddressScheme.chainId.toChar,
    intAsString: Boolean = false,
    trace: Boolean = false,
    evaluateScriptComplexityLimit: Int = Int.MaxValue,
    maxTxErrorLogSize: Int = 1024,
    state: RideRunnerBlockchainState,
    postProcessing: RideRunnerPostProcessing = RideRunnerPostProcessing(),
    test: Option[RideRunnerTest] = None
)
