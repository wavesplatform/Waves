package com.wavesplatform.ride

import com.wavesplatform.blockchain.{ScriptBlockchain, SharedBlockchainData}
import com.wavesplatform.ride.input.RunnerRequest
import com.wavesplatform.state.Blockchain
import play.api.libs.json.JsObject

class RideScript(val index: Int, blockchain: Blockchain, runnerRequest: RunnerRequest) {
  def run(): JsObject = executeUtilsEvaluate(
    blockchain,
    runnerRequest
  )
}

object RideScript {
  def apply(index: Int, blockchainStorage: SharedBlockchainData[Int], runnerRequest: RunnerRequest): RideScript =
    new RideScript(index, new ScriptBlockchain[Int](blockchainStorage, index), runnerRequest)
}
