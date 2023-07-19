package com.wavesplatform.ride.runner.entrypoints

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import com.wavesplatform.ride.runner.input.RideRunnerInputParser
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}
import play.api.libs.json.JsSuccess

class RideRunnerWithPreparedStateTestSuite extends BaseTestSuite with HasTestAccounts {
  "RideRunnerWithPreparedState" in {
    val sampleInput = ConfigFactory.parseResources("sample-input.conf").resolve().root().render(ConfigRenderOptions.concise())
    val inputJson   = RideRunnerInputParser.parseJson(sampleInput)
    val r           = RideRunnerWithPreparedStateApp.run(inputJson)
    (r \ "result" \ "value" \ "_2" \ "value").validate[BigInt] shouldBe JsSuccess(BigInt("9007199361531056"))
  }
}
