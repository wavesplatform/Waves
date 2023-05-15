package com.wavesplatform.ride.runner.input

import com.wavesplatform.account.Alias
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}
import play.api.libs.json.{JsString, JsSuccess}

class RideRunnerJsonTestSuite extends BaseTestSuite with HasTestAccounts {
  "RideRunnerJson" - {
    "aliasReads" - {
      val testAlias = Alias.create("test").explicitGet()

      "allows full format" in {
        parse(s"alias:$chainIdChar:test") shouldBe JsSuccess(testAlias)
      }

      "allows shortened format" in {
        parse("test") shouldBe JsSuccess(testAlias)
      }

      "checks chain id" in {
        parse("alias:W:test").isError shouldBe true
      }

      def parse(rawAlias: String) = RideRunnerJson.aliasReads.reads(JsString(rawAlias))
    }
  }
}
