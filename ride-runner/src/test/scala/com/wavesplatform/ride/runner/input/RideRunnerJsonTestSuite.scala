package com.wavesplatform.ride.runner.input

import com.wavesplatform.account.Alias
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}
import play.api.libs.json.{JsString, JsSuccess}

import java.nio.charset.StandardCharsets

class RideRunnerJsonTestSuite extends BaseTestSuite with HasTestAccounts {
  "RideRunnerJson" - {
    "bytesStrReads" - {
      "allows Base58" in {
        val rawContent = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS"
        parse(s"base58:$rawContent") shouldBe JsSuccess(ByteStr.decodeBase58(rawContent).get)
      }

      "allows Base64" in {
        val rawContent = "dGVzdA=="
        parse(s"base64:$rawContent") shouldBe JsSuccess(ByteStr.decodeBase64(rawContent).get)
      }

      "Base58 string by default" in {
        val rawContent = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS"
        parse(rawContent) shouldBe JsSuccess(ByteStr.decodeBase58(rawContent).get)
      }

      def parse(rawContent: String) = RideRunnerJson.byteStrReads.reads(JsString(rawContent))
    }

    "stringOrBytesReads" - {
      "allows Base58" in {
        val rawContent = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS"
        parse(s"base58:$rawContent") shouldBe JsSuccess(ByteStr.decodeBase58(rawContent).get)
      }

      "allows Base64" in {
        val rawContent = "dGVzdA=="
        parse(s"base64:$rawContent") shouldBe JsSuccess(ByteStr.decodeBase64(rawContent).get)
      }

      "string in UTF-8 by default" in {
        val rawContent = "test"
        parse(rawContent) shouldBe JsSuccess(ByteStr(rawContent.getBytes(StandardCharsets.UTF_8)))
      }

      def parse(rawContent: String) = RideRunnerJson.stringOrBytesReads.reads(JsString(rawContent))
    }

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
