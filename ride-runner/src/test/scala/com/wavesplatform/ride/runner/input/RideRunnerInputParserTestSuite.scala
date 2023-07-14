package com.wavesplatform.ride.runner.input

import com.google.protobuf.UnsafeByteOperations
import com.wavesplatform.account.Alias
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.ride.ScriptUtil
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.libs.json.{JsString, JsSuccess}

import java.nio.charset.StandardCharsets

class RideRunnerInputParserTestSuite extends BaseTestSuite with TableDrivenPropertyChecks with HasTestAccounts {
  "RideRunnerInputParser" - {
    "stringOrBytesReadsAsByteStr" - {
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

      def parse(rawContent: String) = RideRunnerInputParser.stringOrBytesAsByteStrReads.reads(JsString(rawContent))
    }

    "stringOrBytesReadsAsByteString" - {
      "allows Base58" in {
        val rawContent = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS"
        parse(s"base58:$rawContent") shouldBe JsSuccess(UnsafeByteOperations.unsafeWrap(Base58.decode(rawContent)))
      }

      "allows Base64" in {
        val rawContent = "dGVzdA=="
        parse(s"base64:$rawContent") shouldBe JsSuccess(UnsafeByteOperations.unsafeWrap(Base64.decode(rawContent)))
      }

      "string in UTF-8 by default" in {
        val rawContent = "test"
        parse(rawContent) shouldBe JsSuccess(UnsafeByteOperations.unsafeWrap(rawContent.getBytes(StandardCharsets.UTF_8)))
      }

      def parse(rawContent: String) = RideRunnerInputParser.stringOrBytesAsByteStringReads.reads(JsString(rawContent))
    }

    "aliasReads" - {
      val testAlias = Alias.create("test").explicitGet()

      "allows full format" in {
        parse(s"alias:$chainIdChar:test") shouldBe JsSuccess(testAlias)
      }

      "allows shortened format" in {
        parse("alias:test") shouldBe JsSuccess(testAlias)
      }

      "allows shortest format" in {
        parse("test") shouldBe JsSuccess(testAlias)
      }

      "checks chain id" in {
        parse("alias:W:test").isError shouldBe true
      }

      def parse(rawAlias: String) = RideRunnerInputParser.aliasReads.reads(JsString(rawAlias))
    }

    "addressOrAliasReads" - {
      val address   = aliceAddr
      val testAlias = Alias.create("test").explicitGet()

      "allows alias full format" in {
        parse(s"alias:$chainIdChar:test") shouldBe JsSuccess(testAlias)
      }

      "allows alias shortened format" in {
        parse("alias:test") shouldBe JsSuccess(testAlias)
      }

      "checks chain id" in {
        parse("alias:W:test").isError shouldBe true
      }

      "address by default" in {
        parse(address.toString) shouldBe JsSuccess(address)
      }

      "disallows alias without prefix" in {
        parse("test").isError shouldBe true
      }

      def parse(rawAlias: String) = RideRunnerInputParser.addressOrAliasReads.reads(JsString(rawAlias))
    }

    "scriptReads" - {
      val scriptSrc = s"""
{-#STDLIB_VERSION 6 #-}
{-#SCRIPT_TYPE ACCOUNT #-}
{-#CONTENT_TYPE DAPP #-}

@Callable(inv)
func foo() = {
let alice = Address(base58'$aliceAddr')
let x = getIntegerValue(alice, "x")
([], x + height)
}"""
      val script = ScriptUtil.from(scriptSrc)

      "allows bytes" in {
        parse(script.bytes().base64) shouldBe JsSuccess(script)
      }

      "source code by default" in {
        parse(scriptSrc) shouldBe JsSuccess(script)
      }

      def parse(rawContent: String) = RideRunnerInputParser.scriptReads.reads(JsString(rawContent))
    }
  }
}
