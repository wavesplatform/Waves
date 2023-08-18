package com.wavesplatform.ride.runner.input

import cats.syntax.option.*
import com.softwaremill.diffx.generic.auto.*
import com.softwaremill.diffx.scalatest.DiffShouldMatcher.*
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, AddressOrAlias, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.ride.runner.input.RideRunnerInputParser.*
import com.wavesplatform.ride.{DiffXInstances, ScriptUtil}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxNonNegativeAmount
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}
import net.ceedubs.ficus.Ficus.toFicusConfig
import net.ceedubs.ficus.readers.ValueReader
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.libs.json.*

import java.nio.charset.StandardCharsets
import scala.util.{Success, Try}

class RideRunnerInputParserTestSuite extends BaseTestSuite with TableDrivenPropertyChecks with HasTestAccounts with DiffXInstances {
  private val btc  = IssuedAsset(ByteStr.decodeBase58("8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS").get)
  private val txId = ByteStr.decodeBase58("8rc5Asw43qbq7LMZ6tu2aVbVkw72XmBt7tTnwMSNfaNq").get

  "RideRunnerInputParser" - {
    "JsValue" in {
      forAll(
        Table(
          "rawContent"         -> "expected",
          "1"                  -> JsNumber(1),
          "true"               -> JsBoolean(true),
          """ "foo" """        -> JsString("foo"),
          "null"               -> JsNull,
          "[1,2]"              -> JsArray(Seq(JsNumber(1), JsNumber(2))),
          """ { "foo": 1 } """ -> Json.obj("foo" -> 1)
        )
      ) { (rawContent, expected) =>
        parseAs[JsValue](rawContent) shouldBe expected
      }
    }

    "JsObject" in {
      parseAs[JsObject](""" { "foo": 1 } """) shouldBe Json.obj("foo" -> 1)
      Try(parseAs[JsObject]("1")).isFailure shouldBe true
    }

    "StringOrBytesAsByteArray" - {
      "allows Base58" in {
        val rawContent = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS"
        parse(s"base58:$rawContent") shouldBe Base58.decode(rawContent)
      }

      "allows Base64" in {
        val rawContent = "dGVzdA=="
        parse(s"base64:$rawContent") shouldBe Base64.decode(rawContent)
      }

      "string in UTF-8 by default" in {
        val rawContent = "test"
        parse(rawContent) shouldBe rawContent.getBytes(StandardCharsets.UTF_8)
      }

      def parse(rawContent: String) = parseQuotedStringAs[StringOrBytesAsByteArray](rawContent)
    }

    "Alias" - {
      val testAlias = Alias.create("test").explicitGet()

      "allows full format" in {
        parse(s"alias:$chainIdChar:test") shouldBe Success(testAlias)
      }

      "allows shortened format" in {
        parse("alias:test") shouldBe Success(testAlias)
      }

      "allows shortest format" in {
        parse("test") shouldBe Success(testAlias)
      }

      "checks chain id" in {
        parse("alias:W:test").isFailure shouldBe true
      }

      def parse(rawAlias: String) = Try(parseQuotedStringAs[Alias](rawAlias))
    }

    "AddressOrAlias" - {
      val address   = aliceAddr
      val testAlias = Alias.create("test").explicitGet()

      "allows alias full format" in {
        parse(s"alias:$chainIdChar:test") shouldBe Success(testAlias)
      }

      "allows alias shortened format" in {
        parse("alias:test") shouldBe Success(testAlias)
      }

      "checks chain id" in {
        parse("alias:W:test").isFailure shouldBe true
      }

      "address by default" in {
        parse(address.toString) shouldBe Success(address)
      }

      "disallows alias without prefix" in {
        parse("test").isFailure shouldBe true
      }

      def parse(rawAlias: String) = Try(parseQuotedStringAs[AddressOrAlias](rawAlias))
    }

    "Script" - {
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
        parse(script.bytes().base64) shouldBe Success(script)
      }

      "source code by default" in {
        parse(scriptSrc) shouldBe Success(script)
      }

      def parse(rawContent: String) = Try(parseQuotedStringAs[Script](rawContent))
    }

    "RideRunnerDataEntry" - {
      def parse(tpe: String, value: String) = parseAs[RideRunnerDataEntry](s"{ type = $tpe\nvalue = $value }")

      "Integer" in {
        List(
          Byte.MinValue,
          Byte.MaxValue,
          Short.MinValue,
          Short.MaxValue,
          Int.MinValue,
          Int.MaxValue,
          Long.MinValue,
          Long.MaxValue
        ).foreach { x => parse("integer", x.toString) shouldBe IntegerRideRunnerDataEntry(x) }

        Try(parse("integer", "xxx")).isFailure shouldBe true
      }

      "Boolean" in {
        parse("boolean", "true") shouldBe BooleanRideRunnerDataEntry(true)
        parse("boolean", "false") shouldBe BooleanRideRunnerDataEntry(false)
        Try(parse("boolean", "xxx")).isFailure shouldBe true
      }

      val fooBytes = "foo".getBytes(StandardCharsets.UTF_8)

      "String" in {
        List(
          "yes",
          Base64.encode(fooBytes),
          Base58.encode(fooBytes)
        ).foreach { s =>
          parse("string", s) shouldBe StringRideRunnerDataEntry(s)
        }
      }

      "Binary" in {
        List(
          s"base64:${Base64.encode(fooBytes)}",
          s"base58:${Base58.encode(fooBytes)}"
        ).foreach { s =>
          parse("binary", s"\"$s\"") shouldBe BinaryRideRunnerDataEntry(ByteStr(fooBytes))
        }
      }

      "wrong type" in {
        Try(parse("xxx", "1")).isFailure shouldBe true
      }
    }

    "RideRunnerPostProcessingMethod" - {
      def parse(tpe: String, valueKey: String, value: String) = parseAs[RideRunnerPostProcessingMethod](s"{ type = $tpe\n$valueKey = $value }")

      "PickRideRunnerPostProcessingMethod" in {
        parse("pick", "path", "foo.bar") shouldBe PickRideRunnerPostProcessingMethod("foo.bar")
        Try(parse("pick", "xxx", "foo.bar")).isFailure shouldBe true
      }

      "PickAllRideRunnerPostProcessingMethod" in {
        parse("pickAll", "paths", """[ "foo.bar", "baz"] """) shouldBe PickAllRideRunnerPostProcessingMethod(List("foo.bar", "baz"))
        Try(parse("pickAll", "xxx", "foo.bar")).isFailure shouldBe true
      }

      "PruneRideRunnerPostProcessingMethod" in {
        parse("prune", "paths", """[ "foo.bar", "baz"] """) shouldBe PruneRideRunnerPostProcessingMethod(List("foo.bar", "baz"))
        Try(parse("pickAll", "xxx", "foo.bar")).isFailure shouldBe true
      }

      "wrong type" in {
        Try(parse("xxx", "path", "foo.bar")).isFailure shouldBe true
      }
    }

    "file" in {
      val chainId   = 'N'.toByte
      val dAppAddr  = Address.fromString("3Kkqr1kzNhabCTNYQkyzSkztLzzwGtRcTFj", chainId.some).explicitGet()
      val aliceAddr = dAppAddr
      val bobAddr   = Address.fromString("3KXBq6p34jnawXBRY68jjjEr1duPKYmKard", chainId.some).explicitGet()

      val expected = RideRunnerInput(
        address = dAppAddr,
        request = Json.obj(
          "call" -> Json.obj(
            "function" -> "foo",
            "args" -> Json.arr(
              Json.obj(
                "type"  -> "integer",
                "value" -> 1
              )
            )
          ),
          "senderPublicKey" -> "2KwU4vzdgPmKyf7q354H9kSyX9NZjNiq4qbnH2wi2VDF",
          "payment" -> Json.arr(
            Json.obj("amount" -> 1)
          )
        ),
        chainId = chainId.toChar,
        intAsString = true,
        state = RideRunnerBlockchainState(
          height = 3296627,
          accounts = Map(
            bobAddr -> RideRunnerAccount(
              regularBalance = TxNonNegativeAmount.unsafeFrom(500001).some
            ),
            aliceAddr -> RideRunnerAccount(
              assetBalances = Map(btc -> TxNonNegativeAmount(1)),
              regularBalance = TxNonNegativeAmount.unsafeFrom(500100).some,
              leasing = RideRunnerLeaseBalance(in = TxNonNegativeAmount(10), out = TxNonNegativeAmount(100)).some,
              generatingBalance = TxNonNegativeAmount.unsafeFrom(100_000_000_000L).some,
              data = Map(
                "a" -> IntegerRideRunnerDataEntry(11),
                "b" -> BooleanRideRunnerDataEntry(true),
                "c" -> StringRideRunnerDataEntry("foobarbaz"),
                "d" -> BinaryRideRunnerDataEntry(ByteStr.decodeBase64("Zm9vYmFyb2Jheg==").get)
              ).some,
              aliases = List(Alias.create("carl").explicitGet()),
              scriptInfo = RideRunnerScriptInfo(
                script = ScriptUtil.from(
                  src = s"""
{-#STDLIB_VERSION 6 #-}
{-#SCRIPT_TYPE ACCOUNT #-}
{-#CONTENT_TYPE DAPP #-}
{-# IMPORT libs/main.ride #-}

@Callable(inv)
func foo(x: Int) = {
  let alice = Address(base58'$aliceAddr')
  let carl = addressFromRecipient(Alias("carl"))
  let bob = Address(base58'$bobAddr')

  let asset = base58'$btc'
  let txId = base58'$txId'

  # Functions
  let x1 = getIntegerValue(alice, "a")
  let x2 = 0 # if (isDataStorageUntouched(carl)) then 1 else 0 # Not supported
  let x3 = assetBalance(bob, asset)
  let x4 = value(assetInfo(asset)).decimals
  let x5 = value(blockInfoByHeight(3296627)).height
  let x6 = size(value(scriptHash(this)))
  let x7 = value(transactionHeightById(txId))
  let x8 = value(transferTransactionById(txId)).amount
  let x9 = wavesBalance(carl).available
  strict x10 = invoke(this, "bar", [], []).exactAs[Int]

  # Vals
  let y1 = height
  let y2 = lastBlock.height
  let y3 = size(inv.payments)

  ([ScriptTransfer(bob, 1, asset)], x + x1 + x2 + x3 + x4 + x5 +  x6 + x7 + x8 + x9 + x10 + y1 + y2 + y3 + 9007199254740991)
}""",
                  libraries = Map("libs/main.ride" -> s"""
{-# STDLIB_VERSION 6 #-}
{-# SCRIPT_TYPE ACCOUNT #-}
{-# CONTENT_TYPE LIBRARY #-}

@Callable(inv)
func bar () = {
  let x1 = if (valueOrElse(getBoolean("b"), false)) then 1 else 0
  (nil, x1)
}""")
                )
              ).some
            )
          ),
          assets = Map(
            btc -> RideRunnerAsset(
              name = StringOrBytesAsByteArray("Bitcoin".getBytes(StandardCharsets.UTF_8)),
              quantity = 21000000
            )
          ),
          blocks = Map(
            3296627 -> RideRunnerBlock(
              timestamp = 1663299568885L,
              generationSignature = ByteStr
                .decodeBase58(
                  "CrAWgWGpWcH81bZHchdJsjXe4ngh16dimZ7pjunt7nZvfrf1y3aV1vVZGuijf8CnBrVbmXsZcav4izfySsQDf8d9U43M37674bvwSv5gCB3cERewfwQNFTZhGXFh6dbMbvx"
                )
                .get,
              generatorPublicKey = PublicKey(ByteStr.decodeBase58("GtZHNJQGy7RDgP3Jym5mCTVDX3FUKDSesikLhLcyytG5").get),
              VRF = ByteStr.decodeBase58("GHC3DQuW9ncm5sNy5u3TVEF4CXu1fsLVHVHYxJzuZr7b").get.some
            )
          ),
          transactions = Map(
            txId -> RideRunnerTransaction(
              amount = 93119130,
              assetId = btc,
              fee = 100001,
              recipient = Address.fromString("3KZAW98tkMP6xxhWcYZ5fjZspTvsvo77paa", chainId.some).explicitGet(),
              senderPublicKey = PublicKey(ByteStr.decodeBase58("2KwU4vzdgPmKyf7q354H9kSyX9NZjNiq4qbnH2wi2VDF").get),
              height = 3281000.some,
              timestamp = 1663299600039L,
              proofs = List(
                StringOrBytesAsByteArray(Base58.decode("4m9DDG9ALcWheRjvL7iaGQzsW5iqocz1nSGVxbRYW74LLjPdUXpc6iYZ8NjKyNdCdfv6ePMveFZLv4iYnpu7BhfQ"))
              ),
              attachment = StringOrBytesAsByteArray("test".getBytes(StandardCharsets.UTF_8))
            )
          )
        ),
        postProcessing = List(PickRideRunnerPostProcessingMethod("result.value._2.value")),
        test = RideRunnerTest(expected = JsString("9007199361531057")).some
      )

      val actual = RideRunnerInputParser.from(RideRunnerInputParser.prepare(ConfigFactory.parseResources("sample-input.conf")))
      actual shouldMatchTo expected
    }
  }

  private def parseQuotedStringAs[T: ValueReader](s: String): T = ConfigFactory.parseString(s"""x = \"\"\"$s\"\"\"""").as[T]("x")
  private def parseAs[T: ValueReader](rawContent: String): T    = ConfigFactory.parseString(s"""x = $rawContent""").as[T]("x")
}
