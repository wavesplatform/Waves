package com.wavesplatform.http
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.ApiError.ScriptExecutionError
import com.wavesplatform.api.http.utils.{UtilsApiRoute, UtilsInvocationRequest}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.history.DefaultBlockchainSettings
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.serialization.SerdeV1
import com.wavesplatform.state.{Blockchain, IntegerDataEntry, LeaseBalance}
import com.wavesplatform.test.DomainPresets.{RideV5, RideV6}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.utils.{Schedulers, Time}
import io.netty.util.HashedWheelTimer
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.Inside
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks as PropertyChecks
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}

import scala.concurrent.duration.DurationInt

class UtilsRouteEvaluateSpec
    extends RouteSpec("/utils")
    with RestAPISettingsHelper
    with PropertyChecks
    with PathMockFactory
    with Inside
    with WithDomain {
  private val utilsApi: UtilsApiRoute = UtilsApiRoute(
    new Time {
      def correctedTime(): Long = System.currentTimeMillis()
      def getTimestamp(): Long  = System.currentTimeMillis()
    },
    restAPISettings,
    Int.MaxValue,
    () => ScriptEstimatorV3(true, false),
    Schedulers.timeBoundedFixedPool(
      new HashedWheelTimer(),
      5.seconds,
      1,
      "rest-time-limited"
    ),
    stub[Blockchain]("globalBlockchain")
  )

  routePath("/script/evaluate/{address}") - {
    val letFromContract = 1000
    val testScript = TxHelpers.scriptV5(
      s"""
         |let letFromContract = $letFromContract
         |
         |func test(i: Int) = i * 10
         |func testB() = true
         |func testBS() = base58'MATCHER'
         |func testS() = "Test"
         |func testF() = throw("Test")
         |func testCompl() = ${"sigVerify(base58'', base58'', base58'') ||" * 200} true
         |func testThis() = this
         |func testListArg(list: List[String|ByteVector|Int], str: String, bytes: ByteVector) = list.containsElement(str)
         |
         |func nestedCalls(x: List[(String, String, List[Any])]) = {
         |  func call(a: String, x: (String, String, List[Any])) = {
         |    let (dAppAddress, funcName, args) = x
         |    strict res = Address(fromBase58String(dAppAddress)).invoke(funcName, args, [])
         |    a + res.exactAs[String] + "\n"
         |  }
         |  FOLD<20>(x, "", call)
         |}
         |
         |@Callable(i)
         |func getValue() = ([], "value")
         |
         |@Callable(i)
         |func testCallable() = [BinaryEntry("test", i.caller.bytes)]
         |
         |@Callable(i)
         |func testSyncInvoke() = {
         |  strict r = invoke(this, "testCallable", [], [])
         |  [BinaryEntry("testSyncInvoke", i.caller.bytes)]
         |}
         |
         |@Callable(i)
         |func testSyncCallComplexityExcess() = {
         |  strict r = invoke(this, "testSyncCallComplexityExcess", [], [])
         |  []
         |}
         |
         |@Callable(i)
         |func testWriteEntryType(b: ByteVector) = [ BinaryEntry("bytes", b) ]
       """.stripMargin
    )

    val dAppAccount = TxHelpers.defaultSigner
    val dAppAddress = TxHelpers.defaultSigner.toAddress

    def evalScript(text: String, address: Address = dAppAddress) =
      Post(routePath(s"/script/evaluate/$address"), Json.obj("expr" -> text))

    def evalBin(expr: EXPR) = {
      val serialized = ByteStr(SerdeV1.serialize(expr))
      Post(routePath(s"/script/evaluate/$dAppAddress"), Json.obj("expr" -> serialized.toString))
    }

    def responseJson: JsObject = {
      val fullJson = responseAs[JsObject]
      (fullJson \ "address").as[String] shouldBe dAppAddress.toString
      (fullJson \ "expr").as[String] should not be empty
      (fullJson \ "result").asOpt[JsObject].getOrElse(fullJson - "address" - "expr")
    }

    "simple expression" in {
      withDomain(RideV5) { d =>
        val blockchain = d.blockchain
        val api        = utilsApi.copy(blockchain = blockchain, settings = utilsApi.settings.copy(evaluateScriptComplexityLimit = 26000))
        val route      = seal(api.route)

        evalScript("testNone()") ~> route ~> check {
          responseAs[JsObject] shouldBe Json.obj("error" -> 199, "message" -> s"Address $dAppAddress is not dApp")
        }

        d.helpers.creditWavesToDefaultSigner()
        d.helpers.setScript(dAppAccount, testScript)

        evalScript("testListArg([\"test\", 111, base64'dGVzdA==', false], \"test\", base58'aaa')") ~> route ~> check {
          responseJson shouldBe Json.obj("type" -> "Boolean", "value" -> true)
        }

        evalScript("testCallable()") ~> route ~> check {
          val result = responseAs[JsValue]
          (result \ "result").as[JsObject] should matchJson(
            """
              |{
              |    "type" : "Array",
              |    "value" : [ {
              |      "type" : "BinaryEntry",
              |      "value" : {
              |        "key" : {
              |          "type" : "String",
              |          "value" : "test"
              |        },
              |        "value" : {
              |          "type" : "ByteVector",
              |          "value" : "11111111111111111111111111"
              |        }
              |      }
              |    } ]
              |  }
            """.stripMargin
          )
          (result \ "complexity").as[Int] shouldBe 5
          (result \ "expr").as[String] shouldBe "testCallable()"
          (result \ "address").as[String] shouldBe "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
          (result \ "stateChanges").as[JsObject] shouldBe Json.parse(
            """
              |{
              |    "data" : [ {"key":"test","type":"binary","value":"base64:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="} ],
              |    "transfers" : [ ],
              |    "issues" : [ ],
              |    "reissues" : [ ],
              |    "burns" : [ ],
              |    "sponsorFees" : [ ],
              |    "leases" : [ ],
              |    "leaseCancels" : [ ],
              |    "invokes" : [ ]
              |  }
            """.stripMargin
          )
        }

        evalScript("testThis()") ~> route ~> check {
          responseJson shouldBe Json.obj(
            "type"  -> "Address",
            "value" -> Json.obj("bytes" -> Json.obj("type" -> "ByteVector", "value" -> "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"))
          )
        }

        evalScript("testNone()") ~> route ~> check {
          responseJson shouldBe Json.obj(
            "error"   -> 306,
            "message" -> "InvokeRejectError(error = Function or type 'testNone' not found, log = \n\ttestNone.@args = []\n)"
          )
        }

        evalScript("testCompl()") ~> route ~> check {
          responseJson shouldBe Json.obj(
            "error" -> 306,
            "message" -> "InvokeRejectError(error = Calculation complexity limit exceeded, log = \n\ttestCompl.@args = []\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 25800\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 25599\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 25398\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 25197\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 24996\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 24795\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 24594\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 24393\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 24192\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 23991\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 23790\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 23589\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 23388\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 23187\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 22986\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 22785\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 22584\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 22383\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 22182\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 21981\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 21780\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 21579\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 21378\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 21177\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 20976\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 20775\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 20574\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 20373\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 20172\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 19971\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 19770\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 19569\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 19368\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 19167\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 18966\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 18765\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 18564\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 18363\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 18162\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 17961\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 17760\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 17559\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 17358\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 17157\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 16956\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 16755\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 16554\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 16353\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 16152\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 15951\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 15750\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 15549\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 15348\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 15147\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 14946\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 14745\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 14544\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 14343\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 14142\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 13941\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 13740\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 13539\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 13338\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 13137\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 12936\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 12735\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 12534\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 12333\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 12132\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 11931\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 11730\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 11529\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 11328\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 11127\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 10926\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 10725\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 10524\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 10323\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 10122\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 9921\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 9720\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 9519\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 9318\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 9117\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 8916\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 8715\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 8514\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 8313\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 8112\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 7911\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 7710\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 7509\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 7308\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 7107\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 6906\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 6705\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 6504\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 6303\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 6102\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 5901\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 5700\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 5499\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 5298\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 5097\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 4896\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 4695\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 4494\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 4293\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 4092\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 3891\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 3690\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 3489\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 3288\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 3087\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 2886\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 2685\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 2484\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 2283\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 2082\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 1881\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 1680\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 1479\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 1278\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 1077\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 876\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 675\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 474\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 273\n\tsigVerify.@args = [\n\t\tbase58'',\n\t\tbase58'',\n\t\tbase58''\n\t]\n\tsigVerify.@complexity = 200\n\t@complexityLimit = 72\n)"
          )
        }

        evalScript("testF()") ~> route ~> check {
          responseJson shouldBe Json.obj(
            "error" -> 306,
            "message" -> "InvokeRejectError(error = Test, log = \n\ttestF.@args = []\n\tthrow.@args = [\n\t\t\"Test\"\n\t]\n\tthrow.@complexity = 1\n\t@complexityLimit = 25999\n)"
          )
        }

        evalScript("test(123)") ~> route ~> check {
          responseJson shouldBe Json.obj("type" -> "Int", "value" -> 1230)
        }

        evalBin(FUNCTION_CALL(FunctionHeader.User("test"), List(CONST_LONG(123)))) ~> route ~> check {
          responseJson shouldBe Json.obj("type" -> "Int", "value" -> 1230)
        }

        evalScript("testS()") ~> route ~> check {
          responseJson shouldBe Json.obj("type" -> "String", "value" -> "Test")
        }

        evalScript("testB()") ~> route ~> check {
          responseJson shouldBe Json.obj("type" -> "Boolean", "value" -> true)
        }

        evalScript("testBS()") ~> route ~> check {
          responseJson shouldBe Json.obj("type" -> "ByteVector", "value" -> "MATCHER")
        }

        evalScript("""match test(123) {
                     |  case i: Int => i * 123
                     |  case _ => throw("")
                     |}""".stripMargin) ~> route ~> check {
          responseJson shouldBe Json.obj("type" -> "Int", "value" -> 151290)
        }

        val expectingKey   = "some"
        val expectingValue = 1234

        d.helpers.setData(dAppAccount, IntegerDataEntry(expectingKey, expectingValue))
        evalScript(
          s"""
             | this.getInteger("$expectingKey") == $expectingValue &&
             | height == height
            """.stripMargin
        ) ~> route ~> check {
          responseJson shouldBe Json.obj("type" -> "Boolean", "value" -> true)
        }

        evalScript("letFromContract - 1") ~> route ~> check {
          responseJson shouldBe Json.obj("type" -> "Int", "value" -> (letFromContract - 1))
        }

        (() => utilsApi.blockchain.settings)
          .when()
          .returning(DefaultBlockchainSettings)
          .anyNumberOfTimes()
        (utilsApi.blockchain.leaseBalance _)
          .when(*)
          .returning(LeaseBalance.empty)
          .anyNumberOfTimes()

        evalScript(""" testSyncInvoke() """) ~> route ~> check {
          val result = responseAs[JsObject]
          (result - "stateChanges") should matchJson(
            """
              |{
              |  "result" : {
              |    "type" : "Array",
              |    "value" : [ {
              |      "type" : "BinaryEntry",
              |      "value" : {
              |        "key" : {
              |          "type" : "String",
              |          "value" : "testSyncInvoke"
              |        },
              |        "value" : {
              |          "type" : "ByteVector",
              |          "value" : "11111111111111111111111111"
              |        }
              |      }
              |    } ]
              |  },
              |  "complexity" : 92,
              |  "expr" : " testSyncInvoke() ",
              |  "address" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
              |}
            """.stripMargin
          )
          (result \ "stateChanges").as[JsObject] shouldBe Json.parse(
            """
              |{
              |  "data" : [ {
              |    "key" : "testSyncInvoke",
              |    "type" : "binary",
              |    "value" : "base64:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
              |  } ],
              |  "transfers" : [ ],
              |  "issues" : [ ],
              |  "reissues" : [ ],
              |  "burns" : [ ],
              |  "sponsorFees" : [ ],
              |  "leases" : [ ],
              |  "leaseCancels" : [ ],
              |  "invokes" : [ {
              |    "dApp" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
              |    "call" : {
              |      "function" : "testCallable",
              |      "args" : [ ]
              |    },
              |    "payment" : [ ],
              |    "stateChanges" : {
              |      "data" : [ {
              |        "key" : "test",
              |        "type" : "binary",
              |        "value" : "base64:AVQv1P2H4On4q9JvwDzjIpknHO4wLHCiOl4="
              |      } ],
              |      "transfers" : [ ],
              |      "issues" : [ ],
              |      "reissues" : [ ],
              |      "burns" : [ ],
              |      "sponsorFees" : [ ],
              |      "leases" : [ ],
              |      "leaseCancels" : [ ],
              |      "invokes" : [ ]
              |    }
              |  } ]
              |}
            """.stripMargin
          )
        }

        val complexityLimit = 200
        val customApi       = api.copy(settings = restAPISettings.copy(evaluateScriptComplexityLimit = complexityLimit))
        evalScript(""" testSyncCallComplexityExcess() """) ~> customApi.route ~> check {
          val response = responseAs[JsValue]
          val message =
            "InvokeRejectError(error = FailedTransactionError(code = 1, error = Invoke complexity limit = 200 is exceeded), log = \n\ttestSyncCallComplexityExcess.@args = []\n\tinvoke.@args = [\n\t\tAddress(\n\t\t\tbytes = base58'3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9'\n\t\t),\n\t\t\"testSyncCallComplexityExcess\",\n\t\t[],\n\t\t[]\n\t]\n\tinvoke.@complexity = 75\n\t@complexityLimit = 122\n\tr = FailedTransactionError(code = 1, error = Invoke complexity limit = 200 is exceeded, log = \n\t\t@invokedDApp = Address(\n\t\t\tbytes = base58'3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9'\n\t\t)\n\t\t@invokedFuncName = \"testSyncCallComplexityExcess\"\n\t\ti = Invocation(\n\t\t\toriginCaller = Address(\n\t\t\t\tbytes = base58'3MuPKL2kQz1Gp9t7QwrDZN5F8m3u5Uzzo3e'\n\t\t\t)\n\t\t\tpayments = []\n\t\t\tcallerPublicKey = base58'9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ'\n\t\t\tfeeAssetId = Unit\n\t\t\toriginCallerPublicKey = base58'11111111111111111111111111111111'\n\t\t\ttransactionId = base58''\n\t\t\tcaller = Address(\n\t\t\t\tbytes = base58'3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9'\n\t\t\t)\n\t\t\tfee = 0\n\t\t)\n\t\ttestSyncCallComplexityExcess.@args = []\n\t\tinvoke.@args = [\n\t\t\tAddress(\n\t\t\t\tbytes = base58'3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9'\n\t\t\t),\n\t\t\t\"testSyncCallComplexityExcess\",\n\t\t\t[],\n\t\t\t[]\n\t\t]\n\t\tinvoke.@complexity = 75\n\t\t@complexityLimit = 44\n\t\tr = FailedTransactionError(code = 1, error = Invoke complexity limit = 200 is exceeded, log = \n\t\t\t@invokedDApp = Address(\n\t\t\t\tbytes = base58'3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9'\n\t\t\t)\n\t\t\t@invokedFuncName = \"testSyncCallComplexityExcess\"\n\t\t\ti = Invocation(\n\t\t\t\toriginCaller = Address(\n\t\t\t\t\tbytes = base58'3MuPKL2kQz1Gp9t7QwrDZN5F8m3u5Uzzo3e'\n\t\t\t\t)\n\t\t\t\tpayments = []\n\t\t\t\tcallerPublicKey = base58'9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ'\n\t\t\t\tfeeAssetId = Unit\n\t\t\t\toriginCallerPublicKey = base58'11111111111111111111111111111111'\n\t\t\t\ttransactionId = base58''\n\t\t\t\tcaller = Address(\n\t\t\t\t\tbytes = base58'3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9'\n\t\t\t\t)\n\t\t\t\tfee = 0\n\t\t\t)\n\t\t\ttestSyncCallComplexityExcess.@args = []\n\t\t)\n\t)\n)"
          (response \ "message").as[String] shouldBe message
          (response \ "error").as[Int] shouldBe ScriptExecutionError.Id
        }

        evalScript(""" testWriteEntryType("abc") """) ~> route ~> check {
          (responseAs[JsObject] - "stateChanges") should matchJson(
            """{"error":306,"message":"InvokeRejectError(error = Passed args (bytes, abc) are unsuitable for constructor BinaryEntry(String, ByteVector), log = \n\ttestWriteEntryType.@args = [\n\t\t\"abc\"\n\t]\n\tb = \"abc\"\n\tBinaryEntry.@args = [\n\t\t\"bytes\",\n\t\t\"abc\"\n\t]\n)","expr":" testWriteEntryType(\"abc\") ","address":"3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"}"""
          )
        }
        evalScript(""" testWriteEntryType(base58'aaaa') """) ~> route ~> check {
          (responseAs[JsObject] - "stateChanges") should matchJson(
            """{"result":{"type":"Array","value":[{"type":"BinaryEntry","value":{"key":{"type":"String","value":"bytes"},"value":{"type":"ByteVector","value":"aaaa"}}}]},"complexity":3,"expr":" testWriteEntryType(base58'aaaa') ","address":"3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"}"""
          )
        }

        evalScript(s"""parseBigIntValue("${PureContext.BigIntMax}")""") ~> route ~> check {
          (responseAs[JsObject] - "stateChanges") should matchJson(
            s"""{"result":{"type":"BigInt","value":${PureContext.BigIntMax}},"complexity":65,"expr":"parseBigIntValue(\\"${PureContext.BigIntMax}\\")","address":"3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"}"""
          )
        }

        val dAppAccount2 = TxHelpers.secondSigner
        val dAppAddress2 = TxHelpers.secondAddress
        d.helpers.creditWavesFromDefaultSigner(dAppAddress2)
        val testScript2 = TxHelpers.scriptV5(s"""
                                                |@Callable(i)
                                                |func callable() = {
                                                |  strict a = sigVerify(base58'', base58'', base58'')
                                                |  strict r = Address(base58'$dAppAddress').invoke("testCallable", [], [AttachedPayment(unit, 100)])
                                                |  [BinaryEntry("testSyncInvoke", i.caller.bytes)]
                                                |}
                                              """.stripMargin)
        d.helpers.setScript(dAppAccount2, testScript2)

        evalScript(""" callable() """, dAppAddress2) ~> route ~> check {
          (responseAs[JsObject] - "stateChanges") should matchJson(
            """{"result":{"type":"Array","value":[{"type":"BinaryEntry","value":{"key":{"type":"String","value":"testSyncInvoke"},"value":{"type":"ByteVector","value":"11111111111111111111111111"}}}]},"complexity":297,"expr":" callable() ","address":"3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"}"""
          )
        }

        val dApp2 = TxHelpers.scriptV5(
          s"""
             | @Callable(i)
             | func call(a: Int, b: String) = ([], b + a.toString())
            """.stripMargin
        )
        d.helpers.setScript(dAppAccount2, dApp2)

        evalScript(s"nestedCalls([(\"$dAppAddress2\", \"call\", [123, \"abc\"]), (\"$dAppAddress\", \"getValue\", [])])") ~> route ~> check {
          (responseAs[JsValue] \ "result" \ "value").as[String] shouldBe "abc123\nvalue\n"
        }
      }
    }

    "compacted dApp" in {
      withDomain(RideV6) { d =>
        val blockchain = d.blockchain
        val route      = utilsApi.copy(blockchain = blockchain).route

        val compactedDApp = TestCompiler(V6).compileContract(
          """
            | func user1() = 1
            | func user2() = 2
            |
            | @Callable(i)
            | func call() = ([], user1() + user2())
          """.stripMargin,
          compact = true
        )
        d.helpers.setScript(dAppAccount, compactedDApp)

        evalScript("user1()") ~> route ~> check {
          (responseAs[JsValue] \ "result" \ "value").as[Int] shouldBe 1
        }
        evalScript("user2()") ~> route ~> check {
          (responseAs[JsValue] \ "result" \ "value").as[Int] shouldBe 2
        }
        evalScript("call()") ~> route ~> check {
          (responseAs[JsValue] \ "result" \ "value" \ "_2" \ "value").as[Int] shouldBe 3
        }
      }
    }

    "invocation" - {
      withDomain(RideV6) { d =>
        def dApp(caller: Address) = TestCompiler(V6).compileContract(
          s"""
             | @Callable(i)
             | func f(arg1: Int, arg2: String) = {
             |   let check =
             |     this                    == Address(base58'$defaultAddress')            &&
             |     i.caller                == Address(base58'$caller')                    &&
             |     i.originCaller          == Address(base58'$caller')                    &&
             |     i.callerPublicKey       == base58'${secondSigner.publicKey}'           &&
             |     i.originCallerPublicKey == base58'${secondSigner.publicKey}'           &&
             |     i.fee                   == 123456                                      &&
             |     i.payments              == [AttachedPayment(unit, 1)]                  &&
             |     i.transactionId         == base58'3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8' &&
             |     i.feeAssetId            == base58'abcd'                                &&
             |     arg1 == 123 && arg2 == "abc"
             |   if (check) then [] else throw("wrong")
             | }
             |
             | @Callable(i)
             | func default() = {
             |   let check =
             |     this                    == Address(base58'$defaultAddress')            &&
             |     i.caller                == Address(base58'${"1" * 26}')                &&
             |     i.originCaller          == Address(base58'${"1" * 26}')                &&
             |     i.callerPublicKey       == base58'${"1" * 32}'                         &&
             |     i.originCallerPublicKey == base58'${"1" * 32}'                         &&
             |     i.fee                   == 500000                                      &&
             |     i.payments              == []                                          &&
             |     i.transactionId         == base58'${"1" * 32}'                         &&
             |     i.feeAssetId            == unit
             |   if (check) then [] else throw("wrong")
             | }
           """.stripMargin
        )
        d.appendBlock(setScript(defaultSigner, dApp(caller = secondAddress)))

        val route = utilsApi.copy(blockchain = d.blockchain).route
        def invocation(senderAddress: Option[Address] = Some(secondAddress)) =
          Json
            .parse(
              s"""
                 | {
                 |  "call": {
                 |    "function": "f",
                 |    "args": [
                 |      {
                 |        "type": "integer",
                 |        "value": 123
                 |      },
                 |      {
                 |        "type": "string",
                 |        "value": "abc"
                 |      }
                 |    ]
                 |  },
                 |  "id": "3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8",
                 |  "fee": 123456,
                 |  "feeAssetId": "abcd",
                 |  ${senderAddress.fold("")(a => s""""sender": "$a",""")}
                 |  "senderPublicKey": "${secondSigner.publicKey}",
                 |  "payment": [
                 |    {
                 |      "amount": 1,
                 |      "assetId": null
                 |    }
                 |  ]
                 | }
               """.stripMargin
            )
            .as[JsObject]

        val expectedTrace =
          Json.parse(
            s"""
               | [
               |  {
               |    "name": "f.@args",
               |    "type": "Array",
               |    "value": [
               |      {
               |        "type": "Int",
               |        "value": 123
               |      },
               |      {
               |        "type": "String",
               |        "value": "abc"
               |      }
               |    ]
               |  },
               |  {
               |    "name": "Address.@args",
               |    "type": "Array",
               |    "value": [
               |      {
               |        "type": "ByteVector",
               |        "value": "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
               |      }
               |    ]
               |  },
               |  {
               |    "name": "Address.@complexity",
               |    "type": "Int",
               |    "value": 1
               |  },
               |  {
               |    "name": "@complexityLimit",
               |    "type": "Int",
               |    "value": 51999
               |  },
               |  {
               |    "name": "==.@args",
               |    "type": "Array",
               |    "value": [
               |      {
               |        "type": "Address",
               |        "value": {
               |          "bytes": {
               |            "type": "ByteVector",
               |            "value": "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
               |          }
               |        }
               |      },
               |      {
               |        "type": "Address",
               |        "value": {
               |          "bytes": {
               |            "type": "ByteVector",
               |            "value": "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
               |          }
               |        }
               |      }
               |    ]
               |  },
               |  {
               |    "name": "==.@complexity",
               |    "type": "Int",
               |    "value": 1
               |  },
               |  {
               |    "name": "@complexityLimit",
               |    "type": "Int",
               |    "value": 51998
               |  },
               |  {
               |    "name": "i",
               |    "type": "Invocation",
               |    "value": {
               |      "originCaller": {
               |        "type": "Address",
               |        "value": {
               |          "bytes": {
               |            "type": "ByteVector",
               |            "value": "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"
               |          }
               |        }
               |      },
               |      "payments": {
               |        "type": "Array",
               |        "value": [
               |          {
               |            "type": "AttachedPayment",
               |            "value": {
               |              "amount": {
               |                "type": "Int",
               |                "value": 1
               |              },
               |              "assetId": {
               |                "type": "Unit",
               |                "value": {}
               |              }
               |            }
               |          }
               |        ]
               |      },
               |      "callerPublicKey": {
               |        "type": "ByteVector",
               |        "value": "8h47fXqSctZ6sb3q6Sst9qH1UNzR5fjez2eEP6BvEfcr"
               |      },
               |      "feeAssetId": {
               |        "type": "ByteVector",
               |        "value": "abcd"
               |      },
               |      "originCallerPublicKey": {
               |        "type": "ByteVector",
               |        "value": "8h47fXqSctZ6sb3q6Sst9qH1UNzR5fjez2eEP6BvEfcr"
               |      },
               |      "transactionId": {
               |        "type": "ByteVector",
               |        "value": "3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8"
               |      },
               |      "caller": {
               |        "type": "Address",
               |        "value": {
               |          "bytes": {
               |            "type": "ByteVector",
               |            "value": "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"
               |          }
               |        }
               |      },
               |      "fee": {
               |        "type": "Int",
               |        "value": 123456
               |      }
               |    }
               |  },
               |  {
               |    "name": "Address.@args",
               |    "type": "Array",
               |    "value": [
               |      {
               |        "type": "ByteVector",
               |        "value": "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"
               |      }
               |    ]
               |  },
               |  {
               |    "name": "Address.@complexity",
               |    "type": "Int",
               |    "value": 1
               |  },
               |  {
               |    "name": "@complexityLimit",
               |    "type": "Int",
               |    "value": 51997
               |  },
               |  {
               |    "name": "==.@args",
               |    "type": "Array",
               |    "value": [
               |      {
               |        "type": "Address",
               |        "value": {
               |          "bytes": {
               |            "type": "ByteVector",
               |            "value": "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"
               |          }
               |        }
               |      },
               |      {
               |        "type": "Address",
               |        "value": {
               |          "bytes": {
               |            "type": "ByteVector",
               |            "value": "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"
               |          }
               |        }
               |      }
               |    ]
               |  },
               |  {
               |    "name": "==.@complexity",
               |    "type": "Int",
               |    "value": 1
               |  },
               |  {
               |    "name": "@complexityLimit",
               |    "type": "Int",
               |    "value": 51996
               |  },
               |  {
               |    "name": "Address.@args",
               |    "type": "Array",
               |    "value": [
               |      {
               |        "type": "ByteVector",
               |        "value": "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"
               |      }
               |    ]
               |  },
               |  {
               |    "name": "Address.@complexity",
               |    "type": "Int",
               |    "value": 1
               |  },
               |  {
               |    "name": "@complexityLimit",
               |    "type": "Int",
               |    "value": 51995
               |  },
               |  {
               |    "name": "==.@args",
               |    "type": "Array",
               |    "value": [
               |      {
               |        "type": "Address",
               |        "value": {
               |          "bytes": {
               |            "type": "ByteVector",
               |            "value": "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"
               |          }
               |        }
               |      },
               |      {
               |        "type": "Address",
               |        "value": {
               |          "bytes": {
               |            "type": "ByteVector",
               |            "value": "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"
               |          }
               |        }
               |      }
               |    ]
               |  },
               |  {
               |    "name": "==.@complexity",
               |    "type": "Int",
               |    "value": 1
               |  },
               |  {
               |    "name": "@complexityLimit",
               |    "type": "Int",
               |    "value": 51994
               |  },
               |  {
               |    "name": "==.@args",
               |    "type": "Array",
               |    "value": [
               |      {
               |        "type": "ByteVector",
               |        "value": "8h47fXqSctZ6sb3q6Sst9qH1UNzR5fjez2eEP6BvEfcr"
               |      },
               |      {
               |        "type": "ByteVector",
               |        "value": "8h47fXqSctZ6sb3q6Sst9qH1UNzR5fjez2eEP6BvEfcr"
               |      }
               |    ]
               |  },
               |  {
               |    "name": "==.@complexity",
               |    "type": "Int",
               |    "value": 1
               |  },
               |  {
               |    "name": "@complexityLimit",
               |    "type": "Int",
               |    "value": 51993
               |  },
               |  {
               |    "name": "==.@args",
               |    "type": "Array",
               |    "value": [
               |      {
               |        "type": "ByteVector",
               |        "value": "8h47fXqSctZ6sb3q6Sst9qH1UNzR5fjez2eEP6BvEfcr"
               |      },
               |      {
               |        "type": "ByteVector",
               |        "value": "8h47fXqSctZ6sb3q6Sst9qH1UNzR5fjez2eEP6BvEfcr"
               |      }
               |    ]
               |  },
               |  {
               |    "name": "==.@complexity",
               |    "type": "Int",
               |    "value": 1
               |  },
               |  {
               |    "name": "@complexityLimit",
               |    "type": "Int",
               |    "value": 51992
               |  },
               |  {
               |    "name": "==.@args",
               |    "type": "Array",
               |    "value": [
               |      {
               |        "type": "Int",
               |        "value": 123456
               |      },
               |      {
               |        "type": "Int",
               |        "value": 123456
               |      }
               |    ]
               |  },
               |  {
               |    "name": "==.@complexity",
               |    "type": "Int",
               |    "value": 1
               |  },
               |  {
               |    "name": "@complexityLimit",
               |    "type": "Int",
               |    "value": 51991
               |  },
               |  {
               |    "name": "AttachedPayment.@args",
               |    "type": "Array",
               |    "value": [
               |      {
               |        "type": "Unit",
               |        "value": {}
               |      },
               |      {
               |        "type": "Int",
               |        "value": 1
               |      }
               |    ]
               |  },
               |  {
               |    "name": "AttachedPayment.@complexity",
               |    "type": "Int",
               |    "value": 1
               |  },
               |  {
               |    "name": "@complexityLimit",
               |    "type": "Int",
               |    "value": 51990
               |  },
               |  {
               |    "name": "cons.@args",
               |    "type": "Array",
               |    "value": [
               |      {
               |        "type": "AttachedPayment",
               |        "value": {
               |          "assetId": {
               |            "type": "Unit",
               |            "value": {}
               |          },
               |          "amount": {
               |            "type": "Int",
               |            "value": 1
               |          }
               |        }
               |      },
               |      {
               |        "type": "Array",
               |        "value": []
               |      }
               |    ]
               |  },
               |  {
               |    "name": "cons.@complexity",
               |    "type": "Int",
               |    "value": 1
               |  },
               |  {
               |    "name": "@complexityLimit",
               |    "type": "Int",
               |    "value": 51989
               |  },
               |  {
               |    "name": "==.@args",
               |    "type": "Array",
               |    "value": [
               |      {
               |        "type": "Array",
               |        "value": [
               |          {
               |            "type": "AttachedPayment",
               |            "value": {
               |              "amount": {
               |                "type": "Int",
               |                "value": 1
               |              },
               |              "assetId": {
               |                "type": "Unit",
               |                "value": {}
               |              }
               |            }
               |          }
               |        ]
               |      },
               |      {
               |        "type": "Array",
               |        "value": [
               |          {
               |            "type": "AttachedPayment",
               |            "value": {
               |              "assetId": {
               |                "type": "Unit",
               |                "value": {}
               |              },
               |              "amount": {
               |                "type": "Int",
               |                "value": 1
               |              }
               |            }
               |          }
               |        ]
               |      }
               |    ]
               |  },
               |  {
               |    "name": "==.@complexity",
               |    "type": "Int",
               |    "value": 1
               |  },
               |  {
               |    "name": "@complexityLimit",
               |    "type": "Int",
               |    "value": 51988
               |  },
               |  {
               |    "name": "==.@args",
               |    "type": "Array",
               |    "value": [
               |      {
               |        "type": "ByteVector",
               |        "value": "3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8"
               |      },
               |      {
               |        "type": "ByteVector",
               |        "value": "3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8"
               |      }
               |    ]
               |  },
               |  {
               |    "name": "==.@complexity",
               |    "type": "Int",
               |    "value": 1
               |  },
               |  {
               |    "name": "@complexityLimit",
               |    "type": "Int",
               |    "value": 51987
               |  },
               |  {
               |    "name": "==.@args",
               |    "type": "Array",
               |    "value": [
               |      {
               |        "type": "ByteVector",
               |        "value": "abcd"
               |      },
               |      {
               |        "type": "ByteVector",
               |        "value": "abcd"
               |      }
               |    ]
               |  },
               |  {
               |    "name": "==.@complexity",
               |    "type": "Int",
               |    "value": 1
               |  },
               |  {
               |    "name": "@complexityLimit",
               |    "type": "Int",
               |    "value": 51986
               |  },
               |  {
               |    "name": "arg1",
               |    "type": "Int",
               |    "value": 123
               |  },
               |  {
               |    "name": "==.@args",
               |    "type": "Array",
               |    "value": [
               |      {
               |        "type": "Int",
               |        "value": 123
               |      },
               |      {
               |        "type": "Int",
               |        "value": 123
               |      }
               |    ]
               |  },
               |  {
               |    "name": "==.@complexity",
               |    "type": "Int",
               |    "value": 1
               |  },
               |  {
               |    "name": "@complexityLimit",
               |    "type": "Int",
               |    "value": 51985
               |  },
               |  {
               |    "name": "arg2",
               |    "type": "String",
               |    "value": "abc"
               |  },
               |  {
               |    "name": "==.@args",
               |    "type": "Array",
               |    "value": [
               |      {
               |        "type": "String",
               |        "value": "abc"
               |      },
               |      {
               |        "type": "String",
               |        "value": "abc"
               |      }
               |    ]
               |  },
               |  {
               |    "name": "==.@complexity",
               |    "type": "Int",
               |    "value": 1
               |  },
               |  {
               |    "name": "@complexityLimit",
               |    "type": "Int",
               |    "value": 51984
               |  },
               |  {
               |    "name": "check",
               |    "type": "Boolean",
               |    "value": true
               |  }
               |]
             """.stripMargin
          )

        "successful result" in {
          Post(routePath(s"/script/evaluate/$defaultAddress"), invocation()) ~> route ~> check {
            val json = responseAs[JsValue]
            (json \ "complexity").as[Int] shouldBe 16
            (json \ "result").as[JsObject] shouldBe Json.obj("type" -> "Array", "value" -> JsArray())
            (json \ "vars").isEmpty shouldBe true
            json.as[UtilsInvocationRequest] shouldBe invocation().as[UtilsInvocationRequest]
          }
        }

        "trace" in {
          Post(routePath(s"/script/evaluate/$defaultAddress?trace=true"), invocation()) ~> route ~> check {
            val json = responseAs[JsValue]
            (json \ "complexity").as[Int] shouldBe 16
            (json \ "result").as[JsObject] shouldBe Json.obj("type" -> "Array", "value" -> JsArray())
            (json \ "vars").as[JsArray] shouldBe expectedTrace
          }
        }

        "all fields are empty (empty request body)" in {
          Post(routePath(s"/script/evaluate/$defaultAddress"), Json.obj()) ~> route ~> check {
            val json = responseAs[JsValue]
            (json \ "complexity").as[Int] shouldBe 12
            (json \ "result").as[JsObject] shouldBe Json.obj("type" -> "Array", "value" -> JsArray())
          }
        }

        "conflicting request structure" in {
          Post(routePath(s"/script/evaluate/$defaultAddress"), Json.obj("expr" -> "true") ++ invocation()) ~> route ~> check {
            val json = responseAs[JsValue]
            (json \ "message").as[String] shouldBe "Conflicting request structure. Both expression and invocation structure were sent"
            (json \ "error").as[Int] shouldBe 198
          }
        }

        "sender address can be calculated from PK" in {
          Post(routePath(s"/script/evaluate/$defaultAddress"), invocation(None)) ~> route ~> check {
            val json = responseAs[JsValue]
            (json \ "complexity").as[Int] shouldBe 16
            (json \ "result").as[JsObject] shouldBe Json.obj("type" -> "Array", "value" -> JsArray())
            (json \ "vars").isEmpty shouldBe true
            json.as[UtilsInvocationRequest] shouldBe invocation(None).as[UtilsInvocationRequest]
          }
        }

        "sender address can differ from PK address" in withDomain(RideV6) { d =>
          val customSender = signer(2).toAddress
          val route        = utilsApi.copy(blockchain = d.blockchain).route
          d.appendBlock(setScript(defaultSigner, dApp(caller = customSender)))

          Post(routePath(s"/script/evaluate/$defaultAddress"), invocation(Some(customSender))) ~> route ~> check {
            val json = responseAs[JsValue]
            (json \ "complexity").as[Int] shouldBe 16
            (json \ "result").as[JsObject] shouldBe Json.obj("type" -> "Array", "value" -> JsArray())
            (json \ "vars").isEmpty shouldBe true
            json.as[UtilsInvocationRequest] shouldBe invocation(Some(customSender)).as[UtilsInvocationRequest]
          }
        }
      }
    }
  }
}
