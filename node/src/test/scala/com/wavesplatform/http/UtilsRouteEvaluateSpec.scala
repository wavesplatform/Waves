package com.wavesplatform.http

import akka.http.scaladsl.model.headers.Accept
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.http.ApiError.ScriptExecutionError
import com.wavesplatform.api.http.CustomJson
import com.wavesplatform.api.http.utils.{UtilsApiRoute, UtilsInvocationRequest}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.DefaultBlockchainSettings
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.serialization.SerdeV1
import com.wavesplatform.state.{Blockchain, IntegerDataEntry, LeaseBalance}
import com.wavesplatform.test.DomainPresets.{RideV5, RideV6}
import com.wavesplatform.test.NumericExt
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.{Asset, AssetIdLength, TxHelpers}
import com.wavesplatform.utils.{Schedulers, Time}
import io.netty.util.HashedWheelTimer
import monix.execution.schedulers.SchedulerService
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.Inside
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks as PropertyChecks
import play.api.libs.json.*

import scala.concurrent.duration.DurationInt

class UtilsRouteEvaluateSpec
    extends RouteSpec("/utils")
    with RestAPISettingsHelper
    with PropertyChecks
    with PathMockFactory
    with Inside
    with WithDomain {
  private val timeBounded: SchedulerService = Schedulers.timeBoundedFixedPool(
    new HashedWheelTimer(),
    5.seconds,
    1,
    "rest-time-limited"
  )
  private val utilsApi: UtilsApiRoute = UtilsApiRoute(
    new Time {
      def correctedTime(): Long = System.currentTimeMillis()
      def getTimestamp(): Long  = System.currentTimeMillis()
    },
    restAPISettings,
    Int.MaxValue,
    () => ScriptEstimatorV3(true, false),
    timeBounded,
    stub[Blockchain]("globalBlockchain")
  )

  override def afterAll(): Unit = {
    timeBounded.shutdown()
    super.afterAll()
  }

  routePath("/script/evaluate/{address}") - {
    val letFromContract = 1000
    val testScript = TxHelpers.scriptV5(
      s"""
         |let letFromContract = $letFromContract
         |
         |func any(value: Any) = value
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
    val dAppAddress = dAppAccount.toAddress

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
      withDomain(RideV5, AddrWithBalance.enoughBalances(defaultSigner)) { d =>
        val blockchain = d.blockchain
        val api        = utilsApi.copy(blockchain = blockchain, settings = utilsApi.settings.copy(evaluateScriptComplexityLimit = 26000))
        val route      = seal(api.route)

        evalScript("testNone()") ~> route ~> check {
          responseAs[JsObject] shouldBe Json.obj("error" -> 199, "message" -> s"Address $dAppAddress is not dApp")
        }

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
              |          "value" : "3MuPKL2kQz1Gp9t7QwrDZN5F8m3u5Uzzo3e"
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
              |    "data" : [ {"key":"test","type":"binary","value":"base64:AVQ7/yyDy88aPjf+qhzZz/b3yDC2lXyJwWk="} ],
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
              |          "value" : "3MuPKL2kQz1Gp9t7QwrDZN5F8m3u5Uzzo3e"
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
              |    "value" : "base64:AVQ7/yyDy88aPjf+qhzZz/b3yDC2lXyJwWk="
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
            "InvokeRejectError(error = FailedTransactionError(code = 1, error = Invoke complexity limit = 200 is exceeded), log = \n\ttestSyncCallComplexityExcess.@args = []\n\tinvoke.@args = [\n\t\tAddress(\n\t\t\tbytes = base58'3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9'\n\t\t),\n\t\t\"testSyncCallComplexityExcess\",\n\t\t[],\n\t\t[]\n\t]\n\tinvoke.@complexity = 75\n\t@complexityLimit = 122\n\tr = FailedTransactionError(code = 1, error = Invoke complexity limit = 200 is exceeded, log = \n\t\t@invokedDApp = Address(\n\t\t\tbytes = base58'3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9'\n\t\t)\n\t\t@invokedFuncName = \"testSyncCallComplexityExcess\"\n\t\ti = Invocation(\n\t\t\toriginCaller = Address(\n\t\t\t\tbytes = base58'3MuPKL2kQz1Gp9t7QwrDZN5F8m3u5Uzzo3e'\n\t\t\t)\n\t\t\tpayments = []\n\t\t\tcallerPublicKey = base58'9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ'\n\t\t\tfeeAssetId = Unit\n\t\t\toriginCallerPublicKey = base58'11111111111111111111111111111111'\n\t\t\ttransactionId = base58''\n\t\t\tcaller = Address(\n\t\t\t\tbytes = base58'3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9'\n\t\t\t)\n\t\t\tfee = 2000000\n\t\t)\n\t\ttestSyncCallComplexityExcess.@args = []\n\t\tinvoke.@args = [\n\t\t\tAddress(\n\t\t\t\tbytes = base58'3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9'\n\t\t\t),\n\t\t\t\"testSyncCallComplexityExcess\",\n\t\t\t[],\n\t\t\t[]\n\t\t]\n\t\tinvoke.@complexity = 75\n\t\t@complexityLimit = 44\n\t\tr = FailedTransactionError(code = 1, error = Invoke complexity limit = 200 is exceeded, log = \n\t\t\t@invokedDApp = Address(\n\t\t\t\tbytes = base58'3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9'\n\t\t\t)\n\t\t\t@invokedFuncName = \"testSyncCallComplexityExcess\"\n\t\t\ti = Invocation(\n\t\t\t\toriginCaller = Address(\n\t\t\t\t\tbytes = base58'3MuPKL2kQz1Gp9t7QwrDZN5F8m3u5Uzzo3e'\n\t\t\t\t)\n\t\t\t\tpayments = []\n\t\t\t\tcallerPublicKey = base58'9BUoYQYq7K38mkk61q8aMH9kD9fKSVL1Fib7FbH6nUkQ'\n\t\t\t\tfeeAssetId = Unit\n\t\t\t\toriginCallerPublicKey = base58'11111111111111111111111111111111'\n\t\t\t\ttransactionId = base58''\n\t\t\t\tcaller = Address(\n\t\t\t\t\tbytes = base58'3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9'\n\t\t\t\t)\n\t\t\t\tfee = 2000000\n\t\t\t)\n\t\t\ttestSyncCallComplexityExcess.@args = []\n\t\t)\n\t)\n)"
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
        evalScript(s"""parseBigIntValue("${PureContext.BigIntMax}")""") ~> Accept(CustomJson.jsonWithNumbersAsStrings) ~> route ~> check {
          (responseAs[JsObject] - "stateChanges") should matchJson(s"""{
            "result":{
              "type":"BigInt",
              "value":"${PureContext.BigIntMax.toString()}"
            },
            "complexity":65,
            "expr":"parseBigIntValue(\\"${PureContext.BigIntMax}\\")",
            "address":"3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
            }""")
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
            """{"result":{"type":"Array","value":[{"type":"BinaryEntry","value":{"key":{"type":"String","value":"testSyncInvoke"},"value":{"type":"ByteVector","value":"3MuPKL2kQz1Gp9t7QwrDZN5F8m3u5Uzzo3e"}}}]},"complexity":297,"expr":" callable() ","address":"3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC"}"""
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

        evalScript(s"any(${Long.MaxValue})") ~> Accept(CustomJson.jsonWithNumbersAsStrings) ~> route ~> check {
          responseJson shouldBe Json.obj("type" -> "Int", "value" -> s"${Long.MaxValue}")
        }

        evalScript(s"any([${Long.MaxValue}, 0, ${Long.MinValue}])") ~> Accept(CustomJson.jsonWithNumbersAsStrings) ~> route ~> check {
          (responseJson \ "type").as[String] shouldBe "Array"
          (responseJson \ "value").get.toString shouldBe
            s"""[{"type":"Int","value":"${Long.MaxValue}"},{"type":"Int","value":"0"},{"type":"Int","value":"${Long.MinValue}"}]"""
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

    "invocation" in {
      withDomain(RideV6, AddrWithBalance.enoughBalances(defaultSigner, secondSigner, signer(2))) { d =>
        val route   = utilsApi.copy(blockchain = d.blockchain).route
        val issueTx = issue()
        val asset   = IssuedAsset(issueTx.id())
        d.appendBlock(issueTx, sponsor(asset, Some(1)), transfer(asset = asset, amount = 12345678))
        d.appendBlock(setScript(defaultSigner, dApp(caller = secondAddress, secondSigner.publicKey, asset)))

        // successful result
        Post(routePath(s"/script/evaluate/$defaultAddress"), invocation(asset)) ~> route ~> check {
          val json = responseAs[JsValue]
          (json \ "complexity").as[Int] shouldBe 16
          (json \ "result").as[JsObject] shouldBe Json.obj("type" -> "Array", "value" -> JsArray())
          (json \ "vars").isEmpty shouldBe true
          json.as[UtilsInvocationRequest] shouldBe invocation(asset).as[UtilsInvocationRequest]
        }

        // trace
        Post(routePath(s"/script/evaluate/$defaultAddress?trace=true"), invocation(asset)) ~> route ~> check {
          val json = responseAs[JsValue]
          (json \ "complexity").as[Int] shouldBe 16
          (json \ "result").as[JsObject] shouldBe Json.obj("type" -> "Array", "value" -> JsArray())
          (json \ "vars").as[JsArray] shouldBe expectedTrace(asset)
        }

        // all fields are empty (empty request body)
        Post(routePath(s"/script/evaluate/$defaultAddress"), Json.obj()) ~> route ~> check {
          val json = responseAs[JsValue]
          (json \ "complexity").as[Int] shouldBe 12
          (json \ "result").as[JsObject] shouldBe Json.obj("type" -> "Array", "value" -> JsArray())
        }

        // conflicting request structure
        Post(routePath(s"/script/evaluate/$defaultAddress"), Json.obj("expr" -> "true") ++ invocation(asset)) ~> route ~> check {
          val json = responseAs[JsValue]
          (json \ "message").as[String] shouldBe "Conflicting request structure. Both expression and invocation structure were sent"
          (json \ "error").as[Int] shouldBe 198
        }

        // wrong invocation format
        Post(routePath(s"/script/evaluate/$defaultAddress"), Json.obj("call" -> Json.obj("function" -> 1))) ~> route ~> check {
          val json = responseAs[JsValue]
          (json \ "message").as[String] shouldBe "failed to parse json message"
          (json \ "validationErrors") match {
            case JsDefined(validationErrors) =>
              validationErrors should matchJson("""{
                  "obj.call": [
                    {
                      "msg": ["Unexpected call function name format"],
                      "args": []
                    }
                  ]
                }""")
            case _: JsUndefined => fail("No validation errors")
          }
        }

        // sender address can be calculated from PK
        Post(routePath(s"/script/evaluate/$defaultAddress"), invocation(asset, None)) ~> route ~> check {
          val json = responseAs[JsValue]
          (json \ "complexity").as[Int] shouldBe 16
          (json \ "result").as[JsObject] shouldBe Json.obj("type" -> "Array", "value" -> JsArray())
          (json \ "vars").isEmpty shouldBe true
          json.as[UtilsInvocationRequest] shouldBe invocation(asset, None).as[UtilsInvocationRequest]
        }

        // invocation sender address can differ from PK address
        d.appendBlock(setScript(defaultSigner, dApp(caller = signer(2).toAddress, signer(2).publicKey, asset)))
        d.appendBlock(transfer(defaultSigner, signer(2).toAddress, 1234567, asset))
        val request = invocation(asset, Some(signer(2).toAddress), signer(2).publicKey)
        Post(routePath(s"/script/evaluate/$defaultAddress"), request) ~> route ~> check {
          val json = responseAs[JsValue]
          (json \ "complexity").as[Int] shouldBe 16
          (json \ "result").as[JsObject] shouldBe Json.obj("type" -> "Array", "value" -> JsArray())
          (json \ "vars").isEmpty shouldBe true
          json.as[UtilsInvocationRequest] shouldBe request.as[UtilsInvocationRequest]
        }
      }
    }

    "with state" - {
      val callerKeyPair = signer(1)
      val callerAddress = callerKeyPair.toAddress

      val scriptTransferReceiver        = signer(2)
      val scriptTransferReceiverAddress = scriptTransferReceiver.toAddress

      val assetIssuer        = signer(3)
      val assetIssuerAddress = assetIssuer.toAddress

      val dAppAccount = signer(4) // Should be different from default signer, because we're checking balances
      val dAppAddress = dAppAccount.toAddress

      val issueTx = issue(issuer = assetIssuer)
      val asset   = IssuedAsset(issueTx.id())

      def toRide(asset: Asset) = asset.fold("unit")(id => s"base58'$id'")

      def dAppWithTransfer(assetAndAmounts: (Asset, Long)*): Script = {
        def toScriptTransfer(asset: Asset, amount: Long): String = s"ScriptTransfer(receiver, $amount, ${toRide(asset)})"

        TestCompiler(V6).compileContract(
          s"""
             | @Callable(i)
             | func default() = {
             |   let receiver = Address(base58'$scriptTransferReceiverAddress')
             |   [ ${assetAndAmounts.map(Function.tupled(toScriptTransfer)).mkString(", ")} ]
             | }""".stripMargin
        )
      }

      def mkSetScriptTx(transferAssetAmounts: (Asset, Long)*) = setScript(dAppAccount, dAppWithTransfer(transferAssetAmounts*))

      "expr" - {
        def mkExprJson(state: JsObject): JsObject =
          Json
            .parse(
              s""" {
                 |  "expr": "default()",
                 |  "state": ${Json.prettyPrint(state)}
                 | }""".stripMargin
            )
            .as[JsObject]

        "negative" in {
          val exprTests = Table[Seq[(Asset, Long)]](
            "paymentAssetWithAmounts",
            Seq(Asset.Waves -> 3),
            Seq(asset       -> 3),
            Seq(Asset.Waves -> 3, asset -> 3)
          )

          forAll(exprTests) { paymentAssetWithAmounts =>
            val setScriptTx = mkSetScriptTx(paymentAssetWithAmounts*)

            val hasWavesAsset  = paymentAssetWithAmounts.exists { case (a, _) => a == Asset.Waves }
            val hasIssuedAsset = paymentAssetWithAmounts.exists { case (a, _) => a == asset }
            withDomain(
              RideV6,
              Seq(AddrWithBalance(dAppAddress, setScriptTx.fee.value)) ++ {
                if (hasIssuedAsset) Seq(AddrWithBalance(assetIssuerAddress, issueTx.fee.value))
                else Seq()
              }
            ) { d =>
              d.appendBlock(setScriptTx)
              if (hasIssuedAsset) d.appendBlock(issueTx)

              // -1 to test insufficient funds
              def collectLessBalance(asset: Asset): Long = paymentAssetWithAmounts.collect { case (a, x) if a == asset => x }.sum - 1

              val blockchainOverrides = Json.obj(
                "accounts" -> Json.obj(
                  dAppAddress.toString -> Json
                    .obj()
                    .deepMerge {
                      if (hasWavesAsset) Json.obj("regularBalance" -> collectLessBalance(Asset.Waves))
                      else Json.obj()
                    }
                    .deepMerge {
                      if (hasIssuedAsset) Json.obj("assetBalances" -> Json.obj(asset.toString -> collectLessBalance(asset)))
                      else Json.obj()
                    }
                )
              )

              Post(
                routePath(s"/script/evaluate/$dAppAddress"),
                mkExprJson(blockchainOverrides)
              ) ~> utilsApi.copy(blockchain = d.blockchain).route ~> check {
                val json = responseAs[JsValue]
                withClue(s"${Json.prettyPrint(json)}: ") {
                  (json \ "error").asOpt[Int] shouldBe Some(199)
                  withClue("message: ") {
                    (json \ "message").as[String] should include regex """negative \w+ balance.+ -1"""
                  }
                }
              }
            }
          }
        }

        "positive" in {
          val exprTests = Table[Seq[(Asset, Long)]](
            "paymentAssetWithAmounts",
            Seq(Asset.Waves -> 1),
            Seq(asset       -> 1),
            Seq(Asset.Waves -> 1, asset -> 2)
          )

          forAll(exprTests) { paymentAssetWithAmounts =>
            val setScriptTx = mkSetScriptTx(paymentAssetWithAmounts*)

            val hasIssuedAsset = paymentAssetWithAmounts.exists { case (a, _) => a == asset }
            withDomain(
              RideV6,
              Seq(AddrWithBalance(dAppAddress, setScriptTx.fee.value)) ++ {
                if (hasIssuedAsset) Seq(AddrWithBalance(assetIssuerAddress, issueTx.fee.value))
                else Seq()
              }
            ) { d =>
              d.appendBlock(setScriptTx)
              if (hasIssuedAsset) d.appendBlock(issueTx)

              def collectBalance(asset: Asset): Long = paymentAssetWithAmounts.collect { case (a, x) if a == asset => x }.sum

              val blockchainOverrides = Json.obj(
                "accounts" -> Json.obj(
                  dAppAddress.toString -> Json
                    .obj("regularBalance" -> collectBalance(Asset.Waves))
                    .deepMerge {
                      if (hasIssuedAsset) Json.obj("assetBalances" -> Json.obj(asset.toString -> collectBalance(asset)))
                      else Json.obj()
                    }
                )
              )

              Post(
                routePath(s"/script/evaluate/$dAppAddress"),
                mkExprJson(blockchainOverrides)
              ) ~> utilsApi.copy(blockchain = d.blockchain).route ~> check {
                val json = responseAs[JsValue]
                withClue(s"${Json.prettyPrint(json)}: ") {
                  (json \ "error").toOption shouldBe empty
                  withClue("stateChanges: ") {
                    (json \ "stateChanges" \ "transfers").as[JsArray] shouldBe JsArray(
                      paymentAssetWithAmounts.map { case (asset, amount) =>
                        Json.obj(
                          "address" -> scriptTransferReceiverAddress.toString,
                          "asset"   -> Json.toJson(asset),
                          "amount"  -> amount
                        )
                      }
                    )
                  }
                }
              }
            }
          }
        }
      }

      val defaultInvocationFee = 1234567
      "invocation" - {
        "negative" in {
          val sponsorshipTx = sponsor(asset, Some(100000), assetIssuer)

          def toPaymentJson(asset: Asset, amount: Long): String =
            s""" {
               |   "assetId": ${Json.toJson(asset)},
               |   "amount": $amount
               | }"""

          def mkInvocationWithPaymentJson(feeAsset: Asset, state: JsObject, paymentAssetWithAmounts: Seq[(Asset, Long)]): JsObject = Json
            .parse(
              s""" {
                 |  "call": {
                 |    "function": "default"
                 |  },
                 |  "id": "3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8",
                 |  "fee": $defaultInvocationFee,
                 |  "feeAssetId": ${Json.toJson(feeAsset)},
                 |  "sender": "$callerAddress",
                 |  "senderPublicKey": "${callerKeyPair.publicKey}",
                 |  "payment": [
                 |    ${paymentAssetWithAmounts.map(Function.tupled(toPaymentJson)).mkString(",\n")}
                 |  ],
                 |  "state": ${Json.prettyPrint(state)}
                 | }""".stripMargin
            )
            .as[JsObject]

          val invocationTests = Table[Asset, Seq[(Asset, Long)]](
            ("feeAsset", "paymentAssetWithAmounts"),
            (Asset.Waves, Seq(Asset.Waves -> 1)),
            (Asset.Waves, Seq(asset -> 1)),
            (asset, Seq(Asset.Waves -> 1)),
            (asset, Seq(asset -> 1)),
            (asset, Seq(Asset.Waves -> 1, asset -> 2)),
            (Asset.Waves, Seq(Asset.Waves -> 1, asset -> 2))
          )

          forAll(invocationTests) { case (feeAsset, paymentAssetWithAmounts) =>
            val setScriptTx = mkSetScriptTx(paymentAssetWithAmounts*)

            val hasWavesAsset  = paymentAssetWithAmounts.exists { case (a, _) => a == Asset.Waves }
            val hasIssuedAsset = (paymentAssetWithAmounts.map(_._1) :+ feeAsset).contains(asset)
            withDomain(
              RideV6,
              Seq(AddrWithBalance(dAppAddress, setScriptTx.fee.value)) ++ {
                val sponsoredExtra = if (feeAsset == asset) sponsorshipTx.fee.value + defaultInvocationFee else 0
                if (hasIssuedAsset) Seq(AddrWithBalance(assetIssuerAddress, issueTx.fee.value + sponsoredExtra))
                else Seq()
              }
            ) { d =>
              d.appendBlock(setScriptTx)
              if (hasIssuedAsset) d.appendBlock(issueTx)
              if (feeAsset == asset) d.appendBlock(sponsorshipTx)

              val (invocationFeeInWaves, invocationFeeInAsset) = if (feeAsset == asset) (0, defaultInvocationFee) else (defaultInvocationFee, 0)

              // -1 to test insufficient funds
              def collectLessBalance(asset: Asset): Long = paymentAssetWithAmounts.collect { case (a, x) if a == asset => x }.sum - 1

              val blockchainOverrides = Json.obj(
                "accounts" -> Json.obj(
                  callerAddress.toString -> Json
                    .obj()
                    .deepMerge {
                      if (hasWavesAsset) Json.obj("regularBalance" -> (invocationFeeInWaves + collectLessBalance(Asset.Waves)))
                      else Json.obj()
                    }
                    .deepMerge {
                      if (hasIssuedAsset) Json.obj("assetBalances" -> Json.obj(asset.toString -> (invocationFeeInAsset + collectLessBalance(asset))))
                      else Json.obj()
                    }
                )
              )

              Post(
                routePath(s"/script/evaluate/$dAppAddress"),
                mkInvocationWithPaymentJson(
                  feeAsset = feeAsset,
                  state = blockchainOverrides,
                  paymentAssetWithAmounts = paymentAssetWithAmounts
                )
              ) ~> utilsApi.copy(blockchain = d.blockchain).route ~> check {
                val json = responseAs[JsValue]
                withClue(s"${Json.prettyPrint(json)}: ") {
                  (json \ "error").asOpt[Int] shouldBe Some(402)
                  withClue("details: ") {
                    (json \ "details").toString should include regex """negative \w+ balance.+ -1"""
                  }
                }
              }
            }
          }
        }

        "positive" in {
          val sponsorshipTx = sponsor(asset, Some(100000), assetIssuer)

          def toPaymentJson(asset: Asset, amount: Long): String =
            s""" {
               |   "assetId": ${Json.toJson(asset)},
               |   "amount": $amount
               | }"""

          def mkInvocationWithPaymentJson(feeAsset: Asset, state: JsObject, paymentAssetWithAmounts: Seq[(Asset, Long)]): JsObject = Json
            .parse(
              s""" {
                 |  "call": {
                 |    "function": "default"
                 |  },
                 |  "id": "3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8",
                 |  "fee": $defaultInvocationFee,
                 |  "feeAssetId": ${Json.toJson(feeAsset)},
                 |  "sender": "$callerAddress",
                 |  "senderPublicKey": "${callerKeyPair.publicKey}",
                 |  "payment": [
                 |    ${paymentAssetWithAmounts.map(Function.tupled(toPaymentJson)).mkString(",\n")}
                 |  ],
                 |  "state": ${Json.prettyPrint(state)}
                 | }""".stripMargin
            )
            .as[JsObject]

          val invocationTests = Table[Asset, Seq[(Asset, Long)]](
            ("feeAsset", "paymentAssetWithAmounts"),
            (Asset.Waves, Seq(Asset.Waves -> 1)),
            (Asset.Waves, Seq(asset -> 1)),
            (asset, Seq(Asset.Waves -> 1)),
            (asset, Seq(asset -> 1)),
            (asset, Seq(Asset.Waves -> 1, asset -> 2)),
            (Asset.Waves, Seq(Asset.Waves -> 1, asset -> 2))
          )

          forAll(invocationTests) { case (feeAsset, paymentAssetWithAmounts) =>
            val setScriptTx    = mkSetScriptTx(paymentAssetWithAmounts*)
            val hasIssuedAsset = (paymentAssetWithAmounts.map(_._1) :+ feeAsset).contains(asset)
            withDomain(
              RideV6,
              Seq(AddrWithBalance(dAppAddress, setScriptTx.fee.value)) ++ {
                val sponsoredExtra = if (feeAsset == asset) sponsorshipTx.fee.value + defaultInvocationFee else 0
                if (hasIssuedAsset) Seq(AddrWithBalance(assetIssuerAddress, issueTx.fee.value + sponsoredExtra))
                else Seq()
              }
            ) { d =>
              d.appendBlock(setScriptTx)
              if (hasIssuedAsset) d.appendBlock(issueTx)
              if (feeAsset == asset) d.appendBlock(sponsorshipTx)

              val (invocationFeeInWaves, invocationFeeInAsset) = if (feeAsset == asset) (0, defaultInvocationFee) else (defaultInvocationFee, 0)

              def collectBalance(asset: Asset): Long = paymentAssetWithAmounts.collect { case (a, x) if a == asset => x }.sum

              val callerWavesBalance = invocationFeeInWaves + collectBalance(Asset.Waves)
              val callerAssetBalance = invocationFeeInAsset + collectBalance(asset)
              val blockchainOverrides = Json.obj(
                "accounts" -> Json.obj(
                  callerAddress.toString -> Json
                    .obj(
                      "regularBalance" -> callerWavesBalance,
                      "assetBalances"  -> Json.obj(asset.toString -> callerAssetBalance)
                    )
                )
              )

              Post(
                routePath(s"/script/evaluate/$dAppAddress"),
                mkInvocationWithPaymentJson(
                  feeAsset = feeAsset,
                  state = blockchainOverrides,
                  paymentAssetWithAmounts = paymentAssetWithAmounts
                )
              ) ~> utilsApi.copy(blockchain = d.blockchain).route ~> check {
                val json = responseAs[JsValue]
                withClue(s"${Json.prettyPrint(json)}: ") {
                  (json \ "error").toOption shouldBe empty
                  withClue("stateChanges: ") {
                    (json \ "stateChanges" \ "transfers").as[JsArray] shouldBe JsArray(
                      paymentAssetWithAmounts.map { case (asset, amount) =>
                        Json.obj(
                          "address" -> scriptTransferReceiverAddress.toString,
                          "asset"   -> Json.toJson(asset),
                          "amount"  -> amount
                        )
                      }
                    )
                  }
                }
              }
            }
          }
        }
      }

      "waves balances" in {
        val miner        = scriptTransferReceiver
        val minerAddress = miner.toAddress

        val script = TestCompiler(V6).compileContract(
          s"""
             | @Callable(i)
             | func default() = ([], wavesBalance(Address(base58'$minerAddress')))
             | """.stripMargin
        )
        val setScriptTx = setScript(dAppAccount, script)

        def invocation(state: JsObject) =
          Json
            .parse(
              s""" {
                 |  "expr": "default()",
                 |  "state": ${Json.prettyPrint(state)}
                 | }""".stripMargin
            )
            .as[JsObject]

        val minerInitialWavesBalance = 10001
        val leasingTx                = lease(miner, dAppAddress, 333)

        withDomain(
          RideV6,
          Seq(
            AddrWithBalance(dAppAddress, setScriptTx.fee.value),
            AddrWithBalance(minerAddress, minerInitialWavesBalance + leasingTx.fee.value)
          )
        ) { d =>
          d.appendBlock(setScriptTx, leasingTx)
          val route = utilsApi.copy(blockchain = d.blockchain).route

          markup("without overrides")
          Post(
            routePath(s"/script/evaluate/$dAppAddress"),
            invocation(Json.obj())
          ) ~> route ~> check {
            val json = responseAs[JsValue]
            withClue(s"${Json.prettyPrint(json)}: ") {
              (json \ "error").toOption shouldBe empty
              withClue("result: ") {
                def check(tpe: String, expected: Long): Unit = withClue(s"$tpe: ") {
                  (json \ "result" \ "value" \ "_2" \ "value" \ tpe \ "value").asOpt[Long] shouldBe Some(expected)
                }

                check("available", minerInitialWavesBalance - leasingTx.amount.value)
                check("regular", minerInitialWavesBalance)
                check("generating", minerInitialWavesBalance - leasingTx.amount.value)
                check("effective", minerInitialWavesBalance - leasingTx.amount.value)
              }
            }
          }

          markup("with overrides")
          forAll(Table("regularBalanceDiff", -1000, 0, 1000)) { regularBalanceDiff =>
            val minerOverriddenWavesBalance = minerInitialWavesBalance + regularBalanceDiff
            Post(
              routePath(s"/script/evaluate/$dAppAddress"),
              invocation(
                Json.obj(
                  "accounts" -> Json.obj(
                    minerAddress.toString -> Json.obj("regularBalance" -> minerOverriddenWavesBalance)
                  )
                )
              )
            ) ~> route ~> check {
              val json = responseAs[JsValue]
              withClue(s"${Json.prettyPrint(json)}: ") {
                (json \ "error").toOption shouldBe empty
                withClue("result: ") {
                  def check(tpe: String, expected: Long): Unit = withClue(s"$tpe: ") {
                    (json \ "result" \ "value" \ "_2" \ "value" \ tpe \ "value").asOpt[Long] shouldBe Some(expected)
                  }

                  check("available", minerOverriddenWavesBalance - leasingTx.amount.value)
                  check("regular", minerOverriddenWavesBalance)
                  check("generating", math.min(minerInitialWavesBalance, minerOverriddenWavesBalance) - leasingTx.amount.value)
                  check("effective", minerOverriddenWavesBalance - leasingTx.amount.value)
                }
              }
            }
          }
        }
      }
    }

    "validation of root call" in {
      withDomain(RideV6, Seq(AddrWithBalance(secondAddress, 0.01 waves))) { d =>
        val route = seal(utilsApi.copy(blockchain = d.blockchain).route)
        val dApp = TestCompiler(V6).compileContract(
          s"""
             | @Callable(i)
             | func action() = [ ScriptTransfer(Address(base58'$defaultAddress'), 1, unit) ]
             |
             | @Callable(i)
             | func default() = []
             |
             | @Callable(i)
             | func issueAndTransfer() = {
             |   let issue = Issue("name", "description", 1000, 4, true, unit, 0)
             |   [
             |     issue,
             |     ScriptTransfer(Address(base58'$defaultAddress'), 1, calculateAssetId(issue))
             |   ]
             | }
             |
             | @Callable(i)
             | func syncCallWithPayment() = {
             |   strict r = Address(base58'$defaultAddress').invoke("default", [], [AttachedPayment(unit, 100)])
             |   []
             | }
           """.stripMargin
        )
        d.appendBlock(setScript(secondSigner, dApp))

        // negative balance after applying action
        Post(routePath(s"/script/evaluate/$secondAddress"), Json.obj("expr" -> "action()")) ~> route ~> check {
          responseAs[String] should include("AccountBalanceError")
        }

        // negative fee
        Post(routePath(s"/script/evaluate/$secondAddress"), Json.obj("fee" -> -1)) ~> route ~> check {
          responseAs[String] should include("error = Fee in WAVES for InvokeScriptTransaction (-1 in WAVES)")
        }

        // illegal fee asset id
        Post(routePath(s"/script/evaluate/$secondAddress"), Json.obj("feeAssetId" -> "xxxxx")) ~> route ~> check {
          responseAs[String] should include("Asset xxxxx does not exist, cannot be used to pay fees")
        }

        // negative payment
        Post(routePath(s"/script/evaluate/$secondAddress"), Json.parse("""{"payment":[{"amount":-1,"assetId":null}]}""")) ~> route ~> check {
          responseAs[String] should include("non-positive amount: -1 of Waves")
        }

        // illegal payment asset id
        Post(
          routePath(s"/script/evaluate/$secondAddress"),
          Json.parse(s"""{"payment":[{"amount":1,"assetId":"${ByteStr.fill(AssetIdLength)(1)}"}]}""")
        ) ~> route ~> check {
          responseAs[String] should include("Accounts balance errors")
        }

        // issue and transfer
        Post(
          routePath(s"/script/evaluate/$secondAddress"),
          Json.obj("fee" -> 100500000, "call" -> Json.obj("function" -> "issueAndTransfer"))
        ) ~> route ~> check {
          val actions = responseAs[JsObject] \ "result" \ "value"
          (actions \ 0 \ "type").as[String] shouldBe "Issue"
          (actions \ 1 \ "type").as[String] shouldBe "ScriptTransfer"
        }

        // transaction payment attached to sync call
        d.appendBlock(setScript(defaultSigner, dApp))
        Post(
          routePath(s"/script/evaluate/$secondAddress"),
          Json.parse("""{"call": {"function":"syncCallWithPayment"}, "payment":[{"amount":100,"assetId":null}]}""")
        ) ~> route ~> check {
          (responseAs[JsObject] \ "payment" \ 0 \ "amount").as[Int] shouldBe 100
          (responseAs[JsObject] \ "stateChanges" \ "invokes" \ 0 \ "payment" \ 0 \ "amount").as[Int] shouldBe 100
        }

        // not enough payment amount to attach to sync call
        d.appendBlock(setScript(defaultSigner, dApp))
        Post(
          routePath(s"/script/evaluate/$secondAddress"),
          Json.parse("""{"call": {"function":"syncCallWithPayment"}, "payment":[{"amount":99,"assetId":null}]}""")
        ) ~> route ~> check {
          (responseAs[JsObject] \ "payment" \ 0 \ "amount").as[Int] shouldBe 99
          (responseAs[JsObject] \ "message").as[String] should include("negative waves balance")
        }
      }
    }

    "taking into account sync call transfers" in {
      val thirdAddress = signer(2).toAddress
      withDomain(RideV6, Seq(AddrWithBalance(secondAddress, 0.01 waves), AddrWithBalance(thirdAddress, 1 waves))) { d =>
        val route = seal(utilsApi.copy(blockchain = d.blockchain).route)
        val dApp1 = TestCompiler(V6).compileContract(
          s"""
             | @Callable(i)
             | func call1() = {
             |   strict r = Address(base58'$thirdAddress').invoke("call2", [], [])
             |   [ ScriptTransfer(Address(base58'$defaultAddress'), 100, unit) ]
             | }
           """.stripMargin
        )
        val dApp2 = TestCompiler(V6).compileContract(
          s"""
             | @Callable(i)
             | func call2() = [ ScriptTransfer(i.caller, 100, unit) ]
           """.stripMargin
        )
        d.appendBlock(setScript(secondSigner, dApp1))
        d.appendBlock(setScript(signer(2), dApp2))
        Post(
          routePath(s"/script/evaluate/$secondAddress"),
          Json.parse("""{"call": {"function":"call1"}}""")
        ) ~> route ~> check {
          (responseAs[JsObject] \ "message").asOpt[String] should not be defined
          (responseAs[JsObject] \ "result").asOpt[JsObject] shouldBe defined
          (responseAs[JsObject] \ "call").asOpt[JsObject] shouldBe defined
        }
      }
    }

    "taking into account sync call payments" in {
      val thirdAddress = signer(2).toAddress
      withDomain(RideV6, Seq(AddrWithBalance(secondAddress, 1 waves), AddrWithBalance(thirdAddress, 0.01 waves))) { d =>
        val route = seal(utilsApi.copy(blockchain = d.blockchain).route)
        val dApp1 = TestCompiler(V6).compileContract(
          s"""
             | @Callable(i)
             | func call1() = {
             |   strict r = Address(base58'$thirdAddress').invoke("call2", [], [AttachedPayment(unit, 100)])
             |   []
             | }
           """.stripMargin
        )
        val dApp2 = TestCompiler(V6).compileContract(
          s"""
             | @Callable(i)
             | func call2() = [ ScriptTransfer(Address(base58'$defaultAddress'), 100, unit) ]
           """.stripMargin
        )
        d.appendBlock(setScript(secondSigner, dApp1))
        d.appendBlock(setScript(signer(2), dApp2))
        Post(
          routePath(s"/script/evaluate/$secondAddress"),
          Json.parse("""{"call": {"function":"call1"}}""")
        ) ~> route ~> check {
          (responseAs[JsObject] \ "message").asOpt[String] should not be defined
          (responseAs[JsObject] \ "result").asOpt[JsObject] shouldBe defined
          (responseAs[JsObject] \ "call").asOpt[JsObject] shouldBe defined
        }
      }
    }
  }

  private def dApp(caller: Address, callerPk: PublicKey, asset: IssuedAsset) =
    TestCompiler(V6).compileContract(
      s"""
         | @Callable(i)
         | func f(arg1: Int, arg2: String) = {
         |   let check =
         |     this                    == Address(base58'$defaultAddress')            &&
         |     i.caller                == Address(base58'$caller')                    &&
         |     i.originCaller          == Address(base58'$caller')                    &&
         |     i.callerPublicKey       == base58'$callerPk'                           &&
         |     i.originCallerPublicKey == base58'$callerPk'                           &&
         |     i.fee                   == 1234567                                     &&
         |     i.payments              == [AttachedPayment(unit, 1)]                  &&
         |     i.transactionId         == base58'3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8' &&
         |     i.feeAssetId            == base58'$asset'                              &&
         |     arg1 == 123 && arg2 == "abc"
         |   if (check) then [] else throw("wrong " + i.caller.toString() + " " + Address(base58'$caller').toString())
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

  private def invocation(feeAsset: Asset, senderAddress: Option[Address] = Some(secondAddress), senderPk: PublicKey = secondSigner.publicKey) =
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
           |  "fee": 1234567,
           |  "feeAssetId": "$feeAsset",
           |  ${senderAddress.fold("")(a => s""""sender": "$a",""")}
           |  "senderPublicKey": "$senderPk",
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

  private def expectedTrace(feeAsset: Asset) =
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
         |        "value": "$feeAsset"
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
         |        "value": 1234567
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
         |        "value": 1234567
         |      },
         |      {
         |        "type": "Int",
         |        "value": 1234567
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
         |        "value": "$feeAsset"
         |      },
         |      {
         |        "type": "ByteVector",
         |        "value": "$feeAsset"
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
}
