package com.wavesplatform.http

import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.testkit.RouteTestTimeout
import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.ApiError.{ScriptExecutionError, TooBigArrayAllocation}
import com.wavesplatform.api.http.CustomJson
import com.wavesplatform.api.http.requests.ScriptWithImportsRequest
import com.wavesplatform.api.http.utils.{UtilsApiRoute, UtilsInvocationRequest}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.DefaultBlockchainSettings
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction, VerifierAnnotation, VerifierFunction}
import com.wavesplatform.lang.directives.values.{V2, V3, V6}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.serialization.SerdeV1
import com.wavesplatform.lang.{Global, contract}
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature
import com.wavesplatform.settings.TestSettings
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.state.{Blockchain, IntegerDataEntry, LeaseBalance}
import com.wavesplatform.test.DomainPresets.{RideV5, RideV6}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.utils.{Schedulers, Time}
import io.netty.util.HashedWheelTimer
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.Inside
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks as PropertyChecks
import play.api.libs.json.*

import scala.concurrent.duration.*

class UtilsRouteSpec extends RouteSpec("/utils") with RestAPISettingsHelper with PropertyChecks with PathMockFactory with Inside with WithDomain {
  implicit val routeTestTimeout: RouteTestTimeout = RouteTestTimeout(10.seconds)
  implicit val timeout: FiniteDuration            = routeTestTimeout.duration

  private val estimator = ScriptEstimatorV2

  private val utilsApi: UtilsApiRoute = UtilsApiRoute(
    new Time {
      def correctedTime(): Long = System.currentTimeMillis()

      def getTimestamp(): Long = System.currentTimeMillis()
    },
    restAPISettings,
    Int.MaxValue,
    () => estimator,
    Schedulers.timeBoundedFixedPool(
      new HashedWheelTimer(),
      5.seconds,
      1,
      "rest-time-limited"
    ),
    stub[Blockchain]("globalBlockchain")
  )

  (() => utilsApi.blockchain.activatedFeatures).when().returning(Map()).anyNumberOfTimes()
  private val route = seal(utilsApi.route)

  val script: FUNCTION_CALL = FUNCTION_CALL(
    function = PureContext.eq.header,
    args = List(CONST_LONG(1), CONST_LONG(2))
  )

  val dappVer: DApp = DApp(
    meta = DAppMeta(),
    decs = List.empty,
    callableFuncs = List.empty,
    verifierFuncOpt = Some(
      VerifierFunction(VerifierAnnotation("tx"), FUNC("verify", List(), TRUE))
    )
  )

  val badScript: String =
    """
      |{-# STDLIB_VERSION 3 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |func f0() = true
      |func f1() = if (true) then f0() else f0()
      |func f2() = if (true) then f1() else f1()
      |func f3() = if (true) then f2() else f2()
      |func f4() = if (true) then f3() else f3()
      |func f5() = if (true) then f4() else f4()
      |func f6() = if (true) then f5() else f5()
      |func f7() = if (true) then f6() else f6()
      |func f8() = if (true) then f7() else f7()
      |func f9() = if (true) then f8() else f8()
      |func f10() = if (true) then f9() else f9()
      |func f11() = if (true) then f10() else f10()
      |func f12() = if (true) then f11() else f11()
      |func f13() = if (true) then f12() else f12()
      |func f14() = if (true) then f13() else f13()
      |func f15() = if (true) then f14() else f14()
      |func f16() = if (true) then f15() else f15()
      |func f17() = if (true) then f16() else f16()
      |func f18() = if (true) then f17() else f17()
      |func f19() = if (true) then f18() else f18()
      |func f20() = if (true) then f19() else f19()
      |func f21() = if (true) then f20() else f20()
      |func f22() = if (true) then f21() else f21()
      |func f23() = if (true) then f22() else f22()
      |func f24() = if (true) then f23() else f23()
      |func f25() = if (true) then f24() else f24()
      |func f26() = if (true) then f25() else f25()
      |func f27() = if (true) then f26() else f26()
      |func f28() = if (true) then f27() else f27()
      |func f29() = if (true) then f28() else f28()
      |func f30() = if (true) then f29() else f29()
      |func f31() = if (true) then f30() else f30()
      |func f32() = if (true) then f31() else f31()
      |func f33() = if (true) then f32() else f32()
      |func f34() = if (true) then f33() else f33()
      |func f35() = if (true) then f34() else f34()
      |func f36() = if (true) then f35() else f35()
      |func f37() = if (true) then f36() else f36()
      |func f38() = if (true) then f37() else f37()
      |func f39() = if (true) then f38() else f38()
      |func f40() = if (true) then f39() else f39()
      |func f41() = if (true) then f40() else f40()
      |func f42() = if (true) then f41() else f41()
      |func f43() = if (true) then f42() else f42()
      |func f44() = if (true) then f43() else f43()
      |func f45() = if (true) then f44() else f44()
      |func f46() = if (true) then f45() else f45()
      |func f47() = if (true) then f46() else f46()
      |func f48() = if (true) then f47() else f47()
      |func f49() = if (true) then f48() else f48()
      |func f50() = if (true) then f49() else f49()
      |func f51() = if (true) then f50() else f50()
      |func f52() = if (true) then f51() else f51()
      |func f53() = if (true) then f52() else f52()
      |func f54() = if (true) then f53() else f53()
      |func f55() = if (true) then f54() else f54()
      |func f56() = if (true) then f55() else f55()
      |func f57() = if (true) then f56() else f56()
      |func f58() = if (true) then f57() else f57()
      |func f59() = if (true) then f58() else f58()
      |func f60() = if (true) then f59() else f59()
      |func f61() = if (true) then f60() else f60()
      |func f62() = if (true) then f61() else f61()
      |func f63() = if (true) then f62() else f62()
      |func f64() = if (true) then f63() else f63()
      |func f65() = if (true) then f64() else f64()
      |func f66() = if (true) then f65() else f65()
      |func f67() = if (true) then f66() else f66()
      |func f68() = if (true) then f67() else f67()
      |func f69() = if (true) then f68() else f68()
      |func f70() = if (true) then f69() else f69()
      |func f71() = if (true) then f70() else f70()
      |func f72() = if (true) then f71() else f71()
      |func f73() = if (true) then f72() else f72()
      |func f74() = if (true) then f73() else f73()
      |func f75() = if (true) then f74() else f74()
      |func f76() = if (true) then f75() else f75()
      |func f77() = if (true) then f76() else f76()
      |func f78() = if (true) then f77() else f77()
      |func f79() = if (true) then f78() else f78()
      |func f80() = if (true) then f79() else f79()
      |func f81() = if (true) then f80() else f80()
      |func f82() = if (true) then f81() else f81()
      |func f83() = if (true) then f82() else f82()
      |func f84() = if (true) then f83() else f83()
      |func f85() = if (true) then f84() else f84()
      |func f86() = if (true) then f85() else f85()
      |func f87() = if (true) then f86() else f86()
      |func f88() = if (true) then f87() else f87()
      |func f89() = if (true) then f88() else f88()
      |func f90() = if (true) then f89() else f89()
      |func f91() = if (true) then f90() else f90()
      |func f92() = if (true) then f91() else f91()
      |func f93() = if (true) then f92() else f92()
      |func f94() = if (true) then f93() else f93()
      |func f95() = if (true) then f94() else f94()
      |func f96() = if (true) then f95() else f95()
      |func f97() = if (true) then f96() else f96()
      |func f98() = if (true) then f97() else f97()
      |func f99() = if (true) then f98() else f98()
      |func f100() = if (true) then f99() else f99()
      |f100()
      |""".stripMargin

  val badScriptBase64 =
    "AwoBAAAAAmYwAAAAAAYKAQAAAAJmMQAAAAADBgkBAAAAAmYwAAAAAAkBAAAAAmYwAAAAAAoBAAAAAmYyAAAAAAMGCQEAAAACZjEAAAAACQEAAAACZjEAAAAACgEAAAACZjMAAAAAAwYJAQAAAAJmMgAAAAAJAQAAAAJmMgAAAAAKAQAAAAJmNAAAAAADBgkBAAAAAmYzAAAAAAkBAAAAAmYzAAAAAAoBAAAAAmY1AAAAAAMGCQEAAAACZjQAAAAACQEAAAACZjQAAAAACgEAAAACZjYAAAAAAwYJAQAAAAJmNQAAAAAJAQAAAAJmNQAAAAAKAQAAAAJmNwAAAAADBgkBAAAAAmY2AAAAAAkBAAAAAmY2AAAAAAoBAAAAAmY4AAAAAAMGCQEAAAACZjcAAAAACQEAAAACZjcAAAAACgEAAAACZjkAAAAAAwYJAQAAAAJmOAAAAAAJAQAAAAJmOAAAAAAKAQAAAANmMTAAAAAAAwYJAQAAAAJmOQAAAAAJAQAAAAJmOQAAAAAKAQAAAANmMTEAAAAAAwYJAQAAAANmMTAAAAAACQEAAAADZjEwAAAAAAoBAAAAA2YxMgAAAAADBgkBAAAAA2YxMQAAAAAJAQAAAANmMTEAAAAACgEAAAADZjEzAAAAAAMGCQEAAAADZjEyAAAAAAkBAAAAA2YxMgAAAAAKAQAAAANmMTQAAAAAAwYJAQAAAANmMTMAAAAACQEAAAADZjEzAAAAAAoBAAAAA2YxNQAAAAADBgkBAAAAA2YxNAAAAAAJAQAAAANmMTQAAAAACgEAAAADZjE2AAAAAAMGCQEAAAADZjE1AAAAAAkBAAAAA2YxNQAAAAAKAQAAAANmMTcAAAAAAwYJAQAAAANmMTYAAAAACQEAAAADZjE2AAAAAAoBAAAAA2YxOAAAAAADBgkBAAAAA2YxNwAAAAAJAQAAAANmMTcAAAAACgEAAAADZjE5AAAAAAMGCQEAAAADZjE4AAAAAAkBAAAAA2YxOAAAAAAKAQAAAANmMjAAAAAAAwYJAQAAAANmMTkAAAAACQEAAAADZjE5AAAAAAoBAAAAA2YyMQAAAAADBgkBAAAAA2YyMAAAAAAJAQAAAANmMjAAAAAACgEAAAADZjIyAAAAAAMGCQEAAAADZjIxAAAAAAkBAAAAA2YyMQAAAAAKAQAAAANmMjMAAAAAAwYJAQAAAANmMjIAAAAACQEAAAADZjIyAAAAAAoBAAAAA2YyNAAAAAADBgkBAAAAA2YyMwAAAAAJAQAAAANmMjMAAAAACgEAAAADZjI1AAAAAAMGCQEAAAADZjI0AAAAAAkBAAAAA2YyNAAAAAAKAQAAAANmMjYAAAAAAwYJAQAAAANmMjUAAAAACQEAAAADZjI1AAAAAAoBAAAAA2YyNwAAAAADBgkBAAAAA2YyNgAAAAAJAQAAAANmMjYAAAAACgEAAAADZjI4AAAAAAMGCQEAAAADZjI3AAAAAAkBAAAAA2YyNwAAAAAKAQAAAANmMjkAAAAAAwYJAQAAAANmMjgAAAAACQEAAAADZjI4AAAAAAoBAAAAA2YzMAAAAAADBgkBAAAAA2YyOQAAAAAJAQAAAANmMjkAAAAACgEAAAADZjMxAAAAAAMGCQEAAAADZjMwAAAAAAkBAAAAA2YzMAAAAAAKAQAAAANmMzIAAAAAAwYJAQAAAANmMzEAAAAACQEAAAADZjMxAAAAAAoBAAAAA2YzMwAAAAADBgkBAAAAA2YzMgAAAAAJAQAAAANmMzIAAAAACgEAAAADZjM0AAAAAAMGCQEAAAADZjMzAAAAAAkBAAAAA2YzMwAAAAAKAQAAAANmMzUAAAAAAwYJAQAAAANmMzQAAAAACQEAAAADZjM0AAAAAAoBAAAAA2YzNgAAAAADBgkBAAAAA2YzNQAAAAAJAQAAAANmMzUAAAAACgEAAAADZjM3AAAAAAMGCQEAAAADZjM2AAAAAAkBAAAAA2YzNgAAAAAKAQAAAANmMzgAAAAAAwYJAQAAAANmMzcAAAAACQEAAAADZjM3AAAAAAoBAAAAA2YzOQAAAAADBgkBAAAAA2YzOAAAAAAJAQAAAANmMzgAAAAACgEAAAADZjQwAAAAAAMGCQEAAAADZjM5AAAAAAkBAAAAA2YzOQAAAAAKAQAAAANmNDEAAAAAAwYJAQAAAANmNDAAAAAACQEAAAADZjQwAAAAAAoBAAAAA2Y0MgAAAAADBgkBAAAAA2Y0MQAAAAAJAQAAAANmNDEAAAAACgEAAAADZjQzAAAAAAMGCQEAAAADZjQyAAAAAAkBAAAAA2Y0MgAAAAAKAQAAAANmNDQAAAAAAwYJAQAAAANmNDMAAAAACQEAAAADZjQzAAAAAAoBAAAAA2Y0NQAAAAADBgkBAAAAA2Y0NAAAAAAJAQAAAANmNDQAAAAACgEAAAADZjQ2AAAAAAMGCQEAAAADZjQ1AAAAAAkBAAAAA2Y0NQAAAAAKAQAAAANmNDcAAAAAAwYJAQAAAANmNDYAAAAACQEAAAADZjQ2AAAAAAoBAAAAA2Y0OAAAAAADBgkBAAAAA2Y0NwAAAAAJAQAAAANmNDcAAAAACgEAAAADZjQ5AAAAAAMGCQEAAAADZjQ4AAAAAAkBAAAAA2Y0OAAAAAAKAQAAAANmNTAAAAAAAwYJAQAAAANmNDkAAAAACQEAAAADZjQ5AAAAAAoBAAAAA2Y1MQAAAAADBgkBAAAAA2Y1MAAAAAAJAQAAAANmNTAAAAAACgEAAAADZjUyAAAAAAMGCQEAAAADZjUxAAAAAAkBAAAAA2Y1MQAAAAAKAQAAAANmNTMAAAAAAwYJAQAAAANmNTIAAAAACQEAAAADZjUyAAAAAAoBAAAAA2Y1NAAAAAADBgkBAAAAA2Y1MwAAAAAJAQAAAANmNTMAAAAACgEAAAADZjU1AAAAAAMGCQEAAAADZjU0AAAAAAkBAAAAA2Y1NAAAAAAKAQAAAANmNTYAAAAAAwYJAQAAAANmNTUAAAAACQEAAAADZjU1AAAAAAoBAAAAA2Y1NwAAAAADBgkBAAAAA2Y1NgAAAAAJAQAAAANmNTYAAAAACgEAAAADZjU4AAAAAAMGCQEAAAADZjU3AAAAAAkBAAAAA2Y1NwAAAAAKAQAAAANmNTkAAAAAAwYJAQAAAANmNTgAAAAACQEAAAADZjU4AAAAAAoBAAAAA2Y2MAAAAAADBgkBAAAAA2Y1OQAAAAAJAQAAAANmNTkAAAAACgEAAAADZjYxAAAAAAMGCQEAAAADZjYwAAAAAAkBAAAAA2Y2MAAAAAAKAQAAAANmNjIAAAAAAwYJAQAAAANmNjEAAAAACQEAAAADZjYxAAAAAAoBAAAAA2Y2MwAAAAADBgkBAAAAA2Y2MgAAAAAJAQAAAANmNjIAAAAACgEAAAADZjY0AAAAAAMGCQEAAAADZjYzAAAAAAkBAAAAA2Y2MwAAAAAKAQAAAANmNjUAAAAAAwYJAQAAAANmNjQAAAAACQEAAAADZjY0AAAAAAoBAAAAA2Y2NgAAAAADBgkBAAAAA2Y2NQAAAAAJAQAAAANmNjUAAAAACgEAAAADZjY3AAAAAAMGCQEAAAADZjY2AAAAAAkBAAAAA2Y2NgAAAAAKAQAAAANmNjgAAAAAAwYJAQAAAANmNjcAAAAACQEAAAADZjY3AAAAAAoBAAAAA2Y2OQAAAAADBgkBAAAAA2Y2OAAAAAAJAQAAAANmNjgAAAAACgEAAAADZjcwAAAAAAMGCQEAAAADZjY5AAAAAAkBAAAAA2Y2OQAAAAAKAQAAAANmNzEAAAAAAwYJAQAAAANmNzAAAAAACQEAAAADZjcwAAAAAAoBAAAAA2Y3MgAAAAADBgkBAAAAA2Y3MQAAAAAJAQAAAANmNzEAAAAACgEAAAADZjczAAAAAAMGCQEAAAADZjcyAAAAAAkBAAAAA2Y3MgAAAAAKAQAAAANmNzQAAAAAAwYJAQAAAANmNzMAAAAACQEAAAADZjczAAAAAAoBAAAAA2Y3NQAAAAADBgkBAAAAA2Y3NAAAAAAJAQAAAANmNzQAAAAACgEAAAADZjc2AAAAAAMGCQEAAAADZjc1AAAAAAkBAAAAA2Y3NQAAAAAKAQAAAANmNzcAAAAAAwYJAQAAAANmNzYAAAAACQEAAAADZjc2AAAAAAoBAAAAA2Y3OAAAAAADBgkBAAAAA2Y3NwAAAAAJAQAAAANmNzcAAAAACgEAAAADZjc5AAAAAAMGCQEAAAADZjc4AAAAAAkBAAAAA2Y3OAAAAAAKAQAAAANmODAAAAAAAwYJAQAAAANmNzkAAAAACQEAAAADZjc5AAAAAAoBAAAAA2Y4MQAAAAADBgkBAAAAA2Y4MAAAAAAJAQAAAANmODAAAAAACgEAAAADZjgyAAAAAAMGCQEAAAADZjgxAAAAAAkBAAAAA2Y4MQAAAAAKAQAAAANmODMAAAAAAwYJAQAAAANmODIAAAAACQEAAAADZjgyAAAAAAoBAAAAA2Y4NAAAAAADBgkBAAAAA2Y4MwAAAAAJAQAAAANmODMAAAAACgEAAAADZjg1AAAAAAMGCQEAAAADZjg0AAAAAAkBAAAAA2Y4NAAAAAAKAQAAAANmODYAAAAAAwYJAQAAAANmODUAAAAACQEAAAADZjg1AAAAAAoBAAAAA2Y4NwAAAAADBgkBAAAAA2Y4NgAAAAAJAQAAAANmODYAAAAACgEAAAADZjg4AAAAAAMGCQEAAAADZjg3AAAAAAkBAAAAA2Y4NwAAAAAKAQAAAANmODkAAAAAAwYJAQAAAANmODgAAAAACQEAAAADZjg4AAAAAAoBAAAAA2Y5MAAAAAADBgkBAAAAA2Y4OQAAAAAJAQAAAANmODkAAAAACgEAAAADZjkxAAAAAAMGCQEAAAADZjkwAAAAAAkBAAAAA2Y5MAAAAAAKAQAAAANmOTIAAAAAAwYJAQAAAANmOTEAAAAACQEAAAADZjkxAAAAAAoBAAAAA2Y5MwAAAAADBgkBAAAAA2Y5MgAAAAAJAQAAAANmOTIAAAAACgEAAAADZjk0AAAAAAMGCQEAAAADZjkzAAAAAAkBAAAAA2Y5MwAAAAAKAQAAAANmOTUAAAAAAwYJAQAAAANmOTQAAAAACQEAAAADZjk0AAAAAAoBAAAAA2Y5NgAAAAADBgkBAAAAA2Y5NQAAAAAJAQAAAANmOTUAAAAACgEAAAADZjk3AAAAAAMGCQEAAAADZjk2AAAAAAkBAAAAA2Y5NgAAAAAKAQAAAANmOTgAAAAAAwYJAQAAAANmOTcAAAAACQEAAAADZjk3AAAAAAoBAAAAA2Y5OQAAAAADBgkBAAAAA2Y5OAAAAAAJAQAAAANmOTgAAAAACgEAAAAEZjEwMAAAAAADBgkBAAAAA2Y5OQAAAAAJAQAAAANmOTkAAAAACQEAAAAEZjEwMAAAAAD7+x+p"

  val dAppWithFreeVerifier: String =
    """
      |{-# STDLIB_VERSION 3 #-}
      |{-# CONTENT_TYPE DAPP #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |
      |@Callable(i)
      |func write(value: Int) = {
      |    WriteSet([DataEntry("result", value)])
      |}
      |
      |@Callable(i)
      |func sendAsset(recipient: String, amount: Int, assetId: String) = {
      |    TransferSet([ScriptTransfer(Address(recipient.fromBase58String()), amount, assetId.fromBase58String())])
      |}
      |
      |@Callable(i)
      |func writeAndSendWaves(value: Int, recipient: String, amount: Int) = {
      |    ScriptResult(
      |        WriteSet([DataEntry("result", value)]),
      |        TransferSet([ScriptTransfer(Address(recipient.fromBase58String()), amount, unit)])
      |    )
      |}
      |
      |@Verifier(tx)
      |func verify() = sigVerify(base58'', base58'', base58'')
    """.stripMargin

  val dAppWithoutVerifier: String =
    """
      |{-# STDLIB_VERSION 3 #-}
      |{-# CONTENT_TYPE DAPP #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |
      |@Callable(i)
      |func write(value: Int) = {
      |    WriteSet([DataEntry("result", value)])
      |}
      |
      |@Callable(i)
      |func sendAsset(recipient: String, amount: Int, assetId: String) = {
      |    TransferSet([ScriptTransfer(Address(recipient.fromBase58String()), amount, assetId.fromBase58String())])
      |}
      |
      |@Callable(i)
      |func writeAndSendWaves(value: Int, recipient: String, amount: Int) = {
      |    ScriptResult(
      |        WriteSet([DataEntry("result", value)]),
      |        TransferSet([ScriptTransfer(Address(recipient.fromBase58String()), amount, unit)])
      |    )
      |}
    """.stripMargin

  val emptyDApp: String =
    """
      |{-# STDLIB_VERSION 3 #-}
      |{-# CONTENT_TYPE DAPP #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
    """.stripMargin

  val dAppWithNonCallable: String =
    """
      |{-# STDLIB_VERSION 3 #-}
      |{-# CONTENT_TYPE DAPP #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |
      |func test() = true
    """.stripMargin

  val dAppWithPaidVerifier: String =
    """
      |{-# STDLIB_VERSION 3 #-}
      |{-# CONTENT_TYPE DAPP #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |
      |@Callable(i)
      |func callable(value: Int) = {
      |    WriteSet([DataEntry("result", value)])
      |}
      |
      |@Verifier(tx)
      |func verify() = sigVerify(base58'', base58'', base58'') && sigVerify(base58'', base58'', base58'')
    """.stripMargin

  val bigSizeDApp: String =
    s"""
       |{-# STDLIB_VERSION 5 #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |
       |let looooooooooooooooooooooooongName = base58'${"a" * 3602}'
       |${(1 to 18).map(i => s"let a$i = base58'${"a" * 12200}'").mkString("\n")}
       |func test() = looooooooooooooooooooooooongName == looooooooooooooooooooooooongName
     """.stripMargin

  val freeCall =
    s"""
       | {-# STDLIB_VERSION 6 #-}
       | {-# CONTENT_TYPE EXPRESSION #-}
       | {-# SCRIPT_TYPE CALL #-}
       |
       | let a = this
       | strict r = invoke(Address(base58'3MS5SZmYhDWiFh8DvAhKuMMdcmGDiNWqawv'), "child", [], [])
       | func f () = throw()
       |
       | [BooleanEntry("check", (i.caller == a)), BinaryEntry("transactionId", i.transactionId)]
     """.stripMargin.trim + "\n"

  val freeCallExpr =
    ByteStr(Global.serializeExpression(TestCompiler(V6).compileFreeCall(freeCall).expr, V6)).base64

  routePath("/script/decompile") in {
    val base64 = ExprScript(script).explicitGet().bytes().base64
    Post(routePath("/script/decompile"), base64) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "STDLIB_VERSION").as[Int] shouldBe 1
      (json \ "CONTENT_TYPE").as[String] shouldBe "EXPRESSION"
      (json \ "script").as[String] shouldBe "" +
        "{-# STDLIB_VERSION 1 #-}\n" +
        "{-# CONTENT_TYPE EXPRESSION #-}\n" +
        "(1 == 2)"
    }

    // V1 Expression
    Post(routePath("/script/decompile"), "AQa3b8tH") ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "STDLIB_VERSION").as[Int] shouldBe 1
      (json \ "CONTENT_TYPE").as[String] shouldBe "EXPRESSION"
      (json \ "script").as[String] shouldBe "" +
        "{-# STDLIB_VERSION 1 #-}\n" +
        "{-# CONTENT_TYPE EXPRESSION #-}\n" +
        "true"
    }

    // V2 Expression
    Post(routePath("/script/decompile"), "AgZ7TN8j") ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "STDLIB_VERSION").as[Int] shouldBe 2
      (json \ "CONTENT_TYPE").as[String] shouldBe "EXPRESSION"
      (json \ "script").as[String] shouldBe "" +
        "{-# STDLIB_VERSION 2 #-}\n" +
        "{-# CONTENT_TYPE EXPRESSION #-}\n" +
        "true"
    }

    // V3 Expression
    Post(routePath("/script/decompile"), "AwZd0cYf") ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "STDLIB_VERSION").as[Int] shouldBe 3
      (json \ "CONTENT_TYPE").as[String] shouldBe "EXPRESSION"
      (json \ "script").as[String] shouldBe "" +
        "{-# STDLIB_VERSION 3 #-}\n" +
        "{-# CONTENT_TYPE EXPRESSION #-}\n" +
        "true"
    }

    val compiled = "AAIDAAAAAAAAAAIIAQAAAAEAAAAAAWEJAQAAABFAZXh0ck5hdGl2ZSgxMDQwKQAAAAIFAAAAA25pbAIAAAAAAAAAAAAAAACl8TPJ"
    Post(routePath("/script/decompile"), compiled) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "script").as[String].trim shouldBe
        """
          |{-# STDLIB_VERSION 3 #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |{-# CONTENT_TYPE DAPP #-}
          |let a = getIntegerValue(nil, "")
        """.stripMargin.trim
    }

    val dappVerBytesStr = ContractScript(V3, dappVer).explicitGet().bytes().base64

    testdAppDirective(dappVerBytesStr)
    testdAppDirective("\t\t \n\n" + dappVerBytesStr + " \t \n \t")

    Post(routePath("/script/decompile"), freeCallExpr) ~> route ~> check {
      (responseAs[JsValue] \ "script").as[String].trim shouldBe
        """
          |{-# STDLIB_VERSION 6 #-}
          |{-# CONTENT_TYPE EXPRESSION #-}
          |let a = this
          |let r = invoke(Address(base58'3MS5SZmYhDWiFh8DvAhKuMMdcmGDiNWqawv'), "child", nil, nil)
          |if ((r == r))
          |    then {
          |        func f () =         throw()
          |
          |[BooleanEntry("check", (i.caller == a)), BinaryEntry("transactionId", i.transactionId)]
          |        }
          |    else throw("Strict value is not equal to itself.")
        """.stripMargin.trim
    }
  }

  routePath("/script/meta") ignore {
    // Expression
    val exprBase64 = ExprScript(script).explicitGet().bytes().base64
    Post(routePath("/script/meta"), exprBase64) ~> route ~> check {
      val json = responseAs[JsValue]
      json("message").as[String] shouldBe "ScriptParseError(Expected DApp)"
    }

    // DApp
    val dApp = contract.DApp(
      DAppMeta(
        version = 1,
        List(
          CallableFuncSignature(ByteString.copyFrom(Array[Byte](1, 2, 4, 8))),
          CallableFuncSignature(ByteString.copyFrom(Array[Byte](8, 4, 2, 1))),
          CallableFuncSignature(ByteString.EMPTY)
        )
      ),
      List(
        LET("letName", CONST_BOOLEAN(true)),
        FUNC("funcName", List("arg1", "arg2"), CONST_BOOLEAN(false))
      ),
      List(
        CallableFunction(
          CallableAnnotation("func1"),
          FUNC("func1", List("a", "b", "c", "d"), CONST_BOOLEAN(true))
        ),
        CallableFunction(
          CallableAnnotation("func2"),
          FUNC("func2", List("x", "y", "z", "w"), CONST_BOOLEAN(false))
        ),
        CallableFunction(
          CallableAnnotation("func3"),
          FUNC("default", List(), CONST_BOOLEAN(false))
        )
      ),
      Some(
        VerifierFunction(
          VerifierAnnotation("hmmm"),
          FUNC("funcAgain", List("arg"), CONST_BOOLEAN(false))
        )
      )
    )
    val dappBase64 = ContractScript(V3, dApp).explicitGet().bytes().base64
    Post(routePath("/script/meta"), dappBase64) ~> route ~> check {
      val json = responseAs[JsObject]
      json("version").as[String] shouldBe "1"
      json("callableFuncTypes") shouldBe JsObject(
        Seq(
          "func1" -> JsArray(
            Seq(
              JsObject(Seq("name" -> JsString("a"), "type" -> JsString("Int"))),
              JsObject(Seq("name" -> JsString("b"), "type" -> JsString("ByteVector"))),
              JsObject(Seq("name" -> JsString("c"), "type" -> JsString("Boolean"))),
              JsObject(Seq("name" -> JsString("d"), "type" -> JsString("String")))
            )
          ),
          "func2" -> JsArray(
            Seq(
              JsObject(Seq("name" -> JsString("x"), "type" -> JsString("String"))),
              JsObject(Seq("name" -> JsString("y"), "type" -> JsString("Boolean"))),
              JsObject(Seq("name" -> JsString("z"), "type" -> JsString("ByteVector"))),
              JsObject(Seq("name" -> JsString("w"), "type" -> JsString("Int")))
            )
          ),
          "default" -> JsArray()
        )
      )
    }

    // preserves earlier compiled dApp params order
    val dAppBase64 =
      "base64:AAIDAAAAAAAAAEEIARIMCgoICAgBAQEBAQEBEgoKCAgBAQEBAQEBEgASABIDCgEIEgASAwoBCBIAEgMKAQQSABIAEgMKAQQSAwoBBAAAAD0AAAAADGRlcG9zaXRUb2tlbgUAAAAEdW5pdAAAAAANb3JhY2xlRGF0YUtleQIAAAALd2F2ZXNfYnRjXzgAAAAABHRlbjgJAABoAAAAAgkAAGgAAAACAAAAAAAAAABkAAAAAAAAAAPoAAAAAAAAAAPoAAAAAApsZW5kZXJzS2V5AgAAAAdsZW5kZXJzAAAAABFlbmFibGVkTGVuZGVyc0tleQIAAAAOZW5hYmxlZExlbmRlcnMAAAAADmdyYWNlUGVyaW9kS2V5AgAAAAtncmFjZVBlcmlvZAAAAAARaW50ZXJlc3RQZXJpb2RLZXkCAAAADmludGVyZXN0UGVyaW9kAAAAABFidXJuZG93blBlcmlvZEtleQIAAAAOYnVybmRvd25QZXJpb2QAAAAACW9yYWNsZUtleQIAAAAGb3JhY2xlAAAAABVkaXNjb3VudFBlcmNlbnRpbGVLZXkCAAAAEmRpc2NvdW50UGVyY2VudGlsZQAAAAAKbWF4UmF0ZUtleQIAAAAHbWF4UmF0ZQAAAAANYXNzZXRUb2tlbktleQIAAAAKYXNzZXRUb2tlbgAAAAAIYWRtaW5LZXkCAAAABW93bmVyAAAAABdzZXJ2aWNlRmVlUGVyY2VudGlsZUtleQIAAAALc2VydmljZV9mZWUAAAAAC2xlbmRTaXplS2V5AgAAAAlsZW5kX3NpemUAAAAAE2VuYWJsZURlcG9zaXRCdGNLZXkCAAAAEmVuYWJsZV9kZXBvc2l0X2J0YwAAAAARZW5hYmxlTmV3TG9hbnNLZXkCAAAAEGVuYWJsZV9uZXdfbG9hbnMBAAAACnN0YXJ0T2ZLZXkAAAABAAAABnJlbnRlcgkAASwAAAACAgAAAAlzdGFydF9vZl8FAAAABnJlbnRlcgEAAAAPZW5kT2ZHcmFjZU9mS2V5AAAAAQAAAAZyZW50ZXIJAAEsAAAAAgIAAAAQZW5kX29mX2dyYWNlX29mXwUAAAAGcmVudGVyAQAAABJlbmRPZkludGVyZXN0T2ZLZXkAAAABAAAABnJlbnRlcgkAASwAAAACAgAAABNlbmRfb2ZfaW50ZXJlc3Rfb2ZfBQAAAAZyZW50ZXIBAAAAEmVuZE9mQnVybmRvd25PZktleQAAAAEAAAAGcmVudGVyCQABLAAAAAICAAAAE2VuZF9vZl9idXJuZG93bl9vZl8FAAAABnJlbnRlcgEAAAAJcmF0ZU9mS2V5AAAAAQAAAAZyZW50ZXIJAAEsAAAAAgIAAAAIcmF0ZV9vZl8FAAAABnJlbnRlcgEAAAAMZGVwb3NpdE9mS2V5AAAAAQAAAAZyZW50ZXIJAAEsAAAAAgIAAAALZGVwb3NpdF9vZl8FAAAABnJlbnRlcgEAAAAJbGVuZE9mS2V5AAAAAQAAAAZyZW50ZXIJAAEsAAAAAgIAAAAIbGVuZF9vZl8FAAAABnJlbnRlcgEAAAAMbGVuZGVyc09mS2V5AAAAAQAAAAZyZW50ZXIJAAEsAAAAAgIAAAALbGVuZGVyc19vZl8FAAAABnJlbnRlcgEAAAAUY2lyY3VsYXRpbmdBc3NldHNLZXkAAAABAAAABmxlbmRlcgkAASwAAAACAgAAABZjdXJjdWxhdGluZ19hc3NldHNfb2ZfBQAAAAZsZW5kZXIBAAAADm9wZW5MZW5kc09mS2V5AAAAAQAAAAZsZW5kZXIJAAEsAAAAAgIAAAAOb3Blbl9sZW5kc19vZl8FAAAABmxlbmRlcgAAAAAIbGVuZFNpemUJAABoAAAAAgkBAAAAE3ZhbHVlT3JFcnJvck1lc3NhZ2UAAAACCQAEGgAAAAIFAAAABHRoaXMFAAAAC2xlbmRTaXplS2V5AgAAAAtubyBsZW5kU2l6ZQUAAAAEdGVuOAAAAAAUbmV3RGVwb3NpdEJ0Y0VuYWJsZWQEAAAAByRtYXRjaDAJAAQbAAAAAgUAAAAEdGhpcwUAAAATZW5hYmxlRGVwb3NpdEJ0Y0tleQMJAAABAAAAAgUAAAAHJG1hdGNoMAIAAAAHQm9vbGVhbgQAAAABYgUAAAAHJG1hdGNoMAUAAAABYgYAAAAAD25ld0xvYW5zRW5hYmxlZAQAAAAHJG1hdGNoMAkABBsAAAACBQAAAAR0aGlzBQAAABFlbmFibGVOZXdMb2Fuc0tleQMJAAABAAAAAgUAAAAHJG1hdGNoMAIAAAAHQm9vbGVhbgQAAAABYgUAAAAHJG1hdGNoMAUAAAABYgYAAAAADWFsbExlbmRlcnNTdHIEAAAAByRtYXRjaDAJAAQdAAAAAgUAAAAEdGhpcwUAAAAKbGVuZGVyc0tleQMJAAABAAAAAgUAAAAHJG1hdGNoMAIAAAAGU3RyaW5nBAAAAAF4BQAAAAckbWF0Y2gwBQAAAAF4AgAAAAAAAAAAEWVuYWJsZWRMZW5kZXJzU3RyBAAAAAckbWF0Y2gwCQAEHQAAAAIFAAAABHRoaXMFAAAAEWVuYWJsZWRMZW5kZXJzS2V5AwkAAAEAAAACBQAAAAckbWF0Y2gwAgAAAAZTdHJpbmcEAAAAAXgFAAAAByRtYXRjaDAFAAAAAXgCAAAAAAAAAAAIb3duZXJTdHIJAQAAABN2YWx1ZU9yRXJyb3JNZXNzYWdlAAAAAgkABB0AAAACBQAAAAR0aGlzBQAAAAhhZG1pbktleQIAAAAITm8gb3duZXIAAAAABW93bmVyCQEAAAATdmFsdWVPckVycm9yTWVzc2FnZQAAAAIJAQAAABFhZGRyZXNzRnJvbVN0cmluZwAAAAEFAAAACG93bmVyU3RyAgAAAAhObyBvd25lcgAAAAAKYXNzZXRUb2tlbgkAAlkAAAABCQEAAAATdmFsdWVPckVycm9yTWVzc2FnZQAAAAIJAAQdAAAAAgUAAAAEdGhpcwUAAAANYXNzZXRUb2tlbktleQIAAAAKTm8gYXNzZXRJZAAAAAALZ3JhY2VQZXJpb2QJAQAAABN2YWx1ZU9yRXJyb3JNZXNzYWdlAAAAAgkABBoAAAACBQAAAAR0aGlzBQAAAA5ncmFjZVBlcmlvZEtleQIAAAAPTm8gZ3JhY2UgcGVyaW9kAAAAAA5pbnRlcmVzdFBlcmlvZAkBAAAAE3ZhbHVlT3JFcnJvck1lc3NhZ2UAAAACCQAEGgAAAAIFAAAABHRoaXMFAAAAEWludGVyZXN0UGVyaW9kS2V5AgAAABJObyBpbnRlcmVzdCBwZXJpb2QAAAAADmJ1cm5kb3duUGVyaW9kCQEAAAATdmFsdWVPckVycm9yTWVzc2FnZQAAAAIJAAQaAAAAAgUAAAAEdGhpcwUAAAARYnVybmRvd25QZXJpb2RLZXkCAAAAEk5vIGJ1cm5kb3duIHBlcmlvZAAAAAAHbWF4UmF0ZQkBAAAAE3ZhbHVlT3JFcnJvck1lc3NhZ2UAAAACCQAEGgAAAAIFAAAABHRoaXMFAAAACm1heFJhdGVLZXkCAAAAE05vIG9yYWNsZSBtYXggdmFsdWUAAAAABm9yYWNsZQkBAAAAE3ZhbHVlT3JFcnJvck1lc3NhZ2UAAAACCQAEHQAAAAIFAAAABHRoaXMFAAAACW9yYWNsZUtleQIAAAAJTm8gb3JhY2xlAAAAAAtvcmFjbGVWYWx1ZQkBAAAAE3ZhbHVlT3JFcnJvck1lc3NhZ2UAAAACCQAEGgAAAAIJAQAAABN2YWx1ZU9yRXJyb3JNZXNzYWdlAAAAAgkBAAAAEWFkZHJlc3NGcm9tU3RyaW5nAAAAAQUAAAAGb3JhY2xlAgAAABJiYWQgb3JhY2xlIGFkZHJlc3MFAAAADW9yYWNsZURhdGFLZXkCAAAAD05vIG9yYWNsZSB2YWx1ZQAAAAASZGlzY291bnRQZXJjZW50aWxlCQEAAAATdmFsdWVPckVycm9yTWVzc2FnZQAAAAIJAAQaAAAAAgUAAAAEdGhpcwUAAAAVZGlzY291bnRQZXJjZW50aWxlS2V5AgAAABZObyBkaXNjb3VudCBwZXJjZW50aWxlAAAAABRzZXJ2aWNlRmVlUGVyY2VudGlsZQkBAAAAE3ZhbHVlT3JFcnJvck1lc3NhZ2UAAAACCQAEGgAAAAIFAAAABHRoaXMFAAAAF3NlcnZpY2VGZWVQZXJjZW50aWxlS2V5AgAAABlObyBzZXJ2aWNlIGZlZSBwZXJjZW50aWxlAAAAAARyYXRlAwkAAGcAAAACBQAAAAdtYXhSYXRlBQAAAAtvcmFjbGVWYWx1ZQUAAAALb3JhY2xlVmFsdWUJAAACAAAAAQkAASwAAAACCQABLAAAAAIJAAEsAAAAAgIAAAAfU3VzcGljaW91cyByYXRlIHZhbHVlOiBhY3R1YWw6IAkAAaQAAAABBQAAAAtvcmFjbGVWYWx1ZQIAAAAHLCBtYXg6IAkAAaQAAAABBQAAAAdtYXhSYXRlAAAAABFtaW5pbWFsTGVuZEFtb3VudAkAAGQAAAACCQAAaQAAAAIJAABoAAAAAgAAAAAAAAAAZAUAAAAEdGVuOAkAAGgAAAACBQAAABJkaXNjb3VudFBlcmNlbnRpbGUFAAAABHJhdGUDCQAAZgAAAAIJAABqAAAAAgkAAGgAAAACAAAAAAAAAABkBQAAAAR0ZW44CQAAaAAAAAIFAAAAEmRpc2NvdW50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AQAAAAhmb2xkRnVuYwAAAAIFAAAADiRhY2MyMDUzMTM1Mzk1CQABkQAAAAIFAAAADSRsaXN0NTMxMzUzOTUAAAAAAAAAABQJAAACAAAAAQIAAAATTGlzdCBzaXplIGV4Y2VlZCAyMAEAAAAVdW5jbGFpbWVkRGlzdHJpYnV0aW9uAAAAAwAAAAV3YXZlcwAAAANidGMAAAAHbGVuZGVycwQAAAAQd2F2ZXNTZXJ2aWNlUGFydAkAAGsAAAADBQAAAAV3YXZlcwUAAAAUc2VydmljZUZlZVBlcmNlbnRpbGUAAAAAAAAAJxAEAAAAD3dhdmVzRGlzdHJpYnV0ZQkAAGUAAAACBQAAAAV3YXZlcwUAAAAQd2F2ZXNTZXJ2aWNlUGFydAQAAAAYbmV3V2F2ZXNTZXJ2aWNlVW5jbGFpbWVkCQAAZAAAAAIJAQAAABN1bmNsYWltZWREZXBvc2l0Rm9yAAAAAQUAAAAIb3duZXJTdHIFAAAAEHdhdmVzU2VydmljZVBhcnQEAAAACmxlbmRlcnNBbXQJAAGQAAAAAQUAAAAHbGVuZGVycwoBAAAACGZvbGRGdW5jAAAAAgAAAAFsAAAABmxlbmRlcgQAAAAMbmV3VW5jbGFpbWVkCQAAZAAAAAIJAQAAABN1bmNsYWltZWREZXBvc2l0Rm9yAAAAAQUAAAAGbGVuZGVyCQAAaQAAAAIFAAAAD3dhdmVzRGlzdHJpYnV0ZQUAAAAKbGVuZGVyc0FtdAQAAAAPbmV3RGVwb3NpdFZhbHVlCQAAZQAAAAIJAQAAABFAZXh0ck5hdGl2ZSgxMDUwKQAAAAIFAAAABHRoaXMJAQAAABRjaXJjdWxhdGluZ0Fzc2V0c0tleQAAAAEFAAAABmxlbmRlcgkAAGkAAAACBQAAAANidGMFAAAACmxlbmRlcnNBbXQJAARMAAAAAgkBAAAACURhdGFFbnRyeQAAAAIJAQAAABZ1bmNsYWltZWREZXBvc2l0Rm9yS2V5AAAAAQUAAAAGbGVuZGVyBQAAAAxuZXdVbmNsYWltZWQJAARMAAAAAgkBAAAACURhdGFFbnRyeQAAAAIJAQAAABRjaXJjdWxhdGluZ0Fzc2V0c0tleQAAAAEFAAAABmxlbmRlcgUAAAAPbmV3RGVwb3NpdFZhbHVlCQAETAAAAAIJAQAAABhkZWNyZW1lbnRPcGVuTGVuZHNBbW91bnQAAAABBQAAAAZsZW5kZXIFAAAAAWwEAAAADSRsaXN0NjE1NTYyNTUFAAAAB2xlbmRlcnMEAAAADSRzaXplNjE1NTYyNTUJAAGQAAAAAQUAAAANJGxpc3Q2MTU1NjI1NQQAAAANJGFjYzA2MTU1NjI1NQkABEwAAAACCQEAAAAJRGF0YUVudHJ5AAAAAgkBAAAAFnVuY2xhaW1lZERlcG9zaXRGb3JLZXkAAAABBQAAAAhvd25lclN0cgUAAAAYbmV3V2F2ZXNTZXJ2aWNlVW5jbGFpbWVkBQAAAANuaWwDCQAAAAAAAAIFAAAADSRzaXplNjE1NTYyNTUAAAAAAAAAAAAFAAAADSRhY2MwNjE1NTYyNTUEAAAADSRhY2MxNjE1NTYyNTUJAQAAAAhmb2xkRnVuYwAAAAIFAAAADSRhY2MwNjE1NTYyNTUJAAGRAAAAAgUAAAANJGxpc3Q2MTU1NjI1NQAAAAAAAAAAAAMJAAAAAAAAAgUAAAANJHNpemU2MTU1NjI1NQAAAAAAAAAAAQUAAAANJGFjYzE2MTU1NjI1NQQAAAANJGFjYzI2MTU1NjI1NQkBAAAACGZvbGRGdW5jAAAAAgUAAAANJGFjYzE2MTU1NjI1NQkAAZEAAAACBQAAAA0kbGlzdDYxNTU2MjU1AAAAAAAAAAABAwkAAAAAAAACBQAAAA0kc2l6ZTYxNTU2MjU1AAAAAAAAAAACBQAAAA0kYWNjMjYxNTU2MjU1BAAAAA0kYWNjMzYxNTU2MjU1CQEAAAAIZm9sZEZ1bmMAAAACBQAAAA0kYWNjMjYxNTU2MjU1CQABkQAAAAIFAAAADSRsaXN0NjE1NTYyNTUAAAAAAAAAAAIDCQAAAAAAAAIFAAAADSRzaXplNjE1NTYyNTUAAAAAAAAAAAMFAAAADSRhY2MzNjE1NTYyNTUEAAAADSRhY2M0NjE1NTYyNTUJAQAAAAhmb2xkRnVuYwAAAAIFAAAADSRhY2MzNjE1NTYyNTUJAAGRAAAAAgUAAAANJGxpc3Q2MTU1NjI1NQAAAAAAAAAAAwMJAAAAAAAAAgUAAAANJHNpemU2MTU1NjI1NQAAAAAAAAAABAUAAAANJGFjYzQ2MTU1NjI1NQQAAAANJGFjYzU2MTU1NjI1NQkBAAAACGZvbGRGdW5jAAAA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dFRva2VuBAAAAApsb2FuQW1vdW50CQEAAAARQGV4dHJOYXRpdmUoMTA1MCkAAAACBQAAAAR0aGlzCQEAAAAJbGVuZE9mS2V5AAAAAQUAAAAJcmVudGVyU3RyBAAAAA5kZXBvc2l0ZWRWYWx1ZQkBAAAAEUBleHRyTmF0aXZlKDEwNTApAAAAAgUAAAAEdGhpcwkBAAAADGRlcG9zaXRPZktleQAAAAEFAAAACXJlbnRlclN0cgMJAQAAAAEhAAAAAQUAAAALaGFzT3BlbkxvYW4JAAACAAAAAQIAAAAXTm8gb3BlbiBsb2FuIGZvciBjYWxsZXIDCQEAAAABIQAAAAEFAAAADmlzVG9rZW5Db3JyZWN0CQAAAgAAAAEJAAEsAAAAAgkAASwAAAACCQABLAAAAAICAAAAF1VzZXIgbXVzdCByZXR1cm4gV0JUQzogCQACWAAAAAEFAAAACmFzc2V0VG9rZW4CAAAAECBidXQgcmV0dXJuaW5nOiAJAAJYAAAAAQUAAAANcmV0dXJuQXNzZXRJZAQAAAAKZW5kT2ZHcmFjZQkBAAAAEUBleHRyTmF0aXZlKDEwNTApAAAAAgUAAAAEdGhpcwkBAAAAD2VuZE9mR3JhY2VPZktleQAAAAEFAAAACXJlbnRlclN0cgQAAAANZW5kT2ZCdXJuZG93bgkBAAAAEUBleHRyTmF0aXZlKDEwNTApAAAAAgUAAAAEdGhpcwkBAAAAEmVuZE9mQnVybmRvd25PZktleQAAAAEFAAAACXJlbnRlclN0cgQAAAANZW5kT2ZJbnRlcmVzdAkBAAAAEUBleHRyTmF0aXZlKDEwNTApAAAAAgUAAAAEdGhpcwkBAAAAEmVuZE9mSW50ZXJlc3RPZktleQAAAAEFAAAACXJlbnRlclN0cgQAAAATY2FuUmV0dXJuRnVsbEFtb3VudAkAAGcAAAACBQAAAAplbmRPZkdyYWNlBQAAAAZoZWlnaHQEAAAAFHJldHVybnNUaGVTYW1lQW1vdW50CQAAZwAAAAIFAAAACmVuZE9mR3JhY2UFAAAABmhlaWdodAMJAABnAAAAAgUAAAAGaGVpZ2h0BQAAAA1lbmRPZkludGVyZXN0CQAAAgAAAAECAAAAFXlvdXIgbG9hbiBoYXMgZXhwaXJlZAQAAAANbGVuZGVyc1Byb2ZpdAMJAABmAAAAAgUAAAAGaGVpZ2h0BQAAAAplbmRPZkdyYWNlCQAAawAAAAMFAAAACmxvYW5BbW91bnQJAABlAAAAAgUAAAAGaGVpZ2h0BQAAAAplbmRPZkdyYWNlCQAAZQAAAAIFAAAADWVuZE9mQnVybmRvd24FAAAACmVuZE9mR3JhY2UAAAAAAAAAAAAEAAAADnJlcXVpcmVkQW1vdW50AwUAAAAUcmV0dXJuc1RoZVNhbWVBbW91bnQFAAAACmxvYW5BbW91bnQJAABkAAAAAgUAAAAKbG9hbkFtb3VudAUAAAANbGVuZGVyc1Byb2ZpdAQAAAAVaXNSZXR1cm5BbW91bnRDb3JyZWN0CQAAZwAAAAIFAAAACXJldHVybkFtdAUAAAAOcmVxdWlyZWRBbW91bnQEAAAAE2lzUmV0dXJuQW1vdW50RXhhY3QJAAAAAAAAAgUAAAAJcmV0dXJuQW10BQAAAA5yZXF1aXJlZEFtb3VudAMJAQAAAAEhAAAAAQUAAAAVaXNSZXR1cm5BbW91bnRDb3JyZWN0CQAAAgAAAAEJAAEsAAAAAgkAASwAAAACCQABLAAAAAICAAAAEVVzZXIgbXVzdCByZXR1cm4gCQABpAAAAAEFAAAACmxvYW5BbW91bnQCAAAAGSBzYXRvc2hpcywgYnV0IHJldHVybmluZyAJAAGkAAAAAQUAAAAJcmV0dXJuQW10BAAAAAtkZXBvc2l0QmFjawkBAAAADlNjcmlwdFRyYW5zZmVyAAAAAwUAAAAGcmVudGVyBQAAAA5kZXBvc2l0ZWRWYWx1ZQUAAAAMZGVwb3NpdFRva2VuBAAAABJleGNlc3NSZXR1cm5BbW91bnQJAQAAAA5TY3JpcHRUcmFuc2ZlcgAAAAMFAAAABnJlbnRlcgkAAGUAAAACBQAAAAlyZXR1cm5BbXQFAAAADnJlcXVpcmVkQW1vdW50BQAAAAphc3NldFRva2VuBAAAAAl0cmFuc2ZlcnMDBQAAABNpc1JldHVybkFtb3VudEV4YWN0CQAETAAAAAIFAAAAC2RlcG9zaXRCYWNrBQAAAANuaWwJAARMAAAAAgUAAAALZGVwb3NpdEJhY2sJAARMAAAAAgUAAAASZXhjZXNzUmV0dXJuQW1vdW50BQAAAANuaWwEAAAAB2xlbmRlcnMJAAS1AAAAAgkBAAAAE3ZhbHVlT3JFcnJvck1lc3NhZ2UAAAACCQAEHQAAAAIFAAAABHRoaXMJAQAAAAxsZW5kZXJzT2ZLZXkAAAABBQAAAAlyZW50ZXJTdHICAAAAG05vIGxlbmRlcnMgZm9yIGFuIG9wZW4gbG9hbgIAAAABfAkBAAAADFNjcmlwdFJlc3VsdAAAAAIJAQAAAAdjbG9zaW5nAAAABQUAAAAJcmVudGVyU3RyAAAAAAAAAAAABQAAAA1sZW5kZXJzUHJvZml0BgUAAAAHbGVuZGVycwkBAAAAC1RyYW5zZmVyU2V0AAAAAQUAAAAJdHJhbnNmZXJzAQAAAANhZGQAAAACAAAAB2xlbmRlcnMAAAAGbGVuZGVyAwkAAAAAAAACBQAAAAdsZW5kZXJzAgAAAAAFAAAABmxlbmRlcgkAASwAAAACCQABLAAAAAIFAAAAB2xlbmRlcnMCAAAAAXwFAAAABmxlbmRlcgEAAAAGcmVtb3ZlAAAAAgAAAAdsZW5kZXJzAAAABmxlbmRlcgQAAAADYXJyCQAEtQAAAAIFAAAAB2xlbmRlcnMCAAAAAXwKAQAAAAhmb2xkRnVuYwAAAAIAAAADYWNjAAAABGl0ZW0DCQAAAAAAAAIFAAAABGl0ZW0FAAAABmxlbmRlcgUAAAADYWNjCQEAAAADYWRkAAAAAgUAAAADYWNjBQAAAARpdGVtBAAAAA8kbGlzdDEwMDI2MTAwNTMFAAAAA2FycgQAAAAPJHNpemUxMDAyNjEwMDUzCQABkAAAAAEFAAAADyRsaXN0MTAwMjYxMDA1MwQAAAAPJGFjYzAxMDAyNjEwMDUzAgAAAAADCQAAAAAAAAIFAAAADyRzaXplMTAwMjYxMDA1MwAAAAAAAAAAAAUAAAAPJGFjYzAxMDAyNjEwMDUzBAAAAA8kYWNjMTEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAADyRhY2MwMTAwMjYxMDA1MwkAAZEAAAACBQAAAA8kbGlzdDEwMDI2MTAwNTMAAAAAAAAAAAADCQAAAAAAAAIFAAAADyRzaXplMTAwMjYxMDA1MwAAAAAAAAAAAQUAAAAPJGFjYzExMDAyNjEwMDUzBAAAAA8kYWNjMjEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAADyRhY2MxMTAwMjYxMDA1MwkAAZEAAAACBQAAAA8kbGlzdDEwMDI2MTAwNTMAAAAAAAAAAAEDCQAAAAAAAAIFAAAADyRzaXplMTAwMjYxMDA1MwAAAAAAAAAAAgUAAAAPJGFjYzIxMDAyNjEwMDUzBAAAAA8kYWNjMzEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAADyRhY2MyMTAwMjYxMDA1MwkAAZEAAAACBQAAAA8kbGlzdDEwMDI2MTAwNTMAAAAAAAAAAAIDCQAAAAAAAAIFAAAADyRzaXplMTAwMjYxMDA1MwAAAAAAAAAAAwUAAAAPJGFjYzMxMDAyNjEwMDUzBAAAAA8kYWNjNDEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAADyRhY2MzMTAwMjYxMDA1MwkAAZEAAAACBQAAAA8kbGlzdDEwMDI2MTAwNTMAAAAAAAAAAAMDCQAAAAAAAAIFAAAADyRzaXplMTAwMjYxMDA1MwAAAAAAAAAABAUAAAAPJGFjYzQxMDAyNjEwMDUzBAAAAA8kYWNjNTEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAADyRhY2M0MTAwMjYxMDA1MwkAAZEAAAACBQAAAA8kbGlzdDEwMDI2MTAwNTMAAAAAAAAAAAQDCQAAAAAAAAIFAAAADyRzaXplMTAwMjYxMDA1MwAAAAAAAAAABQUAAAAPJGFjYzUxMDAyNjEwMDUzBAAAAA8kYWNjNjEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAADyRhY2M1MTAwMjYxMDA1MwkAAZEAAAACBQAAAA8kbGlzdDEwMDI2MTAwNTMAAAAAAAAAAAUDCQAAAAAAAAIFAAAADyRzaXplMTAwMjYxMDA1MwAAAAAAAAAABgUAAAAPJGFjYzYxMDAyNjEwMDUzBAAAAA8kYWNjNzEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAADyRhY2M2MTAwMjYxMDA1MwkAAZEAAAACBQAAAA8kbGlzdDEwMDI2MTAwNTMAAAAAAAAAAAYDCQAAAAAAAAIFAAAADyRzaXplMTAwMjYxMDA1MwAAAAAAAAAABwUAAAAPJGFjYzcxMDAyNjEwMDUzBAAAAA8kYWNjODEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAADyRhY2M3MTAwMjYxMDA1MwkAAZEAAAACBQAAAA8kbGlzdDEwMDI2MTAwNTMAAAAAAAAAAAcDCQAAAAAAAAIFAAAADyRzaXplMTAwMjYxMDA1MwAAAAAAAAAACAUAAAAPJGFjYzgxMDAyNjEwMDUzBAAAAA8kYWNjOTEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAADyRhY2M4MTAwMjYxMDA1MwkAAZEAAAACBQAAAA8kbGlzdDEwMDI2MTAwNTMAAAAAAAAAAAgDCQAAAAAAAAIFAAAADyRzaXplMTAwMjYxMDA1MwAAAAAAAAAACQUAAAAPJGFjYzkxMDAyNjEwMDUzBAAAABAkYWNjMTAxMDAyNjEwMDUzCQEAAAAIZm9sZEZ1bmMAAAACBQAAAA8kYWNjOTEwMDI2MTAwNTMJAAGRAAAAAgUAAAAPJGxpc3QxMDAyNjEwMDUzAAAAAAAAAAAJAwkAAAAAAAACBQAAAA8kc2l6ZTEwMDI2MTAwNTMAAAAAAAAAAAoFAAAAECRhY2MxMDEwMDI2MTAwNTMEAAAAECRhY2MxMTEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAAECRhY2MxMDEwMDI2MTAwNTMJAAGRAAAAAgUAAAAPJGxpc3QxMDAyNjEwMDUzAAAAAAAAAAAKAwkAAAAAAAACBQAAAA8kc2l6ZTEwMDI2MTAwNTMAAAAAAAAAAAsFAAAAECRhY2MxMTEwMDI2MTAwNTMEAAAAECRhY2MxMjEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAAECRhY2MxMTEwMDI2MTAwNTMJAAGRAAAAAgUAAAAPJGxpc3QxMDAyNjEwMDUzAAAAAAAAAAALAwkAAAAAAAACBQAAAA8kc2l6ZTEwMDI2MTAwNTMAAAAAAAAAAAwFAAAAECRhY2MxMjEwMDI2MTAwNTMEAAAAECRhY2MxMzEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAAECRhY2MxMjEwMDI2MTAwNTMJAAGRAAAAAgUAAAAPJGxpc3QxMDAyNjEwMDUzAAAAAAAAAAAMAwkAAAAAAAACBQAAAA8kc2l6ZTEwMDI2MTAwNTMAAAAAAAAAAA0FAAAAECRhY2MxMzEwMDI2MTAwNTMEAAAAECRhY2MxNDEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAAECRhY2MxMzEwMDI2MTAwNTMJAAGRAAAAAgUAAAAPJGxpc3QxMDAyNjEwMDUzAAAAAAAAAAANAwkAAAAAAAACBQAAAA8kc2l6ZTEwMDI2MTAwNTMAAAAAAAAAAA4FAAAAECRhY2MxNDEwMDI2MTAwNTMEAAAAECRhY2MxNTEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAAECRhY2MxNDEwMDI2MTAwNTMJAAGRAAAAAgUAAAAPJGxpc3QxMDAyNjEwMDUzAAAAAAAAAAAOAwkAAAAAAAACBQAAAA8kc2l6ZTEwMDI2MTAwNTMAAAAAAAAAAA8FAAAAECRhY2MxNTEwMDI2MTAwNTMEAAAAECRhY2MxNjEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAAECRhY2MxNTEwMDI2MTAwNTMJAAGRAAAAAgUAAAAPJGxpc3QxMDAyNjEwMDUzAAAAAAAAAAAPAwkAAAAAAAACBQAAAA8kc2l6ZTEwMDI2MTAwNTMAAAAAAAAAABAFAAAAECRhY2MxNjEwMDI2MTAwNTMEAAAAECRhY2MxNzEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAAECRhY2MxNjEwMDI2MTAwNTMJAAGRAAAAAgUAAAAPJGxpc3QxMDAyNjEwMDUzAAAAAAAAAAAQAwkAAAAAAAACBQAAAA8kc2l6ZTEwMDI2MTAwNTMAAAAAAAAAABEFAAAAECRhY2MxNzEwMDI2MTAwNTMEAAAAECRhY2MxODEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAAECRhY2MxNzEwMDI2MTAwNTMJAAGRAAAAAgUAAAAPJGxpc3QxMDAyNjEwMDUzAAAAAAAAAAARAwkAAAAAAAACBQAAAA8kc2l6ZTEwMDI2MTAwNTMAAAAAAAAAABIFAAAAECRhY2MxODEwMDI2MTAwNTMEAAAAECRhY2MxOTEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAAECRhY2MxODEwMDI2MTAwNTMJAAGRAAAAAgUAAAAPJGxpc3QxMDAyNjEwMDUzAAAAAAAAAAASAwkAAAAAAAACBQAAAA8kc2l6ZTEwMDI2MTAwNTMAAAAAAAAAABMFAAAAECRhY2MxOTEwMDI2MTAwNTMEAAAAECRhY2MyMDEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAAECRhY2MxOTEwMDI2MTAwNTMJAAGRAAAAAgUAAAAPJGxpc3QxMDAyNjEwMDUzAAAAAAAAAAATAwkAAAAAAAACBQAAAA8kc2l6ZTEwMDI2MTAwNTMAAAAAAAAAABQFAAAAECRhY2MyMDEwMDI2MTAwNTMEAAAAECRhY2MyMTEwMDI2MTAwNTMJAQAAAAhmb2xkRnVuYwAAAAIFAAAAECRhY2MyMDEwMDI2MTAwNTMJAAGRAAAAAgUAAAAPJGxpc3QxMDAyNjEwMDUzAAAAAAAAAAAUCQAAAgAAAAECAAAAE0xpc3Qgc2l6ZSBleGNlZWQgMjABAAAADGRvVGFrZVByb2ZpdAAAAAEAAAAGbGVuZGVyBAAAAANzdHIJAAJYAAAAAQgFAAAABmxlbmRlcgAAAAVieXRlcwkBAAAADFNjcmlwdFJlc3VsdAAAAAIJAQAAAAhXcml0ZVNldAAAAAEJAARMAAAAAgkBAAAACURhdGFFbnRyeQAAAAIJAQAAAAxwcm9maXRGb3JLZXkAAAABBQAAAANzdHIAAAAAAAAAAAAJAARMAAAAAgkBAAAACURhdGFFbnRyeQAAAAIJAQAAABZ1bmNsYWltZWREZXBvc2l0Rm9yS2V5AAAAAQUAAAADc3RyAAAAAAAAAAAABQAAAANuaWwJAQAAAAtUcmFuc2ZlclNldAAAAAEJAARMAAAAAgkBAAAADlNjcmlwdFRyYW5zZmVyAAAAAwUAAAAGbGVuZGVyCQEAAAAJcHJvZml0Rm9yAAAAAQUAAAADc3RyBQAAAAphc3NldFRva2VuCQAETAAAAAIJAQAAAA5TY3JpcHRUcmFuc2ZlcgAAAAMFAAAABmxlbmRlcgkBAAAAE3VuY2xhaW1lZERlcG9zaXRGb3IAAAABBQAAAANzdHIFAAAADGRlcG9zaXRUb2tlbgUAAAADbmlsAAAADQAAAAFpAQAAAARpbml0AAAACgAAAAVvd25lcgAAAAV0b2tlbgAAAAZvcmFjbGUAAAAHbWF4UmF0ZQAAAAhkaXNjb3VudAAAAAVncmFjZQAAAAhpbnRlcmVzdAAAAAhidXJuZG93bgAAAApzZXJ2aWNlRmVlAAAACGxlbmRTaXplAwkAAGYAAAACBQAAAAhpbnRlcmVzdAUAAAAIYnVybmRvd24JAAACAAAAAQIAAAAqaW50ZXJlc3QgbXVzdCBiZSBsZXNzIG9yIGVxdWFsIHRvIGJ1cm5kb3duAwkBAAAAAiE9AAAAAggFAAAAAWkAAAAGY2FsbGVyBQAAAAR0aGlzCQAAAgAAAAECAAAAGW9ubHkgZGFwcCBpdHNlbGYgY2FuIGluaXQJAQAAAAhXcml0ZVNldAAAAAEJAARMAAAAAgkBAAAACURhdGFFbnRyeQAAAAIFAAAACGFkbWluS2V5BQAAAAVvd25lcgkABEwAAAACCQEAAAAJRGF0YUVudHJ5AAAAAgUAAAANYXNzZXRUb2tlbktleQUAAAAFdG9rZW4JAARMAAAAAgkBAAAACURhdGFFbnRyeQAAAAIFAAAACW9yYWNsZUtleQUAAAAGb3JhY2xlCQAETAAAAAIJAQAAAAlEYXRhRW50cnkAAAACBQAAAAptYXhSYXRlS2V5BQAAAAdtYXhSYXRlCQAETAAAAAIJAQAAAAlEYXRhRW50cnkAAAACBQAAABVkaXNjb3VudFBlcmNlbnRpbGVLZXkFAAAACGRpc2NvdW50CQAETAAAAAIJAQAAAAlEYXRhRW50cnkAAAACBQAAAA5ncmFjZVBlcmlvZEtleQUAAAAFZ3JhY2UJAARMAAAAAgkBAAAACURhdGFFbnRyeQAAAAIFAAAAEWludGVyZXN0UGVyaW9kS2V5BQAAAAhpbnRlcmVzdAkABEwAAAACCQEAAAAJRGF0YUVudHJ5AAAAAgUAAAARYnVybmRvd25QZXJpb2RLZXkFAAAACGJ1cm5kb3duCQAETAAAAAIJAQAAAAlEYXRhRW50cnkAAAACBQAAABdzZXJ2aWNlRmVlUGVyY2VudGlsZUtleQUAAAAKc2VydmljZUZlZQkABEwAAAACCQEAAAAJRGF0YUVudHJ5AAAAAgUAAAALbGVuZFNpemVLZXkFAAAACGxlbmRTaXplBQAAAANuaWwAAAABaQEAAAAMdXBkYXRlUGFyYW1zAAAACAAAAAZvcmFjbGUAAAAHbWF4UmF0ZQAAAAhkaXNjb3VudAAAAAVncmFjZQAAAAhpbnRlcmVzdAAAAAhidXJuZG93bgAAAApzZXJ2aWNlRmVlAAAACGxlbmRTaXplAwkAAGYAAAACBQAAAAhpbnRlcmVzdAUAAAAIYnVybmRvd24JAAACAAAAAQIAAAAqaW50ZXJlc3QgbXVzdCBiZSBsZXNzIG9yIGVxdWFsIHRvIGJ1cm5kb3duAwkBAAAAAiE9AAAAAggFAAAAAWkAAAAGY2FsbGVyBQAAAAVvd25lcgkAAAIAAAABAgAAABxvbmx5IG93bmVyIGNhbiB1cGRhdGUgcGFyYW1zCQEAAAAIV3JpdGVTZXQAAAABCQAETAAAAAIJAQAAAAlEYXRhRW50cnkAAAACBQAAAAlvcmFjbGVLZXkFAAAABm9yYWNsZQkABEwAAAACCQEAAAAJRGF0YUVudHJ5AAAAAgUAAAAKbWF4UmF0ZUtleQUAAAAHbWF4UmF0ZQkABEwAAAACCQEAAAAJRGF0YUVudHJ5AAAAAgUAAAAVZGlzY291bnRQZXJjZW50aWxlS2V5BQAAAAhkaXNjb3VudAkABEwAAAACCQEAAAAJRGF0YUVudHJ5AAAAAgUAAAAOZ3JhY2VQZXJpb2RLZXkFAAAABWdyYWNlCQAETAAAAAIJAQAAAAlEYXRhRW50cnkAAAACBQAAABFpbnRlcmVzdFBlcmlvZEtleQUAAAAIaW50ZXJlc3QJAARMAAAAAgkBAAAACURhdGFFbnRyeQAAAAIFAAAAEWJ1cm5kb3duUGVyaW9kS2V5BQAAAAhidXJuZG93bgkABEwAAAACCQEAAAAJRGF0YUVudHJ5AAAAAgUAAAAXc2VydmljZUZlZVBlcmNlbnRpbGVLZXkFAAAACnNlcnZpY2VGZWUJAARMAAAAAgkBAAAACURhdGFFbnRyeQAAAAIFAAAAC2xlbmRTaXplS2V5BQAAAAhsZW5kU2l6ZQUAAAADbmlsAAAAAWkBAAAABmJvcnJvdwAAAAAEAAAABnJlbnRlcgkAAlgAAAABCAgFAAAAAWkAAAAGY2FsbGVyAAAABWJ5dGVzAwkBAAAAASEAAAABBQAAAA9uZXdMb2Fuc0VuYWJsZWQJAAACAAAAAQIAAAAeTmV3IGxvYW5zIHRlbXBvcmFyaWx5IGRpc2FibGVkAwkBAAAACmlzTGVuZE9wZW4AAAABBQAAAAZyZW50ZXIJAAACAAAAAQkAASwAAAACBQAAAAZyZW50ZXICAAAAGSBhbHJlYWR5IGhhcyBhbiBvcGVuIGxvYW4EAAAAByRtYXRjaDAIBQAAAAFpAAAAB3BheW1lbnQDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAAD0F0dGFjaGVkUGF5bWVudAQAAAABYQUAAAAHJG1hdGNoMAMJAAAAAAAAAggFAAAAAWEAAAAHYXNzZXRJZAUAAAAMZGVwb3NpdFRva2VuBAAAAA1jdXJyZW50SGVpZ2h0BQAAAAZoZWlnaHQEAAAACmVuZE9mR3JhY2UJAABkAAAAAgUAAAAGaGVpZ2h0BQAAAAtncmFjZVBlcmlvZAQAAAANZW5kT2ZJbnRlcmVzdAkAAGQAAAACBQAAAAplbmRPZkdyYWNlBQAAAA5pbnRlcmVzdFBlcmlvZAQAAAANZW5kT2ZCdXJuZG93bgkAAGQAAAACBQAAAAplbmRPZkdyYWNlBQAAAA5idXJuZG93blBlcmlvZAQAAAANZGVwb3NpdEFtb3VudAgFAAAAAWEAAAAGYW1vdW50BAAAAA9hc3NldFRva2Vuc0xlbnQJAABrAAAAAwUAAAANZGVwb3NpdEFtb3VudAkAAGgAAAACBQAAAARyYXRlBQAAABJkaXNjb3VudFBlcmNlbnRpbGUJAABoAAAAAgUAAAAEdGVuOAAAAAAAAAAAZAMJAABmAAAAAgUAAAAPYXNzZXRUb2tlbnNMZW50AAAAAAAAAAAABAAAAAVkYXRhcwkBAAAACFdyaXRlU2V0AAAAAQkABEwAAAACCQEAAAAJRGF0YUVudHJ5AAAAAgkBAAAACnN0YXJ0T2ZLZXkAAAABBQAAAAZyZW50ZXIFAAAADWN1cnJlbnRIZWlnaHQJAARMAAAAAgkBAAAACURhdGFFbnRyeQAAAAIJAQAAAA9lbmRPZkdyYWNlT2ZLZXkAAAABBQAAAAZyZW50ZXIFAAAACmVuZE9mR3JhY2UJAARMAAAAAgkBAAAACURhdGFFbnRyeQAAAAIJAQAAABJlbmRPZkludGVyZXN0T2ZLZXkAAAABBQAAAAZyZW50ZXIFAAAADWVuZE9mSW50ZXJlc3QJAARMAAAAAgkBAAAACURhdGFFbnRyeQAAAAIJAQAAABJlbmRPZkJ1cm5kb3duT2ZLZXkAAAABBQAAAAZyZW50ZXIFAAAADWVuZE9mQnVybmRvd24JAARMAAAAAgkBAAAACURhdGFFbnRyeQAAAAIJ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"
    Post(routePath("/script/meta"), dAppBase64) ~> route ~> check {
      val json = responseAs[JsValue]
      json("version").as[String] shouldBe "1"
      json("callableFuncTypes") shouldBe JsObject(
        Seq(
          "init" -> JsArray(
            Seq(
              JsObject(Seq("name" -> JsString("owner"), "type" -> JsString("String"))),
              JsObject(Seq("name" -> JsString("token"), "type" -> JsString("String"))),
              JsObject(Seq("name" -> JsString("oracle"), "type" -> JsString("String"))),
              JsObject(Seq("name" -> JsString("maxRate"), "type" -> JsString("Int"))),
              JsObject(Seq("name" -> JsString("discount"), "type" -> JsString("Int"))),
              JsObject(Seq("name" -> JsString("grace"), "type" -> JsString("Int"))),
              JsObject(Seq("name" -> JsString("interest"), "type" -> JsString("Int"))),
              JsObject(Seq("name" -> JsString("burndown"), "type" -> JsString("Int"))),
              JsObject(Seq("name" -> JsString("serviceFee"), "type" -> JsString("Int"))),
              JsObject(Seq("name" -> JsString("lendSize"), "type" -> JsString("Int")))
            )
          ),
          "updateParams" -> JsArray(
            Seq(
              JsObject(Seq("name" -> JsString("oracle"), "type" -> JsString("String"))),
              JsObject(Seq("name" -> JsString("maxRate"), "type" -> JsString("Int"))),
              JsObject(Seq("name" -> JsString("discount"), "type" -> JsString("Int"))),
              JsObject(Seq("name" -> JsString("grace"), "type" -> JsString("Int"))),
              JsObject(Seq("name" -> JsString("interest"), "type" -> JsString("Int"))),
              JsObject(Seq("name" -> JsString("burndown"), "type" -> JsString("Int"))),
              JsObject(Seq("name" -> JsString("serviceFee"), "type" -> JsString("Int"))),
              JsObject(Seq("name" -> JsString("lendSize"), "type" -> JsString("Int")))
            )
          ),
          "borrow"  -> JsArray(),
          "buyBack" -> JsArray(),
          "closeExpiredFor" -> JsArray(
            Seq(
              JsObject(Seq("name" -> JsString("address"), "type" -> JsString("String")))
            )
          ),
          "discard" -> JsArray(),
          "sendProfit" -> JsArray(
            Seq(
              JsObject(Seq("name" -> JsString("lender"), "type" -> JsString("String")))
            )
          ),
          "takeProfit" -> JsArray(),
          "enableLending" -> JsArray(
            Seq(
              JsObject(Seq("name" -> JsString("b"), "type" -> JsString("Boolean")))
            )
          ),
          "depositBtc"  -> JsArray(),
          "withdrawBtc" -> JsArray(),
          "enableDepositBtc" -> JsArray(
            Seq(
              JsObject(Seq("name" -> JsString("b"), "type" -> JsString("Boolean")))
            )
          ),
          "enableNewLoans" -> JsArray(
            Seq(
              JsObject(Seq("name" -> JsString("b"), "type" -> JsString("Boolean")))
            )
          )
        )
      )
    }
  }

  private def testdAppDirective(str: String) =
    Post(routePath("/script/decompile"), str) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "STDLIB_VERSION").as[Int] shouldBe 3
      (json \ "CONTENT_TYPE").as[String] shouldBe "DAPP"
      (json \ "SCRIPT_TYPE").as[String] shouldBe "ACCOUNT"

      val expectedResult =
        "{-# STDLIB_VERSION 3 #-}\n{-# SCRIPT_TYPE ACCOUNT #-}\n{-# CONTENT_TYPE DAPP #-}\n\n\n\n@Verifier(tx)\nfunc verify () = true\n"
      (json \ "script").as[String] shouldBe expectedResult
    }

  routePath("/script/compile") in {
    Post(routePath("/script/compile"), "{-# STDLIB_VERSION 2 #-}\n(1 == 2)") ~> route ~> check {
      val json           = responseAs[JsValue]
      val expectedScript = ExprScript(V2, script).explicitGet()

      Script.fromBase64String((json \ "script").as[String]) shouldBe Right(expectedScript)
      (json \ "complexity").as[Long] shouldBe 3
      (json \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }

    Post(routePath("/script/compile"), badScript) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "error").as[Int] shouldBe 305
      (json \ "message").as[String] shouldBe "Script estimation was interrupted"
    }
  }

  routePath("/script/compileCode") in {
    Post(routePath("/script/compileCode?compact=true"), dAppWithNonCallable) ~> route ~> check {
      responseAs[JsValue] should matchJson("""{
                                             |  "script" : "base64:AAIDAAAAAAAAAAgIASIEdGVzdAAAAAEBAAAAAWEAAAAABgAAAAAAAAAAyF8thg==",
                                             |  "complexity" : 0,
                                             |  "verifierComplexity" : 0,
                                             |  "callableComplexities" : { },
                                             |  "extraFee" : 400000
                                             |}""".stripMargin)

      val script = (responseAs[JsValue] \ "script").as[String]
      inside(Script.fromBase64String(script).explicitGet()) { case ContractScript.ContractScriptImpl(_, expr) =>
        expr.meta.originalNames shouldBe Vector("test")
      }
    }

    Post(routePath("/script/compileCode"), bigSizeDApp) ~> route ~> check {
      responseAs[JsValue] should matchJson("""{
                                             |  "error" : 305,
                                             |  "message" : "Script is too large: 163841 bytes > 163840 bytes"
                                             |}""".stripMargin)
    }

    Post(routePath("/script/compileCode?compact=true"), bigSizeDApp) ~> route ~> check {
      (responseAs[JsValue] \ "script").toOption shouldBe defined
      (responseAs[JsValue] \ "complexity").as[Long] shouldBe 0
      (responseAs[JsValue] \ "verifierComplexity").as[Long] shouldBe 0
      (responseAs[JsValue] \ "extraFee").as[Long] shouldBe 400000
    }

    Post(routePath("/script/compileCode"), "{-# STDLIB_VERSION 2 #-}\n(1 == 2)") ~> route ~> check {
      val json           = responseAs[JsValue]
      val expectedScript = ExprScript(V2, script).explicitGet()

      Script.fromBase64String((json \ "script").as[String]) shouldBe Right(expectedScript)
      (json \ "complexity").as[Long] shouldBe 3
      (json \ "verifierComplexity").as[Long] shouldBe 3
      (json \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }

    Post(routePath("/script/compileCode"), dAppWithFreeVerifier) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "complexity").as[Long] shouldBe 113
      (json \ "verifierComplexity").as[Long] shouldBe 113
      (json \ "callableComplexities").as[Map[String, Int]] shouldBe Map("write" -> 27, "sendAsset" -> 66, "writeAndSendWaves" -> 68)
      (json \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }

    Post(routePath("/script/compileCode"), dAppWithoutVerifier) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "complexity").as[Long] shouldBe 68
      (json \ "verifierComplexity").as[Long] shouldBe 0
      (json \ "callableComplexities").as[Map[String, Int]] shouldBe Map("write" -> 27, "sendAsset" -> 66, "writeAndSendWaves" -> 68)
      (json \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }

    Post(routePath("/script/compileCode"), emptyDApp) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "complexity").as[Long] shouldBe 0
      (json \ "verifierComplexity").as[Long] shouldBe 0
      (json \ "callableComplexities").as[Map[String, Int]] shouldBe Map()
      (json \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }

    Post(routePath("/script/compileCode"), dAppWithPaidVerifier) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "complexity").as[Long] shouldBe 217
      (json \ "verifierComplexity").as[Long] shouldBe 217
      (json \ "callableComplexities").as[Map[String, Int]] shouldBe Map("callable" -> 27)
      (json \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }

    Post(routePath("/script/compileCode"), badScript) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "error").as[Int] shouldBe 305
      (json \ "message").as[String] shouldBe "Script estimation was interrupted"
    }
  }

  routePath(s"/script/compileCode after ${BlockchainFeatures.SynchronousCalls}") in {
    val blockchain = stub[Blockchain]("blockchain")
    val route      = seal(utilsApi.copy(blockchain = blockchain).route)
    (() => blockchain.activatedFeatures).when().returning(Map(BlockchainFeatures.SynchronousCalls.id -> 0))
    (() => blockchain.settings).when().returning(TestSettings.Default.blockchainSettings)

    Post(routePath("/script/compileCode"), dAppWithoutVerifier) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "complexity").as[Long] shouldBe 68
      (json \ "verifierComplexity").as[Long] shouldBe 0
      (json \ "callableComplexities").as[Map[String, Int]] shouldBe Map("write" -> 27, "sendAsset" -> 66, "writeAndSendWaves" -> 68)
      (json \ "extraFee").as[Long] shouldBe 0
    }

    Post(routePath("/script/compileCode"), dAppWithFreeVerifier) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "complexity").as[Long] shouldBe 113
      (json \ "verifierComplexity").as[Long] shouldBe 113
      (json \ "callableComplexities").as[Map[String, Int]] shouldBe Map("write" -> 27, "sendAsset" -> 66, "writeAndSendWaves" -> 68)
      (json \ "extraFee").as[Long] shouldBe 0
    }

    Post(routePath("/script/compileCode"), dAppWithPaidVerifier) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "complexity").as[Long] shouldBe 217
      (json \ "verifierComplexity").as[Long] shouldBe 217
      (json \ "callableComplexities").as[Map[String, Int]] shouldBe Map("callable" -> 27)
      (json \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }

    Post(routePath("/script/compileCode"), freeCall) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "error").as[Int] shouldBe 305
      (json \ "message").as[String] shouldBe "Invoke Expression Transaction is not activated yet"
    }
  }

  routePath(s"/script/compileCode after ${BlockchainFeatures.ContinuationTransaction}") in {
    val blockchain = stub[Blockchain]("blockchain")
    val route      = seal(utilsApi.copy(blockchain = blockchain).route)
    (() => blockchain.activatedFeatures)
      .when()
      .returning(
        Map(BlockchainFeatures.SynchronousCalls.id -> 0, BlockchainFeatures.RideV6.id -> 0, BlockchainFeatures.ContinuationTransaction.id -> 0)
      )

    Post(routePath("/script/compileCode"), freeCall) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "complexity").as[Long] shouldBe 132
      (json \ "verifierComplexity").as[Long] shouldBe 0
      (json \ "callableComplexities").as[Map[String, Int]] shouldBe Map()
      (json \ "extraFee").as[Long] shouldBe 0
    }
  }

  routePath("/script/compileWithImports") in {
    Post(routePath("/script/compileWithImports"), ScriptWithImportsRequest("{-# STDLIB_VERSION 2 #-}\n(1 == 2)")) ~> route ~> check {
      val json           = responseAs[JsValue]
      val expectedScript = ExprScript(V2, script).explicitGet()

      Script.fromBase64String((json \ "script").as[String]) shouldBe Right(expectedScript)
      (json \ "complexity").as[Long] shouldBe 3
      (json \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }

    val request = ScriptWithImportsRequest(
      """
        | {-# SCRIPT_TYPE ACCOUNT #-}
        | {-# IMPORT dir/my/lib.ride #-}
        | let a = 5
        | inc(a) == a + 1
      """.stripMargin,
      Map(
        "dir/my/lib.ride" ->
          """
            | {-# CONTENT_TYPE LIBRARY #-}
            | func inc(a: Int) = a + 1
          """.stripMargin
      )
    )
    Post(routePath("/script/compileWithImports"), request) ~> route ~> check {
      val expectedScript =
        """
          | {-# STDLIB_VERSION 3 #-}
          | {-# SCRIPT_TYPE ACCOUNT #-}
          | func inc(a: Int) = a + 1
          | let a = 5
          | inc(a) == a + 1
        """.stripMargin
      val compiled = ScriptCompiler.compile(expectedScript, ScriptEstimatorV2)

      val json         = responseAs[JsValue]
      val base64Result = Script.fromBase64String((json \ "script").as[String])
      base64Result shouldBe compiled.map(_._1)
      (json \ "complexity").as[Long] shouldBe compiled.map(_._2).explicitGet()
      (json \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }

    Post(routePath("/script/compileWithImports"), ScriptWithImportsRequest(badScript)) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "error").as[Int] shouldBe 305
      (json \ "message").as[String] shouldBe "Script estimation was interrupted"
    }
  }

  routePath("/script/estimate") in {
    val base64 = ExprScript(script).explicitGet().bytes().base64

    Post(routePath("/script/estimate"), base64) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "script").as[String] shouldBe base64
      (json \ "scriptText").as[String] shouldBe "FUNCTION_CALL(Native(0),List(1, 2))" // [WAIT] s"(1 == 2)"
      (json \ "complexity").as[Long] shouldBe 3
      (json \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }

    def dAppToBase64(dApp: String) =
      (for {
        compiled   <- TestCompiler(V3).compile(dApp)
        serialized <- Global.serializeContract(compiled, V3)
      } yield ByteStr(serialized).base64)
        .explicitGet()

    val dAppBase64                = dAppToBase64(dAppWithFreeVerifier)
    val dAppWithoutVerifierBase64 = dAppToBase64(dAppWithoutVerifier)
    val emptyDAppBase64           = dAppToBase64(emptyDApp)

    Post(routePath("/script/estimate"), dAppBase64) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "script").as[String] shouldBe dAppBase64
      (json \ "complexity").as[Long] shouldBe 113
      (json \ "verifierComplexity").as[Long] shouldBe 113
      (json \ "callableComplexities").as[Map[String, Int]] shouldBe Map("write" -> 27, "sendAsset" -> 66, "writeAndSendWaves" -> 68)
      (json \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }

    Post(routePath("/script/estimate"), dAppWithoutVerifierBase64) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "script").as[String] shouldBe dAppWithoutVerifierBase64
      (json \ "complexity").as[Long] shouldBe 68
      (json \ "verifierComplexity").as[Long] shouldBe 0
      (json \ "callableComplexities").as[Map[String, Int]] shouldBe Map("write" -> 27, "sendAsset" -> 66, "writeAndSendWaves" -> 68)
      (json \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }

    Post(routePath("/script/estimate"), emptyDAppBase64) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "script").as[String] shouldBe emptyDAppBase64
      (json \ "complexity").as[Long] shouldBe 0
      (json \ "verifierComplexity").as[Long] shouldBe 0
      (json \ "callableComplexities").as[Map[String, Int]] shouldBe Map()
      (json \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }

    Post(routePath("/script/estimate"), badScriptBase64) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "error").as[Int] shouldBe 305
      (json \ "message").as[String] shouldBe "Script estimation was interrupted"
    }
  }

  routePath(s"/script/estimate after ${BlockchainFeatures.RideV6}") in {
    val blockchain = stub[Blockchain]("blockchain")
    val route      = seal(utilsApi.copy(blockchain = blockchain).route)
    (() => blockchain.activatedFeatures).when().returning(Map(BlockchainFeatures.SynchronousCalls.id -> 0, BlockchainFeatures.RideV6.id -> 0))

    Post(routePath("/script/estimate"), freeCallExpr) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "complexity").as[Long] shouldBe 132
      (json \ "verifierComplexity").as[Long] shouldBe 132
      (json \ "callableComplexities").as[Map[String, Int]] shouldBe Map()
      (json \ "extraFee").as[Long] shouldBe 0
    }
  }

  routePath("/seed") in {
    Get(routePath("/seed")) ~> route ~> check {
      val seed = Base58.tryDecodeWithLimit((responseAs[JsValue] \ "seed").as[String])
      seed.get.length shouldEqual UtilsApiRoute.DefaultSeedSize
    }
  }

  routePath("/seed/{length}") in forAll(Gen.posNum[Int]) { l =>
    if (l > UtilsApiRoute.MaxSeedSize) {
      Get(routePath(s"/seed/$l")) ~> route should produce(TooBigArrayAllocation)
    } else {
      Get(routePath(s"/seed/$l")) ~> route ~> check {
        val seed = Base58.tryDecodeWithLimit((responseAs[JsValue] \ "seed").as[String])
        seed.get.length shouldEqual l
      }
    }
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
         |func testSyncinvoke() = {
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
        val api        = utilsApi.copy(blockchain = blockchain)
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
          responseAs[JsValue] should matchJson(
            """{"result":{"type":"Array","value":[{"type":"BinaryEntry","value":{"key":{"type":"String","value":"test"},"value":{"type":"ByteVector","value":"11111111111111111111111111"}}}]},"complexity":5,"expr":"testCallable()","address":"3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"}"""
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

        evalScript(""" testSyncinvoke() """) ~> route ~> check {
          responseAs[JsValue] should matchJson("""{
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
                                                 |  "expr" : " testSyncinvoke() ",
                                                 |  "address" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"
                                                 |}""".stripMargin)
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
          responseAs[JsValue] should matchJson(
            """{"error":306,"message":"InvokeRejectError(error = Passed args (bytes, abc) are unsuitable for constructor BinaryEntry(String, ByteVector), log = \n\ttestWriteEntryType.@args = [\n\t\t\"abc\"\n\t]\n\tb = \"abc\"\n\tBinaryEntry.@args = [\n\t\t\"bytes\",\n\t\t\"abc\"\n\t]\n)","expr":" testWriteEntryType(\"abc\") ","address":"3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"}"""
          )
        }
        evalScript(""" testWriteEntryType(base58'aaaa') """) ~> route ~> check {
          responseAs[JsValue] should matchJson(
            """{"result":{"type":"Array","value":[{"type":"BinaryEntry","value":{"key":{"type":"String","value":"bytes"},"value":{"type":"ByteVector","value":"aaaa"}}}]},"complexity":3,"expr":" testWriteEntryType(base58'aaaa') ","address":"3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"}"""
          )
        }

        evalScript(s"""parseBigIntValue("${PureContext.BigIntMax}")""") ~> route ~> check {
          responseAs[JsValue] should matchJson(
            s"""{"result":{"type":"BigInt","value":${PureContext.BigIntMax}},"complexity":65,"expr":"parseBigIntValue(\\"${PureContext.BigIntMax}\\")","address":"3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9"}"""
          )
        }

        val dAppAccount2 = TxHelpers.secondSigner
        val dAppAddress2 = TxHelpers.secondAddress
        d.helpers.creditWavesFromDefaultSigner(dAppAddress2)
        val testScript2 = TxHelpers.scriptV5(s"""@Callable(i)
                                                | func callable() = {
                                                |   strict a = sigVerify(base58'', base58'', base58'')
                                                |   strict r = Address(base58'$dAppAddress').invoke("testCallable", [], [AttachedPayment(unit, 100)])
                                                |   [BinaryEntry("testSyncInvoke", i.caller.bytes)]
                                                | }""".stripMargin)
        d.helpers.setScript(dAppAccount2, testScript2)

        evalScript(""" callable() """, dAppAddress2) ~> route ~> check {
          responseAs[JsValue] should matchJson(
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
               |    "value": 25999
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
               |    "value": 25998
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
               |    "value": 25997
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
               |    "value": 25996
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
               |    "value": 25995
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
               |    "value": 25994
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
               |    "value": 25993
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
               |    "value": 25992
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
               |    "value": 25991
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
               |    "value": 25990
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
               |    "value": 25989
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
               |    "value": 25988
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
               |    "value": 25987
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
               |    "value": 25986
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
               |    "value": 25985
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
               |    "value": 25984
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

  for (
    (hash, f) <- Seq[(String, String => Array[Byte])](
      "secure" -> crypto.secureHash,
      "fast"   -> crypto.fastHash
    )
  ) {
    val uri = routePath(s"/hash/$hash")
    uri in {
      forAll(Gen.alphaNumStr) { s =>
        Post(uri, s) ~> route ~> check {
          val r = responseAs[JsObject]
          (r \ "message").as[String] shouldEqual s
          (r \ "hash").as[String] shouldEqual Base58.encode(f(s))
        }
      }
    }
  }
}
