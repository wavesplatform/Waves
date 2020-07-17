package com.wavesplatform.utils
import com.wavesplatform.lang.ScriptMeta.FunctionArgument
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.{EstimateResult, Ride, RideException, Script}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class JavaAdapterTest extends PropSpec with PropertyChecks with Matchers {
  property("all methods with account expression") {
    val expr =
      """
        | {-# STDLIB_VERSION 4 #-}
        | {-# CONTENT_TYPE EXPRESSION #-}
        | {-# SCRIPT_TYPE ACCOUNT #-}
        |
        | true || false
      """.stripMargin


    val script = Ride.compile(expr)
    script.isDApp shouldBe false
    script.isAsset shouldBe false
    script.isExpression shouldBe true

    Script.fromBytes(script.bytes(), false) shouldBe script
    Script.fromBase64String(script.base64String(), false) shouldBe script

    Ride.decompile(script) shouldBe
      """if (true)
        |    then true
        |    else false""".stripMargin

    val estimateResult = Ride.estimate(script)
    estimateResult.getComplexity shouldBe 3
    estimateResult.getCallableComplexities.isEmpty shouldBe true

    (the[RideException] thrownBy Ride.meta(script)).getMessage shouldBe "Expected DApp"
  }

  property("all methods with asset expression") {
    val expr =
      """
        | {-# STDLIB_VERSION 4 #-}
        | {-# CONTENT_TYPE EXPRESSION #-}
        | {-# SCRIPT_TYPE ASSET #-}
        |
        | true || false
      """.stripMargin


    val script = Ride.compile(expr)
    script.isDApp shouldBe false
    script.isAsset shouldBe true
    script.isExpression shouldBe true

    Script.fromBytes(script.bytes(), true) shouldBe script
    Script.fromBase64String(script.base64String(), true) shouldBe script

    Ride.decompile(script) shouldBe
      """if (true)
        |    then true
        |    else false""".stripMargin

    val estimateResult = Ride.estimate(script)
    estimateResult.getComplexity shouldBe 3
    estimateResult.getCallableComplexities.isEmpty shouldBe true

    (the[RideException] thrownBy Ride.meta(script)).getMessage shouldBe "Expected DApp"
  }

  property("all methods with DApp") {
    val dApp =
      """
        | {-# STDLIB_VERSION 4 #-}
        | {-# CONTENT_TYPE DAPP #-}
        | {-# SCRIPT_TYPE ACCOUNT #-}
        |
        | @Callable(inv)
        | func paySelf(asset: String) = {
        |   [StringEntry("key", asset)]
        | }
        |
        | @Verifier(tx)
        | func verify() = true || false
      """.stripMargin


    val script = Ride.compile(dApp)
    script.isDApp shouldBe true
    script.isAsset shouldBe false
    script.isExpression shouldBe false

    Script.fromBytes(script.bytes(), false) shouldBe script
    Script.fromBase64String(script.base64String(), false) shouldBe script

    Ride.decompile(script) shouldBe
      """
        |
        |@Callable(inv)
        |func paySelf (asset) = [StringEntry("key", asset)]
        |
        |
        |@Verifier(tx)
        |func verify () = if (true)
        |    then true
        |    else false
        |""".stripMargin

    val estimateResult = Ride.estimate(script)
    estimateResult.getComplexity shouldBe 3
    estimateResult.getCallableComplexities.get("paySelf") shouldBe 7
    estimateResult.getCallableComplexities.get("verify") shouldBe 3

    Ride.meta(script).getVersion shouldBe 2
    Ride.meta(script).getFunctionSignatures.get(0).getName shouldBe "paySelf"
    Ride.meta(script).getFunctionSignatures.get(0).getArguments.get(0) shouldBe new FunctionArgument("asset", "String")
  }

  property("repl") {
    val repl = Ride.repl(V4)
    repl.execute("let a = 1")
    repl.execute("let b = 2")
    repl.execute("a + b") shouldBe "res1: Int = 3"
    repl.info("b") shouldBe "let b: Int"
    repl.clear()
    repl.info("b") shouldBe "b not found in context"
  }
}
