package com.wavesplatform.lang.v1.estimator

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.produce
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.utils.functionCosts
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.FunctionIds

class ScriptEstimatorV3Test extends ScriptEstimatorTestBase(ScriptEstimatorV3) {
  property("multiple func calls") {
    val script =
      """
        | func f(a: Int) = a + 1
        | func g(a: Int) = f(1) + f(a)
        | func h(a: Int) = f(1) + g(a) + g(a)
        |
        | let a = 1
        | let b = 1 + 1
        | if (h(a) == f(a) + g(a))
        |   then h(a) + f(a) + g(a)
        |   else h(b) + f(b) + g(b) + 1
        |
        |""".stripMargin

    /*
      cost(f) = 3
      cost(g) = 2 * cost(f) + 3 = 9
      cost(h) = 2 * cost(g) + cost(f) + 5 = 18 + 3 + 5 = 26
      cost(a) = 1
      cost(b) = 1
      cost(cond) = cost(h) + cost(f) + cost(g) + 5 = 43
      cost(then) = cost(h) + cost(f) + cost(g) + 5 = 43
      cost(else) = cost(h) + cost(f) + cost(g) + 7 = 45
    */
    estimate(functionCosts(V3), compile(script)) shouldBe Right(
      43 /* cond         */ +
      45 /* else         */ +
      1  /* if-then-else */ +
      1  /* let a        */ +
      3  /* let b        */
    )
  }

  property("outer refs in conditions") {
    val script =
      """
        | let a = 1 + 1
        | let b = 1
        | let c = 1 + 1 + 1 + 1
        | let d = 1
        | let e = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
        |
        | if (a == b)
        | then {
        |   let x = 1
        |   c + x
        | }
        | else {
        |   let y = 1
        |   d + y + 1
        | }
      """.stripMargin

    estimate(functionCosts(V3), compile(script)) shouldBe Right(
      1 /* if-then-else                      */ +
      3 /* a == b    condition               */ +
      5 /* d + y + 1 expr inside else block  */ +
      1 /* let y     decl inside else block  */ +
      3 /* let a     decl used in condition  */ +
      1 /* let b     decl used in condition  */ +
      7 /* let c     decl used in then       */ +
      1 /* let d     decl used in else       */
        /* let e     unused decl             */
    )
  }

  property("big function call tree") {
    val n = 100
    val script =
     s"""
        | func f0() = 0
        | ${(0 until n).map(i => s"func f${i + 1}() = if (true) then f$i() else f$i()").mkString("\n") }
        | f$n()
      """.stripMargin

    estimate(functionCosts(V3), compile(script)) shouldBe Right(n * 2 + 1)
    /*
      cost(f0) = 1
      cost(f1) = cost(cond) + cost(true) + cost(f0) = 1 + 1 + 1 = 1 * 2 + 1
      cost(f2) = cost(cond) + cost(true) + cost(f1) = 1 + 1 + 3 = 2 * 2 + 1
      cost(fn) = n * 2 + 1
    */
  }

  property("overlapped func") {
    val script =
      """
        | func f() = {
        |   func f() = {
        |     func f() = {
        |       1
        |     }
        |     f()
        |   }
        |   f()
        | }
        | f()
        |
        |""".stripMargin

    estimate(functionCosts(V3), compile(script)) shouldBe Right(1)
  }

  property("different if branches") {
    val script =
      """
        |let a = 1
        |let b = "b"
        |if (a == 1) then {
        |    true
        |} else {
        |    if (a > 1) then {
        |        b == "a"
        |    } else {
        |        false
        |    }
        |}
      """.stripMargin

    estimate(functionCosts(V3), compile(script)) shouldBe Right(
        1 /* let a                      */ +
        1 /* let b                      */ +
        1 /* if-then-else               */ +
        3 /* a == 1         condition   */ +
        1 /* if-then-else               */ +
        3 /* a > 1          condition   */ +
        3 /* b == "a"       condition   */
    )
  }

  property("getter") {
    val script = "lastBlock.height"
    estimate(functionCosts(V3), compile(script)) shouldBe Right(2) /* ref eval and field access */
  }

  property("groth16Verify_Ninputs") {
    implicit val version : StdLibVersion = V4
    val c = Array(1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600)
    for { (c, n) <- c.zipWithIndex } {
      estimate(functionCosts(V4), compile(s"groth16Verify_${n+1}inputs(base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK')")) shouldBe Right(c + 3)
    }
  }

  property("groth16Verify") {
    implicit val version : StdLibVersion = V4
    estimate(functionCosts(V4), compile("groth16Verify(base64'ZGdnZHMK',base64'ZGdnZHMK',base64'ZGdnZHMK')")) shouldBe Right(2703)
  }

  property("free declarations") {
    estimate(
      functionCosts(V4),
      compile(
        """
          |
          | let a = 1 + 1 + 1 + 1 + 1
          | func f() = a
          | func g() = f()
          | let b = g()
          | let c = a
          | func h() = a + b + c
          |
          | 1
          |
        """.stripMargin
      )
    ) shouldBe Right(1)
  }

  property("sync invoke functions are allowed only for dApps") {
    def expr(id: Short) = FUNCTION_CALL(
      Native(id),
      List(
        CaseObj(CASETYPEREF("Address", Nil), Map()),
        CONST_STRING("default").explicitGet(),
        REF("nil"),
        REF("nil")
      )
    )
    estimate(functionCosts(V5), expr(FunctionIds.CALLDAPP)) should produce("not found")
    estimate(functionCosts(V5), expr(FunctionIds.CALLDAPPREENTRANT)) should produce("not found")
    estimate(functionCosts(V5, DApp), expr(FunctionIds.CALLDAPP)) shouldBe Right(79)
    estimate(functionCosts(V5, DApp), expr(FunctionIds.CALLDAPPREENTRANT)) shouldBe Right(79)
  }
}
