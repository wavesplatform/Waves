package com.wavesplatform.lang.v1.estimator

import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.utils.functionCosts
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3

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

  property("getter") {
    val script = "lastBlock.height"
    estimate(functionCosts(V3), compile(script)) shouldBe Right(2) /* ref eval and field access */
  }
}
