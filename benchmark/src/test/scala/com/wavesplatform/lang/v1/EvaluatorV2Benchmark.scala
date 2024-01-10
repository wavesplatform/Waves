package com.wavesplatform.lang.v1

import com.wavesplatform.lang.Common
import com.wavesplatform.lang.directives.values.{V1, V3, V5, V6}
import com.wavesplatform.lang.v1.EvaluatorV2Benchmark.*
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, EXPR, IF, TRUE}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2
import com.wavesplatform.lang.v1.evaluator.ctx.DisabledLogEvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.{DisabledLogEvaluationContext, EvaluationContext, LoggedEvaluationContext}
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit
import scala.annotation.tailrec

object EvaluatorV2Benchmark {
  val pureContext: CTX                       = PureContext.build(V1, useNewPowPrecision = true)
  val pureEvalContext: EvaluationContext[Id] = pureContext.evaluationContext(Common.emptyBlockchainEnvironment())
  val evaluatorV2: EvaluatorV2               = new EvaluatorV2(DisabledLogEvaluationContext(pureEvalContext), V1, true, true, false)
}

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
class EvaluatorV2Benchmark {
  @Benchmark
  def funcs(st: Funcs, bh: Blackhole): Unit = bh.consume(eval(pureEvalContext, st.expr, V1))

  @Benchmark
  def lets(st: Lets, bh: Blackhole): Unit = bh.consume(eval(pureEvalContext, st.expr, V1))

  @Benchmark
  def custom(st: CustomFunc, bh: Blackhole): Unit = bh.consume(eval(pureEvalContext, st.expr, V1))

  @Benchmark
  def littleCustom(st: LittleCustomFunc, bh: Blackhole): Unit = bh.consume(eval(pureEvalContext, st.expr, V1))

  @Benchmark
  def conditions(st: Conditions, bh: Blackhole): Unit = bh.consume(eval(pureEvalContext, st.expr, V1))

  @Benchmark
  def recFunc(st: RecFunc, bh: Blackhole): Unit = bh.consume {
    val (_, _, res) = eval(pureEvalContext, st.expr, V1)
    require(res == Right(CONST_LONG(13631488)), s"$res")
  }

  @Benchmark
  def overheadCallable(st: OverheadTest, bh: Blackhole): Unit = bh.consume {
    val (_, comp, res) = eval(pureEvalContext, st.expr.expr, V6)
    require((Int.MaxValue - comp) == 1048576, s"$comp")
  }

  @Benchmark
  def mini_funcs(st: Funcs, bh: Blackhole): Unit = bh.consume(miniEv(st.expr, pureEvalContext))

  @Benchmark
  def mini_lets(st: Lets, bh: Blackhole): Unit = bh.consume(miniEv(st.expr, pureEvalContext))

  @Benchmark
  def mini_custom(st: CustomFunc, bh: Blackhole): Unit = bh.consume(miniEv(st.expr, pureEvalContext))

  @Benchmark
  def mini_littleCustom(st: LittleCustomFunc, bh: Blackhole): Unit = bh.consume(miniEv(st.expr, pureEvalContext))

  @Benchmark
  def mini_conditions(st: Conditions, bh: Blackhole): Unit = bh.consume(miniEv(st.expr, pureEvalContext))

  @Benchmark
  def mini_recFunc(st: RecFunc, bh: Blackhole): Unit = bh.consume {
    val (log, spentComplexity, res) = miniEv(st.expr, pureEvalContext)
    require(res == Right(CONST_LONG(13631488)), s"$res")
  }

  @Benchmark
  def mini_overheadCallable(st: OverheadTest, bh: Blackhole): Unit = bh.consume {
    val (_, comp, res) = miniEv(st.expr.expr, pureEvalContext, 52000)
    require(comp == 1048576, s"$comp")
  }
}

@State(Scope.Benchmark)
class Funcs {
  val count = 2000
  val script =
    s"""
       | func a0() = {
       |   1 + 1
       | }
       | ${1 to count map (i => s"func a$i() = a${i - 1}()") mkString "\n"}
       | a$count() == a$count()
      """.stripMargin

  val expr = {
    val sc = TestCompiler(V6).compileExpression(script, checkSize = false)
    sc.expr
  }
}

@State(Scope.Benchmark)
class Lets {
  val count = 5000
  val script =
    s"""
       | let a0 = 1
       | ${1 to count map (i => s"let a$i = a${i - 1} + 1") mkString "\n"}
       | a$count == a$count
      """.stripMargin

  val expr = TestCompiler(V3).compileExpression(script, checkSize = false).expr
}

@State(Scope.Benchmark)
class RecFunc {
  def scriptStr(size: Int) =
    s"""func f1(i: Int) = i + 1
       |${(2 to size)
      .map { i =>
        s"func f$i(${(0 until i).map(idx => s"i$idx: Int").mkString(",")}) = ${(1 until i).map(fi => s"f$fi(${(1 to fi).map(ii => s"i$ii").mkString(",")})").mkString("+")}"
      }
      .mkString("\n")}
       |f${size}(${(1 to size).mkString(",")})
       |""".stripMargin
  private val script: String = scriptStr(22)
  val expr                   = TestCompiler(V6).compileExpression(script, checkSize = false).expr
}

@State(Scope.Benchmark)
class CustomFunc {
  val script =
    s"""
       | func f() = {
       |   let a0 = 0
       |   let b0 = 1
       |   let a1 = b0
       |   let b1 = a0 + b0
       |   let a2 = b1
       |   let b2 = a1 + b1
       |   let a3 = b2
       |   let b3 = a2 + b2
       |   let a4 = b3
       |   let b4 = a3 + b3
       |   let a5 = b4
       |   let b5 = a4 + b4
       |   let a6 = b5
       |   let b6 = a5 + b5
       |   let a7 = b6
       |   let b7 = a6 + b6
       |   let a8 = b7
       |   let b8 = a7 + b7
       |   let a9 = b8
       |   let b9 = a8 + b8
       |   let a10 = b9
       |   let b10 = a9 + b9
       |   let a11 = b10
       |   let b11 = a10 + b10
       |   let a12 = b11
       |   let b12 = a11 + b11
       |   let a13 = b12
       |   let b13 = a12 + b12
       |   let a14 = b13
       |   let b14 = a13 + b13
       |   b14 == 610
       | }
       |
       | f() && f() && f() && f() && f() && f() && f()
      """.stripMargin

  val expr = TestCompiler(V6).compileExpression(script).expr
}

@State(Scope.Benchmark)
class LittleCustomFunc {
  val script =
    s"""
       | func f() = {
       |   let a0 = 0
       |   let b0 = 1
       |   let a1 = b0
       |   let b1 = a0 + b0
       |   let a2 = b1
       |   let b2 = a1 + b1
       |   let a3 = b2
       |   let b3 = a2 + b2
       |   let a4 = b3
       |   let b4 = a3 + b3
       |   let a5 = b4
       |   let b5 = a4 + b4
       |   let a6 = b5
       |   let b6 = a5 + b5
       |   let a7 = b6
       |   let b7 = a6 + b6
       |   let a8 = b7
       |   let b8 = a7 + b7
       |   let a9 = b8
       |   let b9 = a8 + b8
       |   let a10 = b9
       |   let b10 = a9 + b9
       |   let a11 = b10
       |   let b11 = a10 + b10
       |   let a12 = b11
       |   let b12 = a11 + b11
       |   let a13 = b12
       |   let b13 = a12 + b12
       |   let a14 = b13
       |   let b14 = a13 + b13
       |   b14 == 610
       | }
       |
       | f()
      """.stripMargin

  val expr = TestCompiler(V3).compileExpression(script).expr
}

@State(Scope.Benchmark)
class OverheadTest {
  val expr = {
    val n = 20
    val scriptTest =
      s"""
         | func f0() = true
         | ${(0 until n).map(i => s"func f${i + 1}() = if (f$i()) then f$i() else f$i()").mkString("\n")}
         | f$n()
     """.stripMargin
    println(scriptTest)
    TestCompiler(V5).compileExpression(scriptTest)
  }
}

@State(Scope.Benchmark)
class Conditions {
  @tailrec private def build(r: EXPR, count: Int): EXPR =
    if (count > 0) build(IF(TRUE, TRUE, r), count - 1)
    else r

  val expr = build(TRUE, 11000) // ~ 32 KB
}
