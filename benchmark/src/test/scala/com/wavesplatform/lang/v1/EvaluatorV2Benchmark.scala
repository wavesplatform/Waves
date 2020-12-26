package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import cats.Id
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common
import com.wavesplatform.lang.directives.values.V1
import com.wavesplatform.lang.v1.EvaluatorV2Benchmark._
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, LoggedEvaluationContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.Environment
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

object EvaluatorV2Benchmark {
  val pureContext: CTX[Environment]                       = PureContext.build(V1).withEnvironment[Environment]
  val pureEvalContext: EvaluationContext[Environment, Id] = pureContext.evaluationContext(Common.emptyBlockchainEnvironment())
  val evaluatorV2: EvaluatorV2                            = new EvaluatorV2(LoggedEvaluationContext(_ => _ => (), pureEvalContext), V1)
}

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 20)
@Measurement(iterations = 10)
class EvaluatorV2Benchmark {
  @Benchmark
  def funcs(st: Funcs, bh: Blackhole): Unit = bh.consume(evaluatorV2(st.expr, 1000000))

  @Benchmark
  def lets(st: Lets, bh: Blackhole): Unit = bh.consume(evaluatorV2(st.expr, 1000000))

  @Benchmark
  def custom(st: CustomFunc, bh: Blackhole): Unit = bh.consume(evaluatorV2(st.expr, 1000000))

  @Benchmark
  def littleCustom(st: LittleCustomFunc, bh: Blackhole): Unit = bh.consume(evaluatorV2(st.expr, 1000000))
}

@State(Scope.Benchmark)
class Funcs {
  val context = pureEvalContext

  val count = 2000
  val script =
    s"""
       | func a0() = {
       |   1 + 1
       | }
       | ${1 to count map (i => s"func a$i() = a${i - 1}()") mkString "\n"}
       | a$count() == a$count()
      """.stripMargin

  val parsed = Parser.parseExpr(script).get.value
  val expr = ExpressionCompiler(pureContext.compilerContext, parsed).explicitGet()._1
}

@State(Scope.Benchmark)
class Lets {
  val context = pureEvalContext

  val count = 5000
  val script =
    s"""
       | let a0 = 1
       | ${1 to count map (i => s"let a$i = a${i - 1} + 1") mkString "\n"}
       | a$count == a$count
      """.stripMargin

  val parsed = Parser.parseExpr(script).get.value
  val expr   = ExpressionCompiler(pureContext.compilerContext, parsed).explicitGet()._1
}

@State(Scope.Benchmark)
class CustomFunc {
  val context = pureEvalContext

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

  val parsed = Parser.parseExpr(script).get.value
  val expr   = ExpressionCompiler(pureContext.compilerContext, parsed).explicitGet()._1
}

@State(Scope.Benchmark)
class LittleCustomFunc {
  val context = pureEvalContext

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

  val parsed = Parser.parseExpr(script).get.value
  val expr   = ExpressionCompiler(pureContext.compilerContext, parsed).explicitGet()._1
}
