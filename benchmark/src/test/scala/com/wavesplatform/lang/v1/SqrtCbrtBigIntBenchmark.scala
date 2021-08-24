package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.Common
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, V5}
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import org.openjdk.jmh.annotations.{State, _}
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
class SqrtCbrtBigIntBenchmark {
  @Benchmark
  def sqrt1(bh: Blackhole, s: SqrtBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr1, V5))

  @Benchmark
  def sqrt2(bh: Blackhole, s: SqrtBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr2, V5))

  @Benchmark
  def cbrt1(bh: Blackhole, s: SqrtBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr3, V5))

  @Benchmark
  def cbrt2(bh: Blackhole, s: SqrtBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr4, V5))
}

@State(Scope.Benchmark)
class SqrtBigIntSt {
  val ds  = DirectiveSet(V5, Account, Expression).fold(null, identity)
  val ctx = lazyContexts(ds).value().evaluationContext(Common.emptyBlockchainEnvironment())

  val max = s"""parseBigIntValue("${PureContext.BigIntMax}")"""
  val min = s"""parseBigIntValue("${PureContext.BigIntMin}")"""

  val e1 = "toBigInt(5)"
  val e2 = "toBigInt(3333333333333333)"

  val expr1 = compile(s"pow($max, 0, $e1, 1, 16, DOWN)")
  val expr2 = compile(s"pow($max, 16, $e1, 1, 16, DOWN)")
  val expr3 = compile(s"pow($max, 0, $e2, 16, 16, DOWN)")
  val expr4 = compile(s"pow($min, 16, $e2, 16, 16, DOWN)")

  private def compile(e: String): EXPR =
    TestCompiler(V5).compileExpression(e).expr.asInstanceOf[EXPR]
}
