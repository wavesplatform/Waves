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
class FractionBigIntBenchmark {
  @Benchmark
  def fraction1(bh: Blackhole, s: FractionBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr1, V5))

  @Benchmark
  def fraction2(bh: Blackhole, s: FractionBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr2, V5))

  @Benchmark
  def fraction3(bh: Blackhole, s: FractionBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr3, V5))

  @Benchmark
  def fraction1Round(bh: Blackhole, s: FractionBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr1Round, V5))

  @Benchmark
  def fraction2Round(bh: Blackhole, s: FractionBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr2Round, V5))

  @Benchmark
  def fraction3Round(bh: Blackhole, s: FractionBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr3Round, V5))
}

@State(Scope.Benchmark)
class FractionBigIntSt {
  val ds  = DirectiveSet(V5, Account, Expression).fold(null, identity)
  val ctx = lazyContexts(ds).value().evaluationContext(Common.emptyBlockchainEnvironment())

  val max     = s"""parseBigIntValue("${PureContext.BigIntMax}")"""
  val halfMax = s"""parseBigIntValue("${PureContext.BigIntMax}") / toBigInt(2)"""
  val min     = s"""parseBigIntValue("${PureContext.BigIntMin}")"""
  val maxSqrt = s"""parseBigIntValue("57896044618658097711785492504343953926634992332820282019728792003956564819968")"""
  val three   = "toBigInt(3)"

  val expr1 = TestCompiler(V5).compileExpression(s"fraction($halfMax, $three, $three)").expr.asInstanceOf[EXPR]
  val expr2 = TestCompiler(V5).compileExpression(s"fraction($max, $min, $min)").expr.asInstanceOf[EXPR]
  val expr3 = TestCompiler(V5).compileExpression(s"fraction($maxSqrt, $maxSqrt, $maxSqrt)").expr.asInstanceOf[EXPR]

  val expr1Round = TestCompiler(V5).compileExpression(s"fraction($halfMax, $three, $three, HALFEVEN)").expr.asInstanceOf[EXPR]
  val expr2Round = TestCompiler(V5).compileExpression(s"fraction($max, $min, $min, HALFEVEN)").expr.asInstanceOf[EXPR]
  val expr3Round = TestCompiler(V5).compileExpression(s"fraction($maxSqrt, $maxSqrt, $maxSqrt, HALFEVEN)").expr.asInstanceOf[EXPR]
}
