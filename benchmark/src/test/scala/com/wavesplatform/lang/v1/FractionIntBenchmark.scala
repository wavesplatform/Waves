package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.Common
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, V5, V6}
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.LogExtraInfo
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
class FractionIntBenchmark {
  @Benchmark
  def fraction1(bh: Blackhole, s: St): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr1, LogExtraInfo(), V5, true, true))

  @Benchmark
  def fraction2(bh: Blackhole, s: St): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr2, LogExtraInfo(), V5, true, true))

  @Benchmark
  def fraction3(bh: Blackhole, s: St): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr3, LogExtraInfo(), V5, true, true))

  @Benchmark
  def fraction1Round(bh: Blackhole, s: St): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr1Round, LogExtraInfo(), V5, true, true))

  @Benchmark
  def fraction2Round(bh: Blackhole, s: St): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr2Round, LogExtraInfo(), V5, true, true))

  @Benchmark
  def fraction3Round(bh: Blackhole, s: St): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr3Round, LogExtraInfo(), V5, true, true))
}

@State(Scope.Benchmark)
class St {
  val ds  = DirectiveSet(V6, Account, Expression).fold(null, identity)
  val ctx = lazyContexts((ds, true, true)).value().evaluationContext(Common.emptyBlockchainEnvironment())

  val max     = Long.MaxValue
  val maxSqrt = 3037000499L

  val expr1 = TestCompiler(V6).compileExpression(s"fraction($max / 2, 3, 3)").expr
  val expr2 = TestCompiler(V6).compileExpression(s"fraction($max, -$max, -$max)").expr
  val expr3 = TestCompiler(V6).compileExpression(s"fraction($maxSqrt, $maxSqrt, $maxSqrt)").expr

  val expr1Round = TestCompiler(V6).compileExpression(s"fraction($max / 2, 3, 3, HALFEVEN)").expr
  val expr2Round = TestCompiler(V6).compileExpression(s"fraction($max, -$max, -$max, HALFEVEN)").expr
  val expr3Round = TestCompiler(V6).compileExpression(s"fraction($maxSqrt, $maxSqrt, $maxSqrt, HALFEVEN)").expr
}
