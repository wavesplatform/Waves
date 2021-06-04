package com.wavesplatform.lang.v1

import com.wavesplatform.lang.Common
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, V5}
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2
import org.openjdk.jmh.annotations.{State, _}
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 30)
@Measurement(iterations = 30)
class FractionIntBenchmark {
  @Benchmark
  def fraction1(bh: Blackhole, s: St): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr1, V5))

  @Benchmark
  def fraction2(bh: Blackhole, s: St): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr2, V5))

  @Benchmark
  def fraction3(bh: Blackhole, s: St): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr3, V5))

  @Benchmark
  def fraction1Round(bh: Blackhole, s: St): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr1Round, V5))

  @Benchmark
  def fraction2Round(bh: Blackhole, s: St): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr2Round, V5))

  @Benchmark
  def fraction3Round(bh: Blackhole, s: St): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr3Round, V5))
}

@State(Scope.Benchmark)
class St {
  val ds  = DirectiveSet(V5, Account, Expression).fold(null, identity)
  val ctx = lazyContexts(ds).value().evaluationContext(Common.emptyBlockchainEnvironment())
  val max = Long.MaxValue

  val expr1 = TestCompiler(V5).compileExpression(s"fraction($max, -$max, 1)").expr.asInstanceOf[EXPR]
  val expr2 = TestCompiler(V5).compileExpression(s"fraction($max, -$max, -$max)").expr.asInstanceOf[EXPR]
  val expr3 = TestCompiler(V5).compileExpression(s"fraction($max, $max, ${-max / 2 + 1})").expr.asInstanceOf[EXPR]

  val expr1Round = TestCompiler(V5).compileExpression(s"fraction($max, -$max, 1, HALFEVEN)").expr.asInstanceOf[EXPR]
  val expr2Round = TestCompiler(V5).compileExpression(s"fraction($max, -$max, -$max, HALFEVEN)").expr.asInstanceOf[EXPR]
  val expr3Round = TestCompiler(V5).compileExpression(s"fraction($max, $max, ${-max / 2 + 1}, HALFEVEN)").expr.asInstanceOf[EXPR]
}
