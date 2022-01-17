package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.Common
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, V6}
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.compiler.TestCompiler
import org.openjdk.jmh.annotations.{State, _}
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
class FractionIntBenchmark {
  @Benchmark
  def fraction1(bh: Blackhole, s: St): Unit = bh.consume(eval(s.ctx, s.expr1, V6))

  @Benchmark
  def fraction2(bh: Blackhole, s: St): Unit = bh.consume(eval(s.ctx, s.expr2, V6))

  @Benchmark
  def fraction3(bh: Blackhole, s: St): Unit = bh.consume(eval(s.ctx, s.expr3, V6))

  @Benchmark
  def fraction1Round(bh: Blackhole, s: St): Unit = bh.consume(eval(s.ctx, s.expr1Round, V6))

  @Benchmark
  def fraction2Round(bh: Blackhole, s: St): Unit = bh.consume(eval(s.ctx, s.expr2Round, V6))

  @Benchmark
  def fraction3Round(bh: Blackhole, s: St): Unit = bh.consume(eval(s.ctx, s.expr3Round, V6))
}

@State(Scope.Benchmark)
class St {
  val ds  = DirectiveSet(V6, Account, Expression).fold(null, identity)
  val ctx = lazyContexts(ds).value().evaluationContext(Common.emptyBlockchainEnvironment())

  val max     = Long.MaxValue
  val maxSqrt = 3037000499L

  val expr1 = TestCompiler(V6).compileExpression(s"fraction($max / 2, 3, 3)").expr.asInstanceOf[EXPR]
  val expr2 = TestCompiler(V6).compileExpression(s"fraction($max, -$max, -$max)").expr.asInstanceOf[EXPR]
  val expr3 = TestCompiler(V6).compileExpression(s"fraction($maxSqrt, $maxSqrt, $maxSqrt)").expr.asInstanceOf[EXPR]

  val expr1Round = TestCompiler(V6).compileExpression(s"fraction($max / 2, 3, 3, HALFEVEN)").expr.asInstanceOf[EXPR]
  val expr2Round = TestCompiler(V6).compileExpression(s"fraction($max, -$max, -$max, HALFEVEN)").expr.asInstanceOf[EXPR]
  val expr3Round = TestCompiler(V6).compileExpression(s"fraction($maxSqrt, $maxSqrt, $maxSqrt, HALFEVEN)").expr.asInstanceOf[EXPR]
}
