package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.Common
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, V5}
import com.wavesplatform.lang.utils.lazyContexts
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
  def sqrt1(bh: Blackhole, s: SqrtBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr1, V5))

  @Benchmark
  def sqrt2(bh: Blackhole, s: SqrtBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr2, V5))

  @Benchmark
  def cbrt1(bh: Blackhole, s: SqrtBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr3, V5))

  @Benchmark
  def cbrt2(bh: Blackhole, s: SqrtBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr4, V5))
}

@State(Scope.Benchmark)
class SqrtBigIntSt {
  val ds  = DirectiveSet(V5, Account, Expression).fold(null, identity)
  val ctx = lazyContexts((ds, true, true, true)).value().evaluationContext(Common.emptyBlockchainEnvironment())

  val max = PureContext.BigIntMax
  val min = PureContext.BigIntMin

  val e1 = BigInt(5)
  val e2 = BigInt(3333333333333333L)

  val expr1 = pow(max, 0, e1, 1, 18)
  val expr2 = pow(max, 18, e1, 1, 18)
  val expr3 = pow(max, 0, e2, 16, 18)
  val expr4 = pow(min, 18, e2, 16, 18)
}
