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
class PowBigIntBenchmark {
  @Benchmark
  def pow1(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr1, V5))

  @Benchmark
  def pow2(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr2, V5))

  @Benchmark
  def pow3(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr3, V5))

  @Benchmark
  def pow4(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr4, V5))

  @Benchmark
  def pow5(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr5, V5))

  @Benchmark
  def pow6(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr6, V5))

  @Benchmark
  def pow7(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr7, V5))

  @Benchmark
  def pow8(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr8, V5))

  @Benchmark
  def pow9(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr9, V5))

  @Benchmark
  def pow10(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr10, V5))
}

@State(Scope.Benchmark)
class PowBigIntSt {
  val ds  = DirectiveSet(V5, Account, Expression).fold(null, identity)
  val ctx = lazyContexts((ds, true, true)).value().evaluationContext(Common.emptyBlockchainEnvironment())

  val max = PureContext.BigIntMax
  val min = PureContext.BigIntMin
  val one = BigInt(1)

  val d18 = BigInt(987654321012345678L)
  val d19 = BigInt(1987654321012345678L)

  val e1 = BigInt("3259987654320123456789")
  val e2 = BigInt("515598765432101234567")
  val e3 = max / (BigInt(Long.MaxValue).pow(7) / BigInt(4))

  val expr1  = pow(max, 0, max, 18, 18) // ERROR
  val expr2  = pow(max, 0, max, 0, 0)   // ERROR
  val expr3  = pow(min, 0, max, 0, 0)   // ERROR
  val expr4  = pow(max, 0, min, 0, 18)  // ERROR
  val expr5  = pow(one, 18, min, 0, 18) // ERROR
  val expr6  = pow(d18, 18, min, 0, 18) // ERROR
  val expr7  = pow(d18, 18, max, 0, 18) // ERROR
  val expr8  = pow(d18, 18, e3, 18, 18) // 0
  val expr9  = pow(d18, 18, e1, 18, 18) // 2
  val expr10 = pow(d19, 18, e2, 18, 0)  // ≈ max
}
