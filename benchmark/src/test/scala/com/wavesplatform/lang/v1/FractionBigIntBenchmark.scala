package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.Common
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, V5}
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BIGINT, FUNCTION_CALL}
import com.wavesplatform.lang.v1.evaluator.FunctionIds.{FRACTION_BIGINT, FRACTION_BIGINT_ROUNDS}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{PureContext, Rounding}
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
class FractionBigIntBenchmark {
  @Benchmark
  def fraction1(bh: Blackhole, s: FractionBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr1, V5))

  @Benchmark
  def fraction2(bh: Blackhole, s: FractionBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr2, V5))

  @Benchmark
  def fraction3(bh: Blackhole, s: FractionBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr3, V5))

  @Benchmark
  def fraction1Round(bh: Blackhole, s: FractionBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr1Round, V5))

  @Benchmark
  def fraction2Round(bh: Blackhole, s: FractionBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr2Round, V5))

  @Benchmark
  def fraction3Round(bh: Blackhole, s: FractionBigIntSt): Unit = bh.consume(eval(s.ctx, s.expr3Round, V5))
}

@State(Scope.Benchmark)
class FractionBigIntSt {
  val ds  = DirectiveSet(V5, Account, Expression).fold(null, identity)
  val ctx = lazyContexts(ds -> true).value().evaluationContext(Common.emptyBlockchainEnvironment())

  val max     = CONST_BIGINT(PureContext.BigIntMax)
  val halfMax = CONST_BIGINT(PureContext.BigIntMax / 2)
  val min     = CONST_BIGINT(PureContext.BigIntMin)
  val maxSqrt = CONST_BIGINT(BigInt("57896044618658097711785492504343953926634992332820282019728792003956564819968"))
  val three   = CONST_BIGINT(3)

  val expr1 = FUNCTION_CALL(
    Native(FRACTION_BIGINT),
    List(halfMax, three, three)
  )

  val expr2 = FUNCTION_CALL(
    Native(FRACTION_BIGINT),
    List(max, min, min)
  )

  val expr3 = FUNCTION_CALL(
    Native(FRACTION_BIGINT),
    List(maxSqrt, maxSqrt, three)
  )

  val expr1Round = FUNCTION_CALL(
    Native(FRACTION_BIGINT_ROUNDS),
    List(halfMax, three, three, Rounding.HalfEven.value)
  )

  val expr2Round = FUNCTION_CALL(
    Native(FRACTION_BIGINT_ROUNDS),
    List(max, min, min, Rounding.HalfEven.value)
  )

  val expr3Round = FUNCTION_CALL(
    Native(FRACTION_BIGINT_ROUNDS),
    List(maxSqrt, maxSqrt, maxSqrt, Rounding.HalfEven.value)
  )
}
