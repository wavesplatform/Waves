package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.Common
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, V5}
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BIGINT, FUNCTION_CALL}
import com.wavesplatform.lang.v1.evaluator.FunctionIds.BIGINT_TO_STRING
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import org.openjdk.jmh.annotations.{State, _}
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
class BigIntToStringBenchmark {
  @Benchmark
  def toString(bh: Blackhole, s: BigIntToStringSt): Unit = bh.consume(eval(s.ctx, s.expr, V5))
}

@State(Scope.Benchmark)
class BigIntToStringSt {
  val ds  = DirectiveSet(V5, Account, Expression).fold(null, identity)
  val ctx = lazyContexts((ds, true)).value().evaluationContext(Common.emptyBlockchainEnvironment())

  val expr = FUNCTION_CALL(
    Native(BIGINT_TO_STRING),
    List(CONST_BIGINT(PureContext.BigIntMin))
  )
}
