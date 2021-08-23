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
class BigIntToStringBenchmark {
  @Benchmark
  def toString(bh: Blackhole, s: BigIntToStringSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr, V5))
}

@State(Scope.Benchmark)
class BigIntToStringSt {
  val ds  = DirectiveSet(V5, Account, Expression).fold(null, identity)
  val ctx = lazyContexts(ds).value().evaluationContext(Common.emptyBlockchainEnvironment())

  val max  = s"""parseBigIntValue("${PureContext.BigIntMax}")"""
  val min = s"""parseBigIntValue("${PureContext.BigIntMin}")"""
  val expr = TestCompiler(V5).compileExpression(s"$min.toString()").expr.asInstanceOf[EXPR]
}
