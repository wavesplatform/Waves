package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.Common
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, V5}
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.compiler.TestCompiler
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
class SqrtIntBenchmark {
  @Benchmark
  def sqrt1(bh: Blackhole, s: SqrtIntSt): Unit = bh.consume(eval(s.ctx, s.expr1, V5))

  @Benchmark
  def sqrt2(bh: Blackhole, s: SqrtIntSt): Unit = bh.consume(eval(s.ctx, s.expr2, V5))
}

@State(Scope.Benchmark)
class SqrtIntSt {
  val ds  = DirectiveSet(V5, Account, Expression).fold(null, identity)
  val ctx = lazyContexts((ds, true, true)).value().evaluationContext(Common.emptyBlockchainEnvironment())

  val expr1 = compile(s"pow(${Long.MaxValue}, 0, 5, 1, 8, DOWN)")
  val expr2 = compile(s"pow(${Long.MaxValue}, 8, 5, 1, 8, DOWN)")

  private def compile(e: String): EXPR =
    TestCompiler(V5).compileExpression(e).expr.asInstanceOf[EXPR]
}
