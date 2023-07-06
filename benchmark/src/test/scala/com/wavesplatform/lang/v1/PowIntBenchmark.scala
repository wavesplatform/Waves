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
class PowIntBenchmark {
  @Benchmark
  def pow1(bh: Blackhole, s: PowIntSt): Unit = bh.consume(eval(s.ctx, s.expr1, V5))

  @Benchmark
  def pow2(bh: Blackhole, s: PowIntSt): Unit = bh.consume(eval(s.ctx, s.expr2, V5))

  @Benchmark
  def pow3(bh: Blackhole, s: PowIntSt): Unit = bh.consume(eval(s.ctx, s.expr3, V5))

  @Benchmark
  def pow4(bh: Blackhole, s: PowIntSt): Unit = bh.consume(eval(s.ctx, s.expr4, V5))

  @Benchmark
  def pow5(bh: Blackhole, s: PowIntSt): Unit = bh.consume(eval(s.ctx, s.expr5, V5))

  @Benchmark
  def pow6(bh: Blackhole, s: PowIntSt): Unit = bh.consume(eval(s.ctx, s.expr6, V5))

  @Benchmark
  def pow7(bh: Blackhole, s: PowIntSt): Unit = bh.consume(eval(s.ctx, s.expr7, V5))

  @Benchmark
  def pow8(bh: Blackhole, s: PowIntSt): Unit = bh.consume(eval(s.ctx, s.expr8, V5))

  @Benchmark
  def pow9(bh: Blackhole, s: PowIntSt): Unit = bh.consume(eval(s.ctx, s.expr9, V5))

  @Benchmark
  def pow10(bh: Blackhole, s: PowIntSt): Unit = bh.consume(eval(s.ctx, s.expr10, V5))
}

@State(Scope.Benchmark)
class PowIntSt {
  val ds  = DirectiveSet(V5, Account, Expression).fold(null, identity)
  val ctx = lazyContexts((ds, true, true)).value().evaluationContext(Common.emptyBlockchainEnvironment())

  val max = Long.MaxValue

  val expr1  = compile(s"pow($max, 0, $max, 8, 8, DOWN)")               // ERROR
  val expr2  = compile(s"pow($max, 0, $max, 0, 0, DOWN)")               // ERROR
  val expr3  = compile(s"pow(-$max, 0, $max, 0, 0, DOWN)")              // ERROR
  val expr4  = compile(s"pow($max, 0, -$max, 0, 8, DOWN)")              // ERROR
  val expr5  = compile(s"pow(1, 8, -$max, 0, 8, DOWN)")                 // ERROR
  val expr6  = compile(s"pow(98765432, 8, -$max, 0, 8, DOWN)")          // ERROR
  val expr7  = compile(s"pow(98765432, 8, $max, 0, 8, DOWN)")           // ERROR
  val expr8  = compile(s"pow(98765432, 8, $max, 8, 8, DOWN)")           // 0
  val expr9  = compile(s"pow(98765432, 8, 145998765432, 8, 8, HALFUP)") // 1
  val expr10 = compile(s"pow(198765432, 8, 6298765432, 8, 0, DOWN)")    // ≈ 6 * 10^18

  private def compile(e: String): EXPR =
    TestCompiler(V5).compileExpression(e).expr.asInstanceOf[EXPR]
}
