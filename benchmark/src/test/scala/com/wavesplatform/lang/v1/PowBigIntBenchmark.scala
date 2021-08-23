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
class PowBigIntBenchmark {
  @Benchmark
  def pow1(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr1, V5))

  @Benchmark
  def pow2(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr2, V5))

  @Benchmark
  def pow3(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr3, V5))

  @Benchmark
  def pow4(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr4, V5))

  @Benchmark
  def pow5(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr5, V5))

  @Benchmark
  def pow6(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr6, V5))

  @Benchmark
  def pow7(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr7, V5))

  @Benchmark
  def pow8(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr8, V5))

  @Benchmark
  def pow9(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr9, V5))

  @Benchmark
  def pow10(bh: Blackhole, s: PowBigIntSt): Unit = bh.consume(EvaluatorV2.applyCompleted(s.ctx, s.expr10, V5))
}

@State(Scope.Benchmark)
class PowBigIntSt {
  val ds  = DirectiveSet(V5, Account, Expression).fold(null, identity)
  val ctx = lazyContexts(ds).value().evaluationContext(Common.emptyBlockchainEnvironment())

  val max = s"""parseBigIntValue("${PureContext.BigIntMax}")"""
  val min = s"""parseBigIntValue("${PureContext.BigIntMin}")"""
  val one = s"""toBigInt(1)"""

  val d18 = """parseBigIntValue("987654321012345678")"""
  val d19 = """parseBigIntValue("1987654321012345678")"""

  val e1 = """parseBigIntValue("3259987654320123456789")"""
  val e2 = """parseBigIntValue("515598765432101234567")"""
  val e3 = s"""$max / (${List.fill(7)(s"""toBigInt(${Long.MaxValue})""").mkString(" * ")} / toBigInt(4))"""

  val expr1  = compile(s"pow($max, 0, $max, 18, 18, DOWN)") // ERROR
  val expr2  = compile(s"pow($max, 0, $max, 0, 0, DOWN)")   // ERROR
  val expr3  = compile(s"pow($min, 0, $max, 0, 0, DOWN)")   // ERROR
  val expr4  = compile(s"pow($max, 0, $min, 0, 18, DOWN)")  // ERROR
  val expr5  = compile(s"pow($one, 18, $min, 0, 18, DOWN)") // ERROR
  val expr6  = compile(s"pow($d18, 18, $min, 0, 18, DOWN)") // ERROR
  val expr7  = compile(s"pow($d18, 18, $max, 0, 18, DOWN)") // ERROR
  val expr8  = compile(s"pow($d18, 18, $e3, 18, 18, DOWN)") // 0
  val expr9  = compile(s"pow($d18, 18, $e1, 18, 18, DOWN)") // 2
  val expr10 = compile(s"pow($d19, 18, $e2, 18, 0, DOWN)")  // ≈ max

  private def compile(e: String): EXPR =
    TestCompiler(V5).compileExpression(e).expr.asInstanceOf[EXPR]
}
