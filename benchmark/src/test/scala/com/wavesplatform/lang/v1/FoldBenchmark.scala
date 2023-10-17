package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.Common
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, V5}
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.compiler.TestCompiler
import org.openjdk.jmh.annotations.{State, _}
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 30)
@Measurement(iterations = 30)
class FoldBenchmark {
  @Benchmark
  def singleCall(bh: Blackhole, s: FoldSt): Unit = bh.consume(eval(s.ctx, s.singleCall, V5))

  @Benchmark
  def fold10(bh: Blackhole, s: FoldSt): Unit = bh.consume(eval(s.ctx, s.fold10, V5))

  @Benchmark
  def fold20(bh: Blackhole, s: FoldSt): Unit = bh.consume(eval(s.ctx, s.fold20, V5))

  @Benchmark
  def fold50(bh: Blackhole, s: FoldSt): Unit = bh.consume(eval(s.ctx, s.fold50, V5))

  @Benchmark
  def fold100(bh: Blackhole, s: FoldSt): Unit = bh.consume(eval(s.ctx, s.fold100, V5))

  @Benchmark
  def fold200(bh: Blackhole, s: FoldSt): Unit = bh.consume(eval(s.ctx, s.fold200, V5))

  @Benchmark
  def fold500(bh: Blackhole, s: FoldSt): Unit = bh.consume(eval(s.ctx, s.fold500, V5))

  @Benchmark
  def fold1000(bh: Blackhole, s: FoldSt): Unit = bh.consume(eval(s.ctx, s.fold1000, V5))
}

@State(Scope.Benchmark)
class FoldSt {
  val ds  = DirectiveSet(V5, Account, Expression).fold(null, identity)
  val ctx = lazyContexts((ds, true, true)).value().evaluationContext(Common.emptyBlockchainEnvironment())

  val function = "func f(acc: Boolean, elem: ByteVector) = acc && sigVerify(elem, base58'', base58'')"

  val singleCall = compile(s""" $function\n f(false, base58'') """)
  val fold10     = foldN(10)
  val fold20     = foldN(20)
  val fold50     = foldN(50)
  val fold100    = foldN(100)
  val fold200    = foldN(200)
  val fold500    = foldN(500)
  val fold1000   = foldN(1000)

  def foldN(n: Int): EXPR =
    compile(
      s"""
         | $function
         | let arr = [${List.fill(n)("base58''").mkString(",")}]
         | fold(arr, false, "f")
       """.stripMargin
    )

  def compile(script: String): EXPR =
    TestCompiler(V5).compileExpression(script, checkSize = false).expr.asInstanceOf[EXPR]
}
