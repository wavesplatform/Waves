package com.wavesplatform.lang.v1
import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.v1.compiler.Terms.{CONST_STRING, EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import org.openjdk.jmh.annotations._
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.v1.PureFunctionsRebenchmark.evalV5
import com.wavesplatform.lang.v1.StringSplitBenchmark.{SplitString, SplitString20x10, SplitString20x25}
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
class StringSplitBenchmark {
  @Benchmark
  def splitString(st: SplitString, bh: Blackhole): Unit =
    bh.consume(evalV5(st.expr))

  @Benchmark
  def splitString20x25(st: SplitString20x25, bh: Blackhole): Unit =
    bh.consume(evalV5(st.expr))

  @Benchmark
  def splitString20x10(st: SplitString20x10, bh: Blackhole): Unit =
    bh.consume(evalV5(st.expr))
}

object StringSplitBenchmark {
  abstract class StringSplit(listSize: Int, elementSize: Int) {
    val separator       = ","
    val separatedString = List.fill(listSize)(Random.nextPrintableChar().toString * elementSize).mkString(separator)
    val expr: EXPR =
      FUNCTION_CALL(
        PureContext.splitStr,
        List(
          CONST_STRING(separatedString).explicitGet(),
          CONST_STRING(separator).explicitGet()
        )
      )
  }

  @State(Scope.Benchmark)
  class SplitString extends StringSplit(1000, 31)

  @State(Scope.Benchmark)
  class SplitString20x25 extends StringSplit(20, 25)

  @State(Scope.Benchmark)
  class SplitString20x10 extends StringSplit(20, 10)
}
