package com.wavesplatform.lang.v1
import java.util.concurrent.TimeUnit

import com.wavesplatform.common.utils._
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.PureFunctionsRebenchmark.evalV5
import com.wavesplatform.lang.v1.SumStringBenchmark._
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_STRING, FUNCTION_CALL}
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
class SumStringBenchmark {
  @Benchmark
  def sumString1_32766(st: SumString1_32766, bh: Blackhole): Unit =
    bh.consume(evalV5(st.expr))

  @Benchmark
  def sumString32766_1(st: SumString32766_1, bh: Blackhole): Unit =
    bh.consume(evalV5(st.expr))

  @Benchmark
  def sumString16383_16383(st: SumString16383_16383, bh: Blackhole): Unit =
    bh.consume(evalV5(st.expr))

  @Benchmark
  def sumString1_6000(st: SumString1_6000, bh: Blackhole): Unit =
    bh.consume(evalV5(st.expr))

  @Benchmark
  def sumString6000_1(st: SumString6000_1, bh: Blackhole): Unit =
    bh.consume(evalV5(st.expr))

  @Benchmark
  def sumString3000_3000(st: SumString3000_3000, bh: Blackhole): Unit =
    bh.consume(evalV5(st.expr))
}

object SumStringBenchmark {
  abstract class SumString(size1: Int, size2: Int) {
    val string1 = Random.nextPrintableChar().toString * size1
    val string2 = Random.nextPrintableChar().toString * size2
    val expr =
      FUNCTION_CALL(
        Native(FunctionIds.SUM_STRING),
        List(
          CONST_STRING(string1).explicitGet(),
          CONST_STRING(string2).explicitGet()
        )
      )
  }

  @State(Scope.Benchmark)
  class SumString1_32766 extends SumString(1, 32766)

  @State(Scope.Benchmark)
  class SumString32766_1 extends SumString(32766, 1)

  @State(Scope.Benchmark)
  class SumString16383_16383 extends SumString(32766 / 2, 32766 / 2)

  @State(Scope.Benchmark)
  class SumString1_6000 extends SumString(1, 6000)

  @State(Scope.Benchmark)
  class SumString6000_1 extends SumString(6000, 1)

  @State(Scope.Benchmark)
  class SumString3000_3000 extends SumString(3000, 3000)
}
