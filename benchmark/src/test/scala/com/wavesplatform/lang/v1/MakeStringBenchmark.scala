package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.common.utils.*
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.MakeStringBenchmark.*
import com.wavesplatform.lang.v1.PureFunctionsRebenchmark.evalV5
import com.wavesplatform.lang.v1.compiler.Terms.{ARR, CONST_STRING, FUNCTION_CALL}
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
class MakeStringBenchmark {
  @Benchmark
  def makeStringSep31x1000(st: MakeStringSep31x1000, bh: Blackhole): Unit =
    bh.consume(evalV5(st.expr))

  @Benchmark
  def makeString31x1000(st: MakeString31x1000, bh: Blackhole): Unit =
    bh.consume(evalV5(st.expr))

  @Benchmark
  def makeString31x100(st: MakeString31x100, bh: Blackhole): Unit =
    bh.consume(evalV5(st.expr))

  @Benchmark
  def makeString31x10(st: MakeString31x10, bh: Blackhole): Unit =
    bh.consume(evalV5(st.expr))

  @Benchmark
  def makeString310x100(st: MakeString310x100, bh: Blackhole): Unit =
    bh.consume(evalV5(st.expr))

  @Benchmark
  def makeString3100x10(st: MakeString3100x10, bh: Blackhole): Unit =
    bh.consume(evalV5(st.expr))

  @Benchmark
  def makeString1x70(st: MakeString1x70, bh: Blackhole): Unit =
    bh.consume(evalV5(st.expr))

  @Benchmark
  def makeString7x70(st: MakeString7x70, bh: Blackhole): Unit =
    bh.consume(evalV5(st.expr))
}

object MakeStringBenchmark {
  abstract class MakeString(listSize: Int, stringSize: Int = 1, separatorSize: Int = 1) {
    val string    = Random.nextPrintableChar().toString * stringSize
    val separator = Random.nextPrintableChar().toString * separatorSize
    val expr =
      FUNCTION_CALL(
        Native(FunctionIds.MAKESTRING),
        List(
          ARR(Vector.fill(listSize)(CONST_STRING(string).explicitGet()), limited = true).explicitGet(),
          CONST_STRING(separator).explicitGet()
        )
      )
  }

  @State(Scope.Benchmark)
  class MakeStringSep31x1000 extends MakeString(listSize = 1000, separatorSize = 31)

  @State(Scope.Benchmark)
  class MakeString31x1000 extends MakeString(listSize = 1000, stringSize = 31)

  @State(Scope.Benchmark)
  class MakeString31x100 extends MakeString(listSize = 100, stringSize = 31)

  @State(Scope.Benchmark)
  class MakeString31x10 extends MakeString(listSize = 100, stringSize = 31)

  @State(Scope.Benchmark)
  class MakeString310x100 extends MakeString(listSize = 100, stringSize = 310)

  @State(Scope.Benchmark)
  class MakeString3100x10 extends MakeString(listSize = 10, stringSize = 3100)

  @State(Scope.Benchmark)
  class MakeString1x70 extends MakeString(listSize = 70, stringSize = 1)

  @State(Scope.Benchmark)
  class MakeString7x70 extends MakeString(listSize = 70, stringSize = 7)
}
