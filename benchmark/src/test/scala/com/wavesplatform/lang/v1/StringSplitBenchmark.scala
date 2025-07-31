package com.wavesplatform.lang.v1
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.lang.v1.StringSplitBenchmark.*
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_STRING, EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
class StringSplitBenchmark {
  @Benchmark
  def split(st: SplitStringSt, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))
}

object StringSplitBenchmark {
  @State(Scope.Benchmark)
  class SplitStringSt {
    @Param(Array("1000,31", "500,62", "100,310", "100,60", "20,25", "50,2"))
    var listAndElemSize = ""
    var expr: EXPR      = uninitialized

    @Setup def setup(): Unit = {
      val Array(listSize, elemSize) = listAndElemSize.split(",").map(_.toInt)

      expr = FUNCTION_CALL(
        FunctionHeader.Native(FunctionIds.SPLIT51C),
        List(
          CONST_STRING(List.fill(listSize)("a" * elemSize).mkString(",")).explicitGet(),
          CONST_STRING(",").explicitGet()
        )
      )
    }
  }
}
