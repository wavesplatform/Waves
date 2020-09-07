package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.v1.MakeStringBenchmark.MakeStringSt
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 30)
@Measurement(iterations = 30)
class MakeStringBenchmark {

  @Benchmark
  def makeString(st: MakeStringSt, bh: Blackhole): Unit =
    bh.consume(st.stringList.mkString(","))
}

object MakeStringBenchmark {
  @State(Scope.Benchmark)
  class MakeStringSt {
    val stringList = List.fill(1000)("a" * 33)
  }
}