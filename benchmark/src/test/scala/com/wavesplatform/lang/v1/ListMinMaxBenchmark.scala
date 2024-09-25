package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.v1.ListMinMaxBenchmark.ListMinMaxSt
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 30)
@Measurement(iterations = 30)
class ListMinMaxBenchmark {
  @Benchmark
  def max(st: ListMinMaxSt, bh: Blackhole): Unit =
    bh.consume(st.list.max)

  @Benchmark
  def min(st: ListMinMaxSt, bh: Blackhole): Unit =
    bh.consume(st.list.min)
}

object ListMinMaxBenchmark {
  @State(Scope.Benchmark)
  class ListMinMaxSt {
    val list = (Long.MinValue to Long.MinValue + PureContext.MaxListLengthV4).toVector
  }
}
