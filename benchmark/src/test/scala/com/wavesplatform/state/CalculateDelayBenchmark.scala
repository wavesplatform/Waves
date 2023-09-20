package com.wavesplatform.state

import com.wavesplatform.common.state.ByteStr
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
class CalculateDelayBenchmark {
  import CalculateDelayBenchmark.*

  @Benchmark
  def calculateDelay1(bh: Blackhole, st: St): Unit =
    bh.consume(st.environment.calculateDelay(ByteStr.empty, 0, ByteStr.empty, 0))

  @Benchmark
  def calculateDelay2(bh: Blackhole, st: St): Unit =
    bh.consume(
      st.environment.calculateDelay(ByteStr.fill(96)(127), Long.MaxValue, ByteStr.fill(26)(127), Long.MaxValue)
    )

  @Benchmark
  def calculateDelay3(bh: Blackhole, st: St): Unit =
    bh.consume(
      st.environment.calculateDelay(ByteStr.fill(96)(-128), Long.MinValue, ByteStr.fill(26)(-128), Long.MinValue)
    )

  @Benchmark
  def calculateDelay4(bh: Blackhole, st: St): Unit =
    bh.consume(
      st.environment.calculateDelay(ByteStr.fill(32)(32), 123456, ByteStr.fill(26)(32), 100_000_000)
    )
}

object CalculateDelayBenchmark {
  class St extends DBState {}
}
