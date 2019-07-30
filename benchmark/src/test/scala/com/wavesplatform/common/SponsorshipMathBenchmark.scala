package com.wavesplatform.common
import java.util.concurrent.TimeUnit

import com.wavesplatform.state.diffs.FeeValidation
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@Threads(4)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class SponsorshipMathBenchmark {
  @Benchmark
  def bigDecimal_test(bh: Blackhole): Unit = {
    def toWaves(assetFee: Long, sponsorship: Long): Long = {
      val waves = (BigDecimal(assetFee) * BigDecimal(FeeValidation.FeeUnit)) / BigDecimal(sponsorship)
      if (waves > Long.MaxValue) {
        throw new java.lang.ArithmeticException("Overflow")
      }
      waves.toLong
    }

    bh.consume(toWaves(100000, 100000000))
  }

  @Benchmark
  def bigInt_test(bh: Blackhole): Unit = {
    def toWaves(assetFee: Long, sponsorship: Long): Long = {
      val waves = BigInt(assetFee) * FeeValidation.FeeUnit / sponsorship
      waves.bigInteger.longValueExact()
    }

    bh.consume(toWaves(100000, 100000000))
  }
}
