package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.CalculateAssetLeaseIdBenchmark.*
import com.wavesplatform.lang.v1.EnvironmentFunctionsBenchmark.{randomAddress, randomBytes}
import com.wavesplatform.lang.v1.traits.domain.Recipient.{Address, Alias}
import com.wavesplatform.lang.v1.traits.domain.{Issue, Lease}
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
class CalculateAssetLeaseIdBenchmark {
  @Benchmark
  def calculateAssetId(st: CalculateAssetIdSt, bh: Blackhole): Unit =
    bh.consume(
      Issue.calculateId(
        Int.MaxValue,
        st.MaxAssetDescription,
        isReissuable = true,
        st.MaxAssetName,
        Long.MaxValue,
        Long.MaxValue,
        ByteStr(new Array[Byte](64))
      )
    )

  @Benchmark
  def calculateLeaseIdWithAddress(st: CalculateLeaseIdSt, bh: Blackhole): Unit =
    bh.consume(Lease.calculateId(Lease(st.address, Long.MaxValue, Long.MaxValue), st.txId))

  @Benchmark
  def calculateLeaseIdWithAlias(st: CalculateLeaseIdSt, bh: Blackhole): Unit =
    bh.consume(Lease.calculateId(Lease(st.maxAlias, Long.MaxValue, Long.MaxValue), st.txId))
}

object CalculateAssetLeaseIdBenchmark {
  @State(Scope.Benchmark)
  class CalculateAssetIdSt {
    val MaxAssetName: String        = "a" * 16
    val MaxAssetDescription: String = "a" * 1000
  }

  @State(Scope.Benchmark)
  class CalculateLeaseIdSt {
    val address: Address = Address(randomAddress)
    val maxAlias: Alias  = Alias("a" * 30)
    val txId: ByteStr    = ByteStr(randomBytes(32))
  }
}
