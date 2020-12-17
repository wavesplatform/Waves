package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.CalculateAssetIdBenchmark._
import com.wavesplatform.lang.v1.EnvironmentFunctionsBenchmark.{curve25519, randomAddress, randomBytes}
import com.wavesplatform.lang.v1.traits.domain.Recipient.{Address, Alias}
import com.wavesplatform.lang.v1.traits.domain.{Issue, Lease}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scorex.crypto.hash.{Blake2b256, Keccak256, Sha256}

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class CalculateAssetIdBenchmark {
  @Benchmark
  def blake2b256_150Kb(st: CurveSt150k, bh: Blackhole): Unit =
    bh.consume(Blake2b256.hash(st.message))

  @Benchmark
  def sha256_150Kb(st: CurveSt150k, bh: Blackhole): Unit =
    bh.consume(Sha256.hash(st.message))

  @Benchmark
  def keccak256_150Kb(st: CurveSt150k, bh: Blackhole): Unit =
    bh.consume(Keccak256.hash(st.message))

  @Benchmark
  def blake2b256_128Kb(st: CurveSt128k, bh: Blackhole): Unit =
    bh.consume(Blake2b256.hash(st.message))

  @Benchmark
  def sha256_128Kb(st: CurveSt128k, bh: Blackhole): Unit =
    bh.consume(Sha256.hash(st.message))

  @Benchmark
  def keccak256_128Kb(st: CurveSt128k, bh: Blackhole): Unit =
    bh.consume(Keccak256.hash(st.message))

  @Benchmark
  def blake2b256_96Kb(st: CurveSt96k, bh: Blackhole): Unit =
    bh.consume(Blake2b256.hash(st.message))

  @Benchmark
  def sha256_96Kb(st: CurveSt96k, bh: Blackhole): Unit =
    bh.consume(Sha256.hash(st.message))

  @Benchmark
  def keccak256_96Kb(st: CurveSt96k, bh: Blackhole): Unit =
    bh.consume(Keccak256.hash(st.message))

  @Benchmark
  def blake2b256_64Kb(st: CurveSt64k, bh: Blackhole): Unit =
    bh.consume(Blake2b256.hash(st.message))

  @Benchmark
  def sha256_64Kb(st: CurveSt64k, bh: Blackhole): Unit =
    bh.consume(Sha256.hash(st.message))

  @Benchmark
  def keccak256_64Kb(st: CurveSt64k, bh: Blackhole): Unit =
    bh.consume(Keccak256.hash(st.message))

  @Benchmark
  def blake2b256_32Kb(st: CurveSt32k, bh: Blackhole): Unit =
    bh.consume(Blake2b256.hash(st.message))

  @Benchmark
  def sha256_32Kb(st: CurveSt32k, bh: Blackhole): Unit =
    bh.consume(Sha256.hash(st.message))

  @Benchmark
  def keccak256_32Kb(st: CurveSt32k, bh: Blackhole): Unit =
    bh.consume(Keccak256.hash(st.message))

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

object CalculateAssetIdBenchmark {
  @State(Scope.Benchmark)
  class CurveSt150k extends CurveSt(150)

  @State(Scope.Benchmark)
  class CurveSt128k extends CurveSt(128)

  @State(Scope.Benchmark)
  class CurveSt96k extends CurveSt(96)

  @State(Scope.Benchmark)
  class CurveSt64k extends CurveSt(64)

  @State(Scope.Benchmark)
  class CurveSt32k extends CurveSt(32)

  val (privateKey, gpublicKey) = curve25519.generateKeypair

  class CurveSt(size: Int) {
    val message   = randomBytes(size * 1024)
    val signature = curve25519.sign(privateKey, message)
    val publicKey = gpublicKey
  }

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
