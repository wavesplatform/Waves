package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.crypto.{Blake2b256, Keccak256, Sha256}
import com.wavesplatform.lang.v1.CryptoHashFunctionsBenchmark.*
import com.wavesplatform.lang.v1.EnvironmentFunctionsBenchmark.randomBytes
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 30, time = 1)
@Measurement(iterations = 30, time = 1)
class CryptoHashFunctionsBenchmark {
  @Benchmark
  def blake2b256_162Kb(st: St162Kb, bh: Blackhole): Unit =
    bh.consume(Blake2b256.hash(st.message))

  @Benchmark
  def sha256_162Kb(st: St162Kb, bh: Blackhole): Unit =
    bh.consume(Sha256.hash(st.message))

  @Benchmark
  def keccak256_162Kb(st: St162Kb, bh: Blackhole): Unit =
    bh.consume(Keccak256.hash(st.message))

  @Benchmark
  def blake2b256_150Kb(st: St150Kb, bh: Blackhole): Unit =
    bh.consume(Blake2b256.hash(st.message))

  @Benchmark
  def sha256_150Kb(st: St150Kb, bh: Blackhole): Unit =
    bh.consume(Sha256.hash(st.message))

  @Benchmark
  def keccak256_150Kb(st: St150Kb, bh: Blackhole): Unit =
    bh.consume(Keccak256.hash(st.message))

  @Benchmark
  def blake2b256_128Kb(st: St128Kb, bh: Blackhole): Unit =
    bh.consume(Blake2b256.hash(st.message))

  @Benchmark
  def sha256_128Kb(st: St128Kb, bh: Blackhole): Unit =
    bh.consume(Sha256.hash(st.message))

  @Benchmark
  def keccak256_128Kb(st: St128Kb, bh: Blackhole): Unit =
    bh.consume(Keccak256.hash(st.message))

  @Benchmark
  def blake2b256_96Kb(st: St96Kb, bh: Blackhole): Unit =
    bh.consume(Blake2b256.hash(st.message))

  @Benchmark
  def sha256_96Kb(st: St96Kb, bh: Blackhole): Unit =
    bh.consume(Sha256.hash(st.message))

  @Benchmark
  def keccak256_96Kb(st: St96Kb, bh: Blackhole): Unit =
    bh.consume(Keccak256.hash(st.message))

  @Benchmark
  def blake2b256_64Kb(st: St64Kb, bh: Blackhole): Unit =
    bh.consume(Blake2b256.hash(st.message))

  @Benchmark
  def sha256_64Kb(st: St64Kb, bh: Blackhole): Unit =
    bh.consume(Sha256.hash(st.message))

  @Benchmark
  def keccak256_64Kb(st: St64Kb, bh: Blackhole): Unit =
    bh.consume(Keccak256.hash(st.message))

  @Benchmark
  def blake2b256_32Kb(st: St32Kb, bh: Blackhole): Unit =
    bh.consume(Blake2b256.hash(st.message))

  @Benchmark
  def sha256_32Kb(st: St32Kb, bh: Blackhole): Unit =
    bh.consume(Sha256.hash(st.message))

  @Benchmark
  def keccak256_32Kb(st: St32Kb, bh: Blackhole): Unit =
    bh.consume(Keccak256.hash(st.message))

  @Benchmark
  def blake2b256_16Kb(st: St16Kb, bh: Blackhole): Unit =
    bh.consume(Blake2b256.hash(st.message))

  @Benchmark
  def sha256_16Kb(st: St16Kb, bh: Blackhole): Unit =
    bh.consume(Sha256.hash(st.message))

  @Benchmark
  def keccak256_16Kb(st: St16Kb, bh: Blackhole): Unit =
    bh.consume(Keccak256.hash(st.message))
}

object CryptoHashFunctionsBenchmark {
  @State(Scope.Benchmark)
  class St162Kb extends St(162)

  @State(Scope.Benchmark)
  class St150Kb extends St(150)

  @State(Scope.Benchmark)
  class St128Kb extends St(128)

  @State(Scope.Benchmark)
  class St96Kb extends St(96)

  @State(Scope.Benchmark)
  class St64Kb extends St(64)

  @State(Scope.Benchmark)
  class St32Kb extends St(32)

  @State(Scope.Benchmark)
  class St16Kb extends St(16)

  class St(size: Int) {
    val message = randomBytes(size * 1024)
  }
}
