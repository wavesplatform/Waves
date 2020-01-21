package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.v1.CalculateAssetIdBenchmark.{CalculateAssetIdSt, CurveSt}
import com.wavesplatform.lang.v1.EnvironmentFunctionsBenchmark.{curve25519, randomBytes}
import com.wavesplatform.lang.v1.traits.domain.Issue
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, OutputTimeUnit, Scope, State, Threads, Warmup}
import org.openjdk.jmh.infra.Blackhole
import scorex.crypto.signatures.{Curve25519, Signature}

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class CalculateAssetIdBenchmark {
  @Benchmark
  def sigVerify150Kb(st: CurveSt, bh: Blackhole): Unit =
    bh.consume(Curve25519.verify(Signature @@ st.signature150Kb, st.message150Kb, st.publicKey))

  @Benchmark
  def calculateAssetId(st: CalculateAssetIdSt, bh: Blackhole): Unit =
    bh.consume(Issue.calculateId(Int.MaxValue, st.MaxAssetDescription, isReissuable = true, st.MaxAssetName, Long.MaxValue, Long.MaxValue))
}

object CalculateAssetIdBenchmark {
  @State(Scope.Benchmark)
  class CurveSt {
    val (privateKey, publicKey) = curve25519.generateKeypair
    val message150Kb            = randomBytes(150 * 1024)
    val message32Kb             = randomBytes(32 * 1024)
    val signature150Kb          = curve25519.sign(privateKey, message150Kb)
    val signature32Kb           = curve25519.sign(privateKey, message32Kb)
  }

  @State(Scope.Benchmark)
  class CalculateAssetIdSt {
    val MaxAssetName: String        = "a" * 16
    val MaxAssetDescription: String = "a" * 1000
  }
}