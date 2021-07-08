package com.wavesplatform.common

import java.util.concurrent.TimeUnit

import com.wavesplatform.common.SigVerifyBenchmark._
import com.wavesplatform.crypto.Curve25519
import com.wavesplatform.lang.v1.EnvironmentFunctionsBenchmark.{curve25519, randomBytes}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 30)
@Measurement(iterations = 30)
class SigVerifyBenchmark {
  @Benchmark
  def sigVerify_128b(st: CurveSt128b, bh: Blackhole): Unit =
    bh.consume(Curve25519.verify(st.signature, st.message, st.publicKey))

  @Benchmark
  def sigVerify_1Kb(st: CurveSt1Kb, bh: Blackhole): Unit =
    bh.consume(Curve25519.verify(st.signature, st.message, st.publicKey))

  @Benchmark
  def sigVerify_5Kb(st: CurveSt5Kb, bh: Blackhole): Unit =
    bh.consume(Curve25519.verify(st.signature, st.message, st.publicKey))

  @Benchmark
  def sigVerify_6Kb(st: CurveSt6Kb, bh: Blackhole): Unit =
    bh.consume(Curve25519.verify(st.signature, st.message, st.publicKey))

  @Benchmark
  def sigVerify_7Kb(st: CurveSt7Kb, bh: Blackhole): Unit =
    bh.consume(Curve25519.verify(st.signature, st.message, st.publicKey))

  @Benchmark
  def sigVerify_8Kb(st: CurveSt8Kb, bh: Blackhole): Unit =
    bh.consume(Curve25519.verify(st.signature, st.message, st.publicKey))

  @Benchmark
  def sigVerify_16Kb(st: CurveSt16Kb, bh: Blackhole): Unit =
    bh.consume(Curve25519.verify(st.signature, st.message, st.publicKey))

  @Benchmark
  def sigVerify_32Kb(st: CurveSt32Kb, bh: Blackhole): Unit =
    bh.consume(Curve25519.verify(st.signature, st.message, st.publicKey))

  @Benchmark
  def sigVerify_64Kb(st: CurveSt64Kb, bh: Blackhole): Unit =
    bh.consume(Curve25519.verify(st.signature, st.message, st.publicKey))

  @Benchmark
  def sigVerify_128Kb(st: CurveSt128Kb, bh: Blackhole): Unit =
    bh.consume(Curve25519.verify(st.signature, st.message, st.publicKey))

  @Benchmark
  def sigVerify_150Kb(st: CurveSt150Kb, bh: Blackhole): Unit =
    bh.consume(Curve25519.verify(st.signature, st.message, st.publicKey))

  // For DataTransaction.MaxProtoBytes
  @Benchmark
  def sigVerify_162Kb(st: CurveSt162Kb, bh: Blackhole): Unit =
    bh.consume(Curve25519.verify(st.signature, st.message, st.publicKey))

}

object SigVerifyBenchmark {
  @State(Scope.Benchmark)
  class CurveSt162Kb extends CurveSt(162 * 1024)

  @State(Scope.Benchmark)
  class CurveSt150Kb extends CurveSt(150 * 1024)

  @State(Scope.Benchmark)
  class CurveSt128Kb extends CurveSt(128 * 1024)

  @State(Scope.Benchmark)
  class CurveSt64Kb extends CurveSt(64 * 1024)

  @State(Scope.Benchmark)
  class CurveSt32Kb extends CurveSt(32 * 1024)

  @State(Scope.Benchmark)
  class CurveSt16Kb extends CurveSt(16 * 1024)

  @State(Scope.Benchmark)
  class CurveSt8Kb extends CurveSt(8 * 1024)

  @State(Scope.Benchmark)
  class CurveSt7Kb extends CurveSt(7 * 1024)

  @State(Scope.Benchmark)
  class CurveSt6Kb extends CurveSt(6 * 1024)

  @State(Scope.Benchmark)
  class CurveSt5Kb extends CurveSt(5 * 1024)

  @State(Scope.Benchmark)
  class CurveSt1Kb extends CurveSt(1024)

  @State(Scope.Benchmark)
  class CurveSt128b extends CurveSt(128)

  class CurveSt(size: Int) {
    val (privateKey, publicKey) = curve25519.generateKeypair
    val message                 = randomBytes(size)
    val signature               = curve25519.sign(privateKey, message)
  }
}
