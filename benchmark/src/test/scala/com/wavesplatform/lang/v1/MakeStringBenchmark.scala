package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import MakeStringBenchmark.{CurveSt32k, MakeStringSt}
import com.wavesplatform.lang.v1.EnvironmentFunctionsBenchmark.{curve25519, randomBytes}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scorex.crypto.signatures.{Curve25519, Signature}

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

  @Benchmark
  def sigVerify32Kb(st: CurveSt32k, bh: Blackhole): Unit =
    bh.consume(Curve25519.verify(Signature @@ st.signature, st.message, st.publicKey))
}

object MakeStringBenchmark {
  @State(Scope.Benchmark)
  class MakeStringSt {
    val stringList = List.fill(1000)("a" * 33)
  }

  @State(Scope.Benchmark)
  class CurveSt32k {
    val (privateKey, publicKey) = curve25519.generateKeypair
    val message                 = randomBytes(32 * 1024)
    val signature               = curve25519.sign(privateKey, message)
  }
}