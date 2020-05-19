package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.v1.EnvironmentFunctionsBenchmark.{curve25519, randomBytes}
import com.wavesplatform.lang.v1.ListIndexOfBenchmark.CurveSt32k
import com.wavesplatform.lang.v1.ListMinMaxBenchmark.ListMinMaxSt
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scorex.crypto.signatures.{Curve25519, Signature}

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

  @Benchmark
  def sigVerify32Kb(st: CurveSt32k, bh: Blackhole): Unit =
    bh.consume(Curve25519.verify(Signature @@ st.signature, st.message, st.publicKey))
}

object ListMinMaxBenchmark {
  @State(Scope.Benchmark)
  class ListMinMaxSt {
    val list = (Long.MinValue to Long.MinValue + PureContext.MaxListLengthV4).toVector
  }

  @State(Scope.Benchmark)
  class CurveSt32k {
    val (privateKey, publicKey) = curve25519.generateKeypair
    val message                 = randomBytes(32 * 1024)
    val signature               = curve25519.sign(privateKey, message)
  }
}


