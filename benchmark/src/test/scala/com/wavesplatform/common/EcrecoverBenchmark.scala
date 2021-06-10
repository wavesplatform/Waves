package com.wavesplatform.common

import java.util.concurrent.TimeUnit

import com.google.common.io.BaseEncoding
import com.wavesplatform.common.EcrecoverBenchmark.{EcrecoverSt1, EcrecoverSt2}
import com.wavesplatform.crypto.Keccak256
import com.wavesplatform.lang.Global
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 30)
@Measurement(iterations = 30)
class EcrecoverBenchmark {

  @Benchmark
  def ecrover1(st: EcrecoverSt1, bh: Blackhole): Unit =
    bh.consume(Global.ecrecover(st.messageHash, st.signature))

  @Benchmark
  def ecrover2(st: EcrecoverSt2, bh: Blackhole): Unit =
    bh.consume(Global.ecrecover(st.messageHash, st.signature))
}

object EcrecoverBenchmark {
  @State(Scope.Benchmark)
  class EcrecoverSt1 {
    val signature = BaseEncoding
      .base16()
      .decode(
        "848ffb6a07e7ce335a2bfe373f1c17573eac320f658ea8cf07426544f2203e9d52dbba4584b0b6c0ed5333d84074002878082aa938fdf68c43367946b2f615d01b"
      )
    val message     = "i am the owner"
    val prefix      = "\u0019Ethereum Signed Message:\n" + message.length
    val messageHash = Keccak256.hash((prefix + message).getBytes)
  }

  @State(Scope.Benchmark)
  class EcrecoverSt2 {
    val signature = BaseEncoding
      .base16()
      .decode(
        "3b163bbd90556272b57c35d1185b46824f8e16ca229bdb36f8dfd5eaaee9420723ef7bc3a6c0236568217aa990617cf292b1bef1e7d1d936fb2faef3d846c5751b"
      )
    val message     = "what's up jim"
    val prefix      = "\u0019Ethereum Signed Message:\n" + message.length
    val messageHash = Keccak256.hash((prefix + message).getBytes)
  }
}
