package com.wavesplatform.common

import com.wavesplatform.account.PrivateKey
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsPublicKey, BlsSignature}
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import com.wavesplatform.common.utils.EitherExt2.explicitGet

import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 30, time = 1)
@Measurement(iterations = 30, time = 1)
class BLSBenchmark {
  @Benchmark
  def verifyAgg(st: EndorsementSt, bh: Blackhole) = bh.consume(st.aggregatedSignature.verifyAgg(st.message, st.publicKeys).explicitGet())
}

@State(Scope.Benchmark)
class EndorsementSt {
  @Param(Array("32", "64", "128"))
  var generatorCount                   = 0
  var publicKeys                       = Seq.empty[BlsPublicKey]
  var aggregatedSignature: BlsSignature = uninitialized
  val message: Array[Byte] = {
    val bs = new Array[Byte](64)
    bs
  }

  @Setup
  def setup(): Unit = {
    val privateKeys = Seq.tabulate(generatorCount) { i =>
      val bs = new Array[Byte](32)
      bs(0) = i.toByte
      BlsKeyPair(PrivateKey(bs))
    }

    publicKeys = privateKeys.map(_.publicKey)
    aggregatedSignature = BlsSignature.agg(privateKeys.map(sk => sk.sign(message))).explicitGet()
  }
}
