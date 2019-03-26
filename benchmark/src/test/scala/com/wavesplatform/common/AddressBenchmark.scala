package com.wavesplatform.common

import java.util.concurrent.TimeUnit

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.AddressBenchmark.{CachedAddress, PublicKeySt, UncachedAddress}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(4)
@Fork(1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
class AddressBenchmark {
  @Benchmark
  def createAddress_test(st: PublicKeySt, bh: Blackhole): Unit = {
    bh.consume(new UncachedAddress(st.publicKey).toAddress)
  }

  @Benchmark
  def readAddress_test(st: PublicKeySt, bh: Blackhole): Unit = {
    val addr = new UncachedAddress(st.publicKey)
    (0 to 1000).foreach(_ => bh.consume(addr.toAddress))
  }

  @Benchmark
  def readCachedAddress_test(st: PublicKeySt, bh: Blackhole): Unit = {
    val addr = new CachedAddress(st.publicKey)
    (0 to 1000).foreach(_ => bh.consume(addr.toAddress))
  }
}

object AddressBenchmark {
  @State(Scope.Benchmark)
  class PublicKeySt {
    val publicKey = new Array[Byte](scorex.crypto.signatures.Curve25519.KeyLength25519)
    Random.nextBytes(publicKey)
  }

  class UncachedAddress(publicKey: Array[Byte]) {
    def toAddress = Address.fromPublicKey(PublicKey(publicKey))
  }

  class CachedAddress(publicKey: Array[Byte]) {
    lazy val toAddress = Address.fromPublicKey(PublicKey(publicKey))
  }
}
