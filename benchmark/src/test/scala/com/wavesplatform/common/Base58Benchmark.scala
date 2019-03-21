package com.wavesplatform.common

import java.util.concurrent.{ThreadLocalRandom, TimeUnit}

import com.wavesplatform.common.Base58Benchmark.{Base58St, BytesSt}
import com.wavesplatform.common.utils.{Base58, FastBase58, StdBase58}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.SECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@Threads(4)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class Base58Benchmark {
  @Benchmark
  def base58_fastEncode_test(st: BytesSt, bh: Blackhole): Unit = bh.consume(FastBase58.encode(st.bytes))

  @Benchmark
  def base58_encode_test(st: BytesSt, bh: Blackhole): Unit = bh.consume(StdBase58.encode(st.bytes))

  @Benchmark
  def base58_decode_test(st: Base58St, bh: Blackhole): Unit = bh.consume(StdBase58.decode(st.base58))

  @Benchmark
  def base58_fastDecode_test(st: Base58St, bh: Blackhole): Unit = bh.consume(FastBase58.decode(st.base58))
}

object Base58Benchmark {
  def randomBytes(length: Int): Array[Byte] = {
    val bytes = new Array[Byte](length)
    ThreadLocalRandom.current().nextBytes(bytes)
    bytes
  }

  @State(Scope.Benchmark)
  class BytesSt {
    val bytes = randomBytes(10000)
  }

  @State(Scope.Benchmark)
  class Base58St extends BytesSt {
    val base58 = Base58.encode(bytes)
  }
}
