package com.wavesplatform.common

import java.util.concurrent.TimeUnit

import com.wavesplatform.common.ArrayCompareBenchmark.BytesSt
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

//noinspection ScalaStyle
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@Threads(4)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class ArrayCompareBenchmark {
  @Benchmark
  def sameElements_test(st: BytesSt, bh: Blackhole): Unit =
    bh.consume(st.bytes.sameElements(st.bytes1))

  @Benchmark
  def arraysEquals_test(st: BytesSt, bh: Blackhole): Unit =
    bh.consume(java.util.Arrays.equals(st.bytes, st.bytes1))
}

object ArrayCompareBenchmark {
  @State(Scope.Benchmark)
  class BytesSt {
    val bytes  = new Array[Byte](1024)
    val bytes1 = new Array[Byte](1024)

    Random.nextBytes(bytes)
    bytes.copyToArray(bytes1)
  }
}
