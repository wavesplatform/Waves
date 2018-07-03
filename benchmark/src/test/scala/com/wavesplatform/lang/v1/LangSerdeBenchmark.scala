package com.wavesplatform.lang.v1

import java.io.ByteArrayOutputStream
import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.v1.LangSerdeBenchmark.St
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scodec.bits.BitVector

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(4)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class LangSerdeBenchmark {

  @Benchmark
  def serialize_test(st: St, bh: Blackhole): Unit = bh.consume(Serde.codec.encode(st.expr).require)

  @Benchmark
  def serialize_own_test(st: St, bh: Blackhole): Unit = {
    val bb = new ByteArrayOutputStream()
    Serde.serialize(st.expr, bb)
    bh.consume(bb.toByteArray)
  }

  @Benchmark
  def deserialize_test(st: St, bh: Blackhole): Unit = bh.consume(Serde.codec.decode(st.serializedExpr).require.value)

  @Benchmark
  def deserialize_own_test(st: St, bh: Blackhole): Unit = bh.consume(Serde.deserialize(st.serializedExprByteBuffer).explicitGet())

}

object LangSerdeBenchmark {

  @State(Scope.Benchmark)
  class St extends BigSum {
    val serializedExpr: BitVector = Serde.codec.encode(expr).require.compact

    private val bb               = serializedExpr.toByteBuffer
    def serializedExprByteBuffer = bb.duplicate()
  }

}
