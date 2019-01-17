package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.LangSerdeBenchmark.St
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(4)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class LangSerdeBenchmark {

  @Benchmark
  def serialize_test(st: St, bh: Blackhole): Unit = bh.consume(Serde.serialize(st.expr))

  @Benchmark
  def deserialize_test(st: St, bh: Blackhole): Unit = bh.consume(Serde.deserialize(st.serializedExpr).explicitGet())

}

object LangSerdeBenchmark {

  @State(Scope.Benchmark)
  class St extends BigSum {
    val serializedExpr: Array[Byte] = Serde.serialize(expr)
  }

}
