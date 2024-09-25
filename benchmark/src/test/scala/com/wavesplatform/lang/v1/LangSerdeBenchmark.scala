package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.LangSerdeBenchmark.St
import com.wavesplatform.lang.v1.serialization.{SerdeV1, SerdeV2}
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(4)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class LangSerdeBenchmark {

  @Benchmark
  def serializeTestV1(st: St, bh: Blackhole): Unit = bh.consume(SerdeV1.serialize(st.expr))

  @Benchmark
  def deserializeTestV1(st: St, bh: Blackhole): Unit = bh.consume(SerdeV1.deserialize(st.serializedExprV1).explicitGet())

  @Benchmark
  def serializeTestV2(st: St, bh: Blackhole): Unit = bh.consume(SerdeV2.serialize(st.expr))

  @Benchmark
  def deserializeTestV2(st: St, bh: Blackhole): Unit = bh.consume(SerdeV2.deserialize(st.serializedExprV2).explicitGet())

}

object LangSerdeBenchmark {

  @State(Scope.Benchmark)
  class St extends BigSum {
    val serializedExprV1: Array[Byte] = SerdeV1.serialize(expr)
    val serializedExprV2: Array[Byte] = SerdeV2.serialize(expr)
  }

}
