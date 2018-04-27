package com.wavesplatform.lang

import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.LangSerdeBenchmark.St
import com.wavesplatform.lang.v1.FunctionHeader.{FunctionHeaderType => FHT}
import com.wavesplatform.lang.v1.Terms.Typed.{CONST_LONG, EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.Terms.{BOOLEAN, LONG, Typed}
import com.wavesplatform.lang.v1.{FunctionHeader, Serde}
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
  def deserialize_test(st: St, bh: Blackhole): Unit = bh.consume(Serde.codec.decode(st.serilizedExpr).require.value)

}

object LangSerdeBenchmark {

  @State(Scope.Benchmark)
  class St {
    private val bigSum = (1 to 100).foldLeft[EXPR](CONST_LONG(0)) { (r, i) =>
      FUNCTION_CALL(
        function = FunctionHeader(name = "+", List(FHT.LONG, FHT.LONG)),
        args = List(r, CONST_LONG(i)),
        LONG
      )
    }

    val expr: Typed.EXPR = FUNCTION_CALL(
      function = FunctionHeader(name = "==", List(FHT.LONG, FHT.LONG)),
      args = List(CONST_LONG(1), bigSum),
      BOOLEAN
    )

    val serilizedExpr: BitVector = Serde.codec.encode(expr).require
  }

}
