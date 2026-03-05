package com.wavesplatform.common

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.explicitGet
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.eval
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 20, time = 1)
@Measurement(iterations = 20, time = 1)
class HashBenchmark {
  @Benchmark
  def blake2b(st: HashBenchmark.Blake2bHashData, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def sha256(st: HashBenchmark.Sha256HashData, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def keccak(st: HashBenchmark.KeccakHashData, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))
}

object HashBenchmark {
  @State(Scope.Benchmark)
  class HashData(functionId: Short) {
    @Param(Array("16384", "32768", "65536", "131072", "153600"))
    var dataSize   = 0
    var expr: EXPR = uninitialized

    @Setup def setup(): Unit = {
      expr = FUNCTION_CALL(
        Native(functionId),
        List(CONST_BYTESTR(ByteStr(new Array[Byte](dataSize)), CONST_BYTESTR.NoLimit).explicitGet())
      )
    }
  }

  class Blake2bHashData extends HashData(FunctionIds.BLAKE256)
  class Sha256HashData  extends HashData(FunctionIds.SHA256)
  class KeccakHashData  extends HashData(FunctionIds.KECCAK256)
}
