package com.wavesplatform.common

import com.wavesplatform.common.SigVerifyBenchmark.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.explicitGet
import com.wavesplatform.crypto.Curve25519
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.eval
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.utils.randomBytes
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 30, time = 1)
@Measurement(iterations = 30, time = 1)
class SigVerifyBenchmark {
  @Benchmark
  def sigVerify(st: SigVerifySt, bh: Blackhole): Unit = {
    bh.consume(eval(st.expr))
  }
}

object SigVerifyBenchmark {
  @State(Scope.Benchmark)
  class SigVerifySt {
    @Param(Array("128", "1024", "8192", "16384", "32768", "65536", "131072", "153600"))
    var dataSize   = 0
    var expr: EXPR = uninitialized

    @Setup def setup(): Unit = {
      val (privateKey, publicKey) = Curve25519.createKeyPair(randomBytes(32))
      val message                 = randomBytes(dataSize)
      val signature               = Curve25519.sign(privateKey, message)
      expr = FUNCTION_CALL(
        Native(FunctionIds.SIGVERIFY),
        List(
          CONST_BYTESTR(ByteStr(message), CONST_BYTESTR.NoLimit).explicitGet(),
          CONST_BYTESTR(ByteStr(signature)).explicitGet(),
          CONST_BYTESTR(ByteStr(publicKey)).explicitGet()
        )
      )
    }
  }
}
