package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.CompilerBenchmark.St
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class CompilerBenchmark {

  @Benchmark
  def serialize_test(st: St, bh: Blackhole): Unit = bh.consume(ScriptCompiler(st.scriptString, isAssetScript = false, ScriptEstimatorV2).explicitGet()._1)
}

object CompilerBenchmark {

  @State(Scope.Benchmark)
  class St {
    val scriptString: String =
      """
        |let alicePubKey  = base58'B1Yz7fH1bJ2gVDjyJnuyKNTdMFARkKEpV'
        |let bobPubKey    = base58'7hghYeWtiekfebgAcuCg9ai2NXbRreNzc'
        |let cooperPubKey = base58'BVqYXrapgJP9atQccdBPAgJPwHDKkh6A8'
        |
        |let aliceSigned  = if(sigVerify(tx.bodyBytes, tx.proofs[0], alicePubKey  )) then 1 else 0
        |let bobSigned    = if(sigVerify(tx.bodyBytes, tx.proofs[1], bobPubKey    )) then 1 else 0
        |let cooperSigned = if(sigVerify(tx.bodyBytes, tx.proofs[2], cooperPubKey )) then 1 else 0
        |
        |aliceSigned + bobSigned + cooperSigned >= 2
      """.stripMargin
  }
}
