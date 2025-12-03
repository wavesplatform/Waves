package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.directives.values.V1
import com.wavesplatform.lang.utils
import com.wavesplatform.lang.v1.ScriptEstimatorBenchmark.St
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import monix.eval.Coeval
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(4)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class ScriptEstimatorBenchmark {
  @Benchmark
  def apply_test(st: St, bh: Blackhole): Unit = bh.consume(ScriptEstimatorV2(Set.empty, st.functionCosts, st.expr))
}

object ScriptEstimatorBenchmark {

  class St extends BigSum {
    val functionCosts: Map[FunctionHeader, Coeval[Long]] = utils.functionCosts(V1)
  }

}
