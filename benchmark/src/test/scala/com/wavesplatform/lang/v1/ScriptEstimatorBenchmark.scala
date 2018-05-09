package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.v1.ScriptEstimatorBenchmark.St
import com.wavesplatform.lang.v1.ctx.Context
import com.wavesplatform.lang.v1.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluation.EvaluatorV1_1
import com.wavesplatform.utils
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(4)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class ScriptEstimatorBenchmark {

  @Benchmark
  def apply_test(st: St, bh: Blackhole): Unit = bh.consume(ScriptEstimator(st.functionCosts, st.expr))

  @Benchmark
  def v1_test(st: St, bh: Blackhole): Unit = bh.consume(EvaluatorV1[Long](PureContext.instance, st.expr))

  @Benchmark
  def v1_1_test(st: St, bh: Blackhole): Unit = bh.consume(EvaluatorV1_1[Long](PureContext.instance, st.expr))

}

object ScriptEstimatorBenchmark {

  class St extends BigSum {
    val functionCosts: Map[FunctionHeader, Long] = Context.functionCosts(utils.dummyContext.functions.values)
  }

}
