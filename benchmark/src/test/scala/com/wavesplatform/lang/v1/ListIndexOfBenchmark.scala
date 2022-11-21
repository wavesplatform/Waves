package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.ListIndexOfBenchmark.ListIndexOfSt
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_STRING, EVALUATED}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 30)
@Measurement(iterations = 30)
class ListIndexOfBenchmark {
  @Benchmark
  def indexOfMaxSizeElementInMaxCmpWeightElementsList(st: ListIndexOfSt, bh: Blackhole): Unit =
    bh.consume(st.indexOf(st.listWithMaxCmpWeightElements, st.maxSizeElementToFound))

  @Benchmark
  def indexOfMaxCmpWeightElementInMaxSizeElementsList(st: ListIndexOfSt, bh: Blackhole): Unit =
    bh.consume(st.indexOf(st.listWithMaxSizeElements, st.maxCmpWeightElementToFound))

  @Benchmark
  def lastIndexOfMaxSizeElementInMaxCmpWeightElementsList(st: ListIndexOfSt, bh: Blackhole): Unit =
    bh.consume(st.lastIndexOf(st.listWithMaxCmpWeightElements, st.maxSizeElementToFound))

  @Benchmark
  def lastIndexOfMaxCmpWeightElementInMaxSizeElementsList(st: ListIndexOfSt, bh: Blackhole): Unit =
    bh.consume(st.lastIndexOf(st.listWithMaxSizeElements, st.maxCmpWeightElementToFound))
}

object ListIndexOfBenchmark {
  @State(Scope.Benchmark)
  class ListIndexOfSt {
    val maxCmpWeightElementToFound   = CONST_STRING("a" * ContractLimits.MaxCmpWeight.toInt).explicitGet()
    val maxSizeElementToFound        = CONST_STRING("a" * 150 * 1024).explicitGet()
    val listWithMaxCmpWeightElements = IndexedSeq.fill(1000)(CONST_STRING("a" * (ContractLimits.MaxCmpWeight.toInt - 1) + "b").explicitGet())
    val listWithMaxSizeElements      = IndexedSeq.fill(1000)(CONST_STRING(("a" * (150 * 1024 - 1)) + "b").explicitGet())

    def indexOf(list: Seq[EVALUATED], element: EVALUATED) =
      PureContext.genericListIndexOf(element, list.indexOf, list.indexWhere)

    def lastIndexOf(list: Seq[EVALUATED], element: EVALUATED) =
      PureContext.genericListIndexOf(element, list.lastIndexOf(_), list.lastIndexWhere)
  }
}
