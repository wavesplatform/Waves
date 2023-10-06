package com.wavesplatform.lang.v1

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.evaluator.FunctionIds.REPLACE_BY_INDEX_OF_LIST
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.traits.Environment
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import scala.concurrent.duration.{MICROSECONDS, SECONDS}

@OutputTimeUnit(MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10, time = 1, timeUnit = SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = SECONDS)
class ListReplaceByIndexBenchmark {
  @Benchmark
  def listReplaceFirstByIndex(st: ListReplaceByIndexSt, bh: Blackhole): Unit =
    bh.consume(eval(st.ctx, st.replaceFirst))

  @Benchmark
  def listReplaceMiddleByIndex(st: ListReplaceByIndexSt, bh: Blackhole): Unit =
    bh.consume(eval(st.ctx, st.replaceMiddle))

  @Benchmark
  def listReplaceLastByIndex(st: ListReplaceByIndexSt, bh: Blackhole): Unit =
    bh.consume(eval(st.ctx, st.replaceLast))
}

@State(Scope.Benchmark)
class ListReplaceByIndexSt {
  val ctx =
    PureContext
      .build(StdLibVersion.VersionDic.all.max, useNewPowPrecision = true)
      .withEnvironment[Environment]
      .evaluationContext(Common.emptyBlockchainEnvironment())

  val list = ARR(Vector.fill(1000)(CONST_LONG(Long.MaxValue)), limited = true).explicitGet()

  val replaceFirst =
    FUNCTION_CALL(
      Native(REPLACE_BY_INDEX_OF_LIST),
      List(
        list,
        CONST_LONG(0),
        CONST_LONG(777)
      )
    )

  val replaceMiddle =
    FUNCTION_CALL(
      Native(REPLACE_BY_INDEX_OF_LIST),
      List(
        list,
        CONST_LONG(500),
        CONST_LONG(777)
      )
    )

  val replaceLast =
    FUNCTION_CALL(
      Native(REPLACE_BY_INDEX_OF_LIST),
      List(
        list,
        CONST_LONG(999),
        CONST_LONG(777)
      )
    )
}
