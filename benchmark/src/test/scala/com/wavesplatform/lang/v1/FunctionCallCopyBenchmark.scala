package com.wavesplatform.lang.v1

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.FunctionCallCopyBenchmark.FuncCallSt
import com.wavesplatform.lang.v1.compiler.Terms.*
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
class FunctionCallCopyBenchmark {
  @Benchmark
  def twoLevelsDeep(st: FuncCallSt, bh: Blackhole): Unit =
    bh.consume(st.twoLevelsDeep.deepCopy.value)

  @Benchmark
  def threeLevelsDeep(st: FuncCallSt, bh: Blackhole): Unit =
    bh.consume(st.threeLevelsDeep.deepCopy.value)

  @Benchmark
  def fourLevelsDeep(st: FuncCallSt, bh: Blackhole): Unit =
    bh.consume(st.fourLevelsDeep.deepCopy.value)
}

object FunctionCallCopyBenchmark {

  @State(Scope.Benchmark)
  class FuncCallSt {
    val simpleCall: FUNCTION_CALL = FUNCTION_CALL(
      FunctionHeader.User("fooBar"),
      List(
        CONST_STRING("foobar").explicitGet(),
        CONST_LONG(1000L),
        CONST_BOOLEAN(false)
      )
    )

    val twoLevelsDeep: FUNCTION_CALL = FUNCTION_CALL(
      FunctionHeader.User("fooBar"),
      List(
        FUNCTION_CALL(FunctionHeader.User("fooBar"), List.fill(3)(simpleCall)),
        FUNCTION_CALL(FunctionHeader.User("fooBar"), List.fill(3)(simpleCall)),
        FUNCTION_CALL(FunctionHeader.User("fooBar"), List.fill(3)(simpleCall))
      )
    )

    val threeLevelsDeep: FUNCTION_CALL = FUNCTION_CALL(
      FunctionHeader.User("fooBar"),
      List(
        FUNCTION_CALL(FunctionHeader.User("fooBar"), List.fill(3)(twoLevelsDeep)),
        FUNCTION_CALL(FunctionHeader.User("fooBar"), List.fill(3)(twoLevelsDeep)),
        FUNCTION_CALL(FunctionHeader.User("fooBar"), List.fill(3)(twoLevelsDeep))
      )
    )

    val fourLevelsDeep: FUNCTION_CALL = FUNCTION_CALL(
      FunctionHeader.User("fooBar"),
      List(
        FUNCTION_CALL(FunctionHeader.User("fooBar"), List.fill(3)(threeLevelsDeep)),
        FUNCTION_CALL(FunctionHeader.User("fooBar"), List.fill(3)(threeLevelsDeep)),
        FUNCTION_CALL(FunctionHeader.User("fooBar"), List.fill(3)(threeLevelsDeep))
      )
    )
  }
}
