package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.lang.v1.EnvironmentFunctionsBenchmark.randomBytes
import com.wavesplatform.lang.v1.DataFuncs._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 30)
@Measurement(iterations = 30)
class DataFuncs {
  @Benchmark
  def decode64_35Kb(st: StrSt35K, bh: Blackhole): Unit =
    bh.consume(Base64.decode(st.message))

  @Benchmark
  def decode64_70Kb(st: StrSt70K, bh: Blackhole): Unit =
    bh.consume(Base64.decode(st.message))

  @Benchmark
  def decode64_105Kb(st: StrSt105K, bh: Blackhole): Unit =
    bh.consume(Base64.decode(st.message))

  @Benchmark
  def decode64_140Kb(st: StrSt140K, bh: Blackhole): Unit =
    bh.consume(Base64.decode(st.message))

  @Benchmark
  def decode64_175Kb(st: StrSt175K, bh: Blackhole): Unit =
    bh.consume(Base64.decode(st.message))

  @Benchmark
  def encode64_26Kb(st: BinSt26K, bh: Blackhole): Unit =
    bh.consume(Base64.encode(st.message))

  @Benchmark
  def encode64_52Kb(st: BinSt52K, bh: Blackhole): Unit =
    bh.consume(Base64.encode(st.message))

  @Benchmark
  def encode64_78Kb(st: BinSt78K, bh: Blackhole): Unit =
    bh.consume(Base64.encode(st.message))

  @Benchmark
  def encode64_104Kb(st: BinSt104K, bh: Blackhole): Unit =
    bh.consume(Base64.encode(st.message))

  @Benchmark
  def encode64_130Kb(st: BinSt130K, bh: Blackhole): Unit =
    bh.consume(Base64.encode(st.message))

  @Benchmark
  def concat_35Kb(st: StrSt35K, bh: Blackhole): Unit =
    bh.consume(st.message ++ "q")

  @Benchmark
  def concat_70Kb(st: StrSt70K, bh: Blackhole): Unit =
    bh.consume(st.message ++ "q")

  @Benchmark
  def concat_105Kb(st: StrSt105K, bh: Blackhole): Unit =
    bh.consume(st.message ++ "q")

  @Benchmark
  def concat_140Kb(st: StrSt140K, bh: Blackhole): Unit =
    bh.consume(st.message ++ "q")

  @Benchmark
  def concat_175Kb(st: StrSt175K, bh: Blackhole): Unit =
    bh.consume(st.message ++ "q")

  @Benchmark
  def concatr_35Kb(st: StrSt35K, bh: Blackhole): Unit =
    bh.consume("q" ++ st.message)

  @Benchmark
  def concatr_70Kb(st: StrSt70K, bh: Blackhole): Unit =
    bh.consume("q" ++ st.message)

  @Benchmark
  def concatr_105Kb(st: StrSt105K, bh: Blackhole): Unit =
    bh.consume("q" ++ st.message)

  @Benchmark
  def concatr_140Kb(st: StrSt140K, bh: Blackhole): Unit =
    bh.consume("q" ++ st.message)

  @Benchmark
  def concatr_175Kb(st: StrSt175K, bh: Blackhole): Unit =
    bh.consume("q" ++ st.message)


}

object DataFuncs {
  @State(Scope.Benchmark)
  class StrSt35K extends StrSt(35)
  @State(Scope.Benchmark)
  class StrSt70K extends StrSt(70)
  @State(Scope.Benchmark)
  class StrSt105K extends StrSt(105)
  @State(Scope.Benchmark)
  class StrSt140K extends StrSt(140)
  @State(Scope.Benchmark)
  class StrSt175K extends StrSt(175)

  class StrSt(size: Int) {
    val message   = "B" * (size * 1024)
  }

  @State(Scope.Benchmark)
  class BinSt26K extends BinSt(26)
  @State(Scope.Benchmark)
  class BinSt52K extends BinSt(52)
  @State(Scope.Benchmark)
  class BinSt78K extends BinSt(78)
  @State(Scope.Benchmark)
  class BinSt104K extends BinSt(104)
  @State(Scope.Benchmark)
  class BinSt130K extends BinSt(130)

  class BinSt(size: Int) {
    val message   = randomBytes(size * 1024)
  }
}
