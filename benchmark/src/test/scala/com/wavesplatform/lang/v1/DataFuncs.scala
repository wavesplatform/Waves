package com.wavesplatform.lang.v1

import com.esaulpaugh.headlong.util.FastHex
import com.sun.org.apache.xerces.internal.impl.dv.util.HexBin
import com.wavesplatform.common.utils.*
import com.wavesplatform.lang.v1.DataFuncs.*
import com.wavesplatform.lang.v1.EnvironmentFunctionsBenchmark.randomBytes
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
class DataFuncs {
  @Benchmark
  def decode64_35Kb(st: StrSt35K, bh: Blackhole): Unit =
    bh.consume(Base64.decode(st.message))

  @Benchmark
  def decode16_32kb_bcprov(st: StrSt105K, bh: Blackhole): Unit =
    bh.consume(org.bouncycastle.util.encoders.Hex.decode(st.message))

  @Benchmark
  def decode16_32kb_guava(st: StrSt105K, bh: Blackhole): Unit =
    bh.consume(com.google.common.io.BaseEncoding.base16.decode(st.message))

  @Benchmark
  def decode16_32kb_commons_codec(st: StrSt105K, bh: Blackhole): Unit =
    bh.consume(org.apache.commons.codec.binary.Hex.decodeHex(st.message))

  @Benchmark
  def decode16_32kb_web3j(st: StrSt105K, bh: Blackhole): Unit =
    bh.consume(org.web3j.utils.Numeric.hexStringToByteArray(st.message))

  @Benchmark
  def decode16_32kb_headlong(st: StrSt105K, bh: Blackhole): Unit =
    bh.consume(FastHex.decode(st.message))

  @Benchmark
  def decode16_32kb_jdk_hexbin(st: StrSt105K, bh: Blackhole): Unit =
    bh.consume(HexBin.decode(st.message))

  @Benchmark
  def decode64_70Kb(st: StrSt70K, bh: Blackhole): Unit =
    bh.consume(Base64.decode(st.message))

  @Benchmark
  def decode64_105Kb_jdk(st: StrSt105K, bh: Blackhole): Unit =
    bh.consume(java.util.Base64.getDecoder.decode(st.message))

  @Benchmark
  def decode64_105Kb_bcprov(st: StrSt105K, bh: Blackhole): Unit =
    bh.consume(org.bouncycastle.util.encoders.Base64.decode(st.message))

  @Benchmark
  def decode64_105Kb_guava(st: StrSt105K, bh: Blackhole): Unit =
    bh.consume(com.google.common.io.BaseEncoding.base64().decode(st.message))

  @Benchmark
  def decode64_105Kb_commons_codec(st: StrSt105K, bh: Blackhole): Unit =
    bh.consume(org.apache.commons.codec.binary.Base64.decodeBase64(st.message))

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

  @Benchmark
  def decode58_16b(st: StrSt16b, bh: Blackhole): Unit =
    bh.consume(Base58.decode(st.message))

  @Benchmark
  def decode58_128bb(st: StrSt128b, bh: Blackhole): Unit =
    bh.consume(Base58.decode(st.message))

  @Benchmark
  def decode58_256Kb(st: StrSt256b, bh: Blackhole): Unit =
    bh.consume(Base58.decode(st.message))

  @Benchmark
  def decode58_512b(st: StrSt512b, bh: Blackhole): Unit =
    bh.consume(Base58.decode(st.message))

  @Benchmark
  def decode58_768b(st: StrSt768b, bh: Blackhole): Unit =
    bh.consume(Base58.decode(st.message))

  @Benchmark
  def decode58_896b(st: StrSt896b, bh: Blackhole): Unit =
    bh.consume(Base58.decode(st.message))

  @Benchmark
  def encode58_16b(st: StrSt16b, bh: Blackhole): Unit =
    bh.consume(Base58.encode(st.bmessage))

  @Benchmark
  def encode58_128bb(st: StrSt128b, bh: Blackhole): Unit =
    bh.consume(Base58.encode(st.bmessage))

  @Benchmark
  def encode58_256Kb(st: StrSt256b, bh: Blackhole): Unit =
    bh.consume(Base58.encode(st.bmessage))

  @Benchmark
  def encode58_512b(st: StrSt512b, bh: Blackhole): Unit =
    bh.consume(Base58.encode(st.bmessage))

  @Benchmark
  def encode58_768b(st: StrSt768b, bh: Blackhole): Unit =
    bh.consume(Base58.encode(st.bmessage))

  @Benchmark
  def encode58_896b(st: StrSt896b, bh: Blackhole): Unit =
    bh.consume(Base58.encode(st.bmessage))

}

object DataFuncs {
  @State(Scope.Benchmark)
  class StrSt8K extends StrSt(8)
  @State(Scope.Benchmark)
  class StrSt35K extends StrSt(35)
  @State(Scope.Benchmark)
  class StrSt70K extends StrSt(70)
  @State(Scope.Benchmark)
  class StrSt105K extends StrSt(32)
  @State(Scope.Benchmark)
  class StrSt140K extends StrSt(140)
  @State(Scope.Benchmark)
  class StrSt175K extends StrSt(175)

  class StrSt(size: Int) {
    val message = "B" * (size * 1024)
  }

  @State(Scope.Benchmark)
  class StrSt16b extends StrStS(16)
  @State(Scope.Benchmark)
  class StrSt128b extends StrStS(128)
  @State(Scope.Benchmark)
  class StrSt256b extends StrStS(256)
  @State(Scope.Benchmark)
  class StrSt512b extends StrStS(512)
  @State(Scope.Benchmark)
  class StrSt768b extends StrStS(768)
  @State(Scope.Benchmark)
  class StrSt896b extends StrStS(896)

  class StrStS(size: Int) {
    val message  = "B" * size
    val bmessage = randomBytes(size)
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
    val message = randomBytes(size * 1024)
  }
}
