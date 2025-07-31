package com.wavesplatform.lang.v1

import com.google.common.primitives.Longs
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.PureFunctionsRebenchmark.*
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BYTESTR.NoLimit
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types
import com.wavesplatform.lang.{Global, v1}
import org.apache.commons.lang3.RandomStringUtils
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.{ThreadLocalRandom, TimeUnit}
import scala.compiletime.uninitialized

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 20, time = 1)
@Measurement(iterations = 20, time = 1)
class PureFunctionsRebenchmark {
  @Benchmark
  def parseIntValue(st: ParseIntVal, bh: Blackhole): Unit = bh.consume(eval(st.expr))

  @Benchmark
  def toBase58(st: ToBase58, bh: Blackhole): Unit = bh.consume(eval(st.expr))

  @Benchmark
  def fromBase58(st: FromBase58, bh: Blackhole): Unit = bh.consume(eval(st.expr))

  @Benchmark
  def toBase64(st: ToBase64, bh: Blackhole): Unit = bh.consume(eval(st.expr))

  @Benchmark
  def fromBase64(st: FromBase64, bh: Blackhole): Unit = bh.consume(eval(st.expr))

  @Benchmark
  def toBase16(st: ToBase16, bh: Blackhole): Unit = bh.consume(eval(st.expr))

  @Benchmark
  def fromBase16(st: FromBase16, bh: Blackhole): Unit = bh.consume(eval(st.expr))

  @Benchmark
  def sumByteString(st: SumByteString, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def longToBytes(st: LongToBytes, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def stringToBytes(st: StringToBytes, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def takeBytes(st: TakeBytes, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def takeRightBytes(st: TakeRightBytes, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def dropBytes(st: DropBytes, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def dropRightBytes(st: DropRightBytes, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def takeString(st: TakeString, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def takeRightString(st: TakeRightString, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def dropString(st: DropString, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def dropRightString(st: DropString, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def listAppend(st: ListAppend, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def listConstructor(st: ListConstructor, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def listConcat(st: ListConcat, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def listGetSize(st: ListGetSize, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def toUtf8String(st: ToUtf8String, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def bytesToLong(st: BytesToLong, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def stringIndexOf(st: StringIndexOf, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def listGetElement(st: ListGetElement, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def listIndexOf(st: ListIndexOf, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def replaceFirst(st: ReplaceFirst, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def replaceAll(st: ReplaceAll, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def fill(st: Fill, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def listMin(st: ListMin, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def listMax(st: ListMax, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def dataEntryFromArrayByString1(st: DataEntryFromArrayByString1, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def dataEntryFromArrayByString2(st: DataEntryFromArrayByString2, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def dataEntryFromArrayByInt1(st: DataEntryFromArrayByInt1, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def dataEntryFromArrayByInt2(st: DataEntryFromArrayByInt2, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def listReplaceByIndex(st: ListReplaceByIndex, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))

  @Benchmark
  def listRemoveByIndex(st: ListRemoveByIndex, bh: Blackhole): Unit =
    bh.consume(eval(st.expr))
}

object PureFunctionsRebenchmark {
  def randomBytes(length: Int): Array[Byte] = {
    val bytes = new Array[Byte](length)
    ThreadLocalRandom.current().nextBytes(bytes)
    bytes
  }

  @State(Scope.Benchmark)
  class ParseIntVal {
    val expr: EXPR =
      FUNCTION_CALL(
        PureContext.parseIntVal,
        List(
          CONST_STRING(Long.MinValue.toString).explicitGet()
        )
      )
  }

  private def fromBaseStringExpr(string: String, functionId: Short) =
    FUNCTION_CALL(
      Native(functionId),
      List(
        CONST_STRING(string, reduceLimit = false).explicitGet()
      )
    )

  @State(Scope.Benchmark)
  class FromBase58 {
    val expr: EXPR = fromBaseStringExpr(Base58.encode(randomBytes(Global.MaxBase58Bytes)), FunctionIds.FROMBASE58)
  }

  @State(Scope.Benchmark)
  class FromBase64 {
    @Param(Array("1024", "8192", "16383", "32766"))
    var byteCount  = 0
    var expr: EXPR = uninitialized

    @Setup def setup(): Unit = {
      expr = fromBaseStringExpr(Base64.encode(randomBytes(byteCount)), FunctionIds.FROMBASE64)
    }
  }

  @State(Scope.Benchmark)
  class FromBase16 {
    @Param(Array("1024", "8192", "16383"))
    var byteCount  = 0
    var expr: EXPR = uninitialized

    @Setup def setup(): Unit = {
      expr = fromBaseStringExpr(Global.base16Encode(randomBytes(byteCount), None).explicitGet(), FunctionIds.FROMBASE16)
    }
  }

  @State(Scope.Benchmark)
  abstract class ToBaseStr(functionId: Short) {
    def byteCount: Int
    var expr: EXPR = uninitialized
    @Setup def setup(): Unit = {
      expr = FUNCTION_CALL(Native(functionId), List(CONST_BYTESTR(ByteStr(randomBytes(byteCount))).explicitGet()))
    }
  }

  class ToBase58 extends ToBaseStr(FunctionIds.TOBASE58) {
    @Param(Array("32", "64"))
    var byteCount: Int = 0
  }

  class ToBase64 extends ToBaseStr(FunctionIds.TOBASE64) {
    @Param(Array("512", "1024", "24572"))
    var byteCount: Int = 0
  }

  class ToBase16 extends ToBaseStr(FunctionIds.TOBASE16) {
    @Param(Array("512", "1024", "8192"))
    var byteCount = 0
  }

  @State(Scope.Benchmark)
  class SumByteString {
    @Param(Array("1024", "4096", "16384", "32766"))
    var prefixLength = 0
    var expr: EXPR   = uninitialized

    @Setup def setup(): Unit = {
      val byteString1 = ByteStr(Array.fill[Byte](prefixLength)(-127))
      val byteString2 = ByteStr(Array.fill[Byte](32767 - prefixLength)(-127))
      expr = FUNCTION_CALL(
        Native(FunctionIds.SUM_BYTES),
        List(
          CONST_BYTESTR(byteString1).explicitGet(),
          CONST_BYTESTR(byteString2).explicitGet()
        )
      )
    }

  }

  @State(Scope.Benchmark)
  class LongToBytes {
    val long = Long.MinValue
    val expr: EXPR =
      FUNCTION_CALL(
        Native(FunctionIds.LONG_TO_BYTES),
        List(
          CONST_LONG(long)
        )
      )
  }

  @State(Scope.Benchmark)
  class StringToBytes {
    val string = "\uD834\uDD1E\uD833\uDD1E" * 4095
    val expr: EXPR =
      FUNCTION_CALL(
        Native(FunctionIds.STRING_TO_BYTES),
        List(
          CONST_STRING(string).explicitGet()
        )
      )
  }

  @State(Scope.Benchmark)
  class BytesState(functionId: Short) {
    @Param(Array("1024", "4096", "16384", "32766"))
    var byteCount  = 0
    var expr: EXPR = uninitialized

    @Setup def setup(): Unit = {
      expr = FUNCTION_CALL(
        Native(functionId),
        List(
          CONST_BYTESTR(ByteStr(Array.fill[Byte](32767)(-127))).explicitGet(),
          CONST_LONG(byteCount)
        )
      )
    }
  }

  class TakeBytes extends BytesState(FunctionIds.TAKE_BYTES)

  class TakeRightBytes extends BytesState(FunctionIds.TAKE_RIGHT_BYTES)

  class DropBytes extends BytesState(FunctionIds.DROP_BYTES)

  class DropRightBytes extends BytesState(FunctionIds.DROP_RIGHT_BYTES)

  @State(Scope.Benchmark)
  class StringState(functionId: Short) {
    @Param(Array("1024", "4096", "16384", "32766"))
    var prefixLength = 0
    var expr: EXPR   = uninitialized

    @Setup
    def setup(): Unit = {
      val string = "\uD834\uDD1E\uD833\uDD1E" * 8191
      expr = FUNCTION_CALL(
        Native(functionId),
        List(
          CONST_STRING(string).explicitGet(),
          CONST_LONG(prefixLength)
        )
      )
    }
  }

  class DropString extends StringState(FunctionIds.DROP_STRING)

  class DropRightString extends StringState(FunctionIds.DROP_RIGHT_STRING)

  class TakeString extends StringState(FunctionIds.TAKE_STRING)

  class TakeRightString extends StringState(FunctionIds.TAKE_RIGHT_STRING)

  @State(Scope.Benchmark)
  class ListConstructor {
    val list = Vector.fill(999)(CONST_LONG(1))
    val expr: EXPR =
      FUNCTION_CALL(
        Native(FunctionIds.CREATE_LIST),
        List(
          CONST_LONG(1),
          ARR(list, limited = true).explicitGet()
        )
      )
  }

  @State(Scope.Benchmark)
  class ListAppend {
    val list = Vector.fill(999)(CONST_LONG(1))
    val expr: EXPR =
      FUNCTION_CALL(
        Native(FunctionIds.APPEND_LIST),
        List(
          ARR(list, limited = true).explicitGet(),
          CONST_LONG(1)
        )
      )
  }

  @State(Scope.Benchmark)
  class ListConcat {
    @Param(value = Array("1", "100", "500", "999"))
    var prefixLength = 0
    var expr: EXPR   = uninitialized

    @Setup
    def setup(): Unit = {
      expr = FUNCTION_CALL(
        Native(FunctionIds.CONCAT_LIST),
        List(
          ARR(Vector.fill(prefixLength)(CONST_LONG(1)), limited = true).explicitGet(),
          ARR(Vector.fill(1000 - prefixLength)(CONST_LONG(1)), limited = true).explicitGet()
        )
      )
    }
  }

  @State(Scope.Benchmark)
  class ToUtf8String {
    val bytes = RandomStringUtils.insecure().next(10000).getBytes("utf-8")
    val expr: EXPR =
      FUNCTION_CALL(
        Native(FunctionIds.UTF8STRING),
        List(
          CONST_BYTESTR(ByteStr(bytes), CONST_BYTESTR.NoLimit).explicitGet()
        )
      )
  }

  @State(Scope.Benchmark)
  class BytesToLong {
    val longBytes = Longs.toByteArray(Long.MinValue)
    val expr: EXPR =
      FUNCTION_CALL(
        Native(FunctionIds.BININT),
        List(
          CONST_BYTESTR(ByteStr(longBytes)).explicitGet()
        )
      )
  }

  @State(Scope.Benchmark)
  class StringIndexOf {
    val expr: EXPR =
      FUNCTION_CALL(
        Native(FunctionIds.INDEXOF),
        List(
          CONST_STRING("b" * 32766 + "a").explicitGet(),
          CONST_STRING("a").explicitGet()
        )
      )
  }

  @State(Scope.Benchmark)
  class ListGetElement {
    @Param(Array("0", "500", "999"))
    var index      = 0
    var expr: EXPR = uninitialized

    @Setup def setup(): Unit = {
      expr = FUNCTION_CALL(
        Native(FunctionIds.GET_LIST),
        List(
          ARR(Vector.fill(1000)(CONST_LONG(1)), limited = true).explicitGet(),
          CONST_LONG(index)
        )
      )
    }
  }

  @State(Scope.Benchmark)
  class ListIndexOf {
    @Param(Array("500,648", "1000,324")) // make list as heavy as possible
    var listAndStringSize = ""
    var expr: EXPR        = uninitialized

    @Setup def setup(): Unit = {
      val Array(listSize, stringSize) = listAndStringSize.split(",").map(_.toInt)
      val otherString                 = CONST_STRING("a" * (stringSize - 1) + "b").explicitGet()
      val searchString                = CONST_STRING("a" * stringSize).explicitGet()
      expr = FUNCTION_CALL(
        Native(FunctionIds.INDEX_OF_LIST),
        List(
          ARR(IndexedSeq.fill(listSize - 1)(otherString) :+ searchString, limited = true).explicitGet(),
          searchString
        )
      )
    }
  }

  @State(Scope.Benchmark)
  class ListGetSize {
    @Param(Array("1", "500", "1000"))
    var listSize   = 0
    var expr: EXPR = uninitialized

    @Setup def setup(): Unit = {
      expr = FUNCTION_CALL(
        Native(FunctionIds.SIZE_LIST),
        List(
          ARR(Vector.fill(listSize)(CONST_LONG(1)), limited = true).explicitGet()
        )
      )
    }
  }

  @State(Scope.Benchmark)
  class ReplaceFirst {
    @Param(Array("1", "1024", "2047", "4096", "16383"))
    var prefixSize = 0
    var expr: EXPR = uninitialized

    @Setup def setup(): Unit = {
      expr = FUNCTION_CALL(
        Native(FunctionIds.REPLACEFIRST),
        List(
          CONST_STRING("A" * (16384 - prefixSize) + "$" * prefixSize).explicitGet(),
          CONST_STRING("$" * prefixSize).explicitGet(),
          CONST_STRING("B" * prefixSize).explicitGet()
        )
      )
    }
  }

  @State(Scope.Benchmark)
  class ReplaceAll {
    @Param(Array("1", "1024", "4096", "32765"))
    var prefixSize = 0
    var expr: EXPR = uninitialized

    @Setup def setup(): Unit = {
      expr = FUNCTION_CALL(
        Native(FunctionIds.REPLACEALL),
        List(
          CONST_STRING("$" * prefixSize + "A" * (32766 - prefixSize)).explicitGet(),
          CONST_STRING("$").explicitGet(),
          CONST_STRING("B").explicitGet()
        )
      )
    }
  }

  @State(Scope.Benchmark)
  class Fill {
    @Param(Array("1", "1000"))
    var length     = 0
    var expr: EXPR = uninitialized

    @Setup def setup(): Unit = {
      expr = FUNCTION_CALL(
        Native(FunctionIds.FILL_LIST),
        List(
          CONST_LONG(length),
          CONST_BYTESTR(ByteStr(new Array[Byte](312)), NoLimit).explicitGet()
        )
      )
    }
  }

  @State(Scope.Benchmark)
  class ListMinMax(functionId: Short) {
    val expr = FUNCTION_CALL(
      Native(functionId),
      List(
        ARR((1 to 1000).map(i => CONST_LONG(i)).toVector, limited = true).explicitGet()
      )
    )
  }

  class ListMin extends ListMinMax(FunctionIds.MIN_LIST)
  class ListMax extends ListMinMax(FunctionIds.MAX_LIST)

  @State(Scope.Benchmark)
  class DataEntryFromArray(funcHeader: FunctionHeader, selector: CONST_LONG | CONST_STRING, keyLength: Int, listLength: Int) {
    val expr: EXPR = FUNCTION_CALL(
      funcHeader,
      List(
        ARR(
          Vector.fill(listLength)(
            CaseObj(Types.stringDataEntry, Map("key" -> CONST_STRING("a" * (keyLength - 1) + "b").explicitGet(), "value" -> CONST_LONG(1)))
          ),
          limited = true
        ).explicitGet(),
        selector
      )
    )
  }

  class DataEntryFromArrayByString1
      extends DataEntryFromArray(Native(FunctionIds.DATA_LONG_FROM_ARRAY), CONST_STRING("a" * 400).explicitGet(), 399, 652)
  class DataEntryFromArrayByString2
      extends DataEntryFromArray(Native(FunctionIds.DATA_LONG_FROM_ARRAY), CONST_STRING("a" * 400).explicitGet(), 216, 1000)
  class DataEntryFromArrayByInt1 extends DataEntryFromArray(FunctionHeader.User("getInteger"), CONST_LONG(651), 399, 652)
  class DataEntryFromArrayByInt2 extends DataEntryFromArray(FunctionHeader.User("getInteger"), CONST_LONG(999), 216, 1000)

  @State(Scope.Benchmark)
  class ListReplaceByIndex {
    @Param(Array("0", "500", "999"))
    var index      = 0
    var expr: EXPR = uninitialized

    @Setup def setup(): Unit = {
      expr = FUNCTION_CALL(
        Native(FunctionIds.REPLACE_BY_INDEX_OF_LIST),
        List(
          ARR(Vector.fill(1000)(CONST_LONG(1)), limited = true).explicitGet(),
          CONST_LONG(index),
          CONST_LONG(100)
        )
      )
    }
  }

  @State(Scope.Benchmark)
  class ListRemoveByIndex {
    @Param(Array("0", "500", "999"))
    var index      = 0
    var expr: EXPR = uninitialized

    @Setup def setup(): Unit = {
      expr = FUNCTION_CALL(
        Native(FunctionIds.REMOVE_BY_INDEX_OF_LIST),
        List(
          ARR(Vector.fill(1000)(CONST_LONG(1)), limited = true).explicitGet(),
          CONST_LONG(index)
        )
      )
    }
  }
}
