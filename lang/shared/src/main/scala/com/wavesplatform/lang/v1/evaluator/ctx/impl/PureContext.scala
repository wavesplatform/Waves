package com.wavesplatform.lang.v1.evaluator.ctx.impl

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.charset.{MalformedInputException, StandardCharsets}
import java.nio.{BufferUnderflowException, ByteBuffer}

import cats.Id
import cats.implicits._
import com.google.common.annotations.VisibleForTesting
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.ContractLimits._
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.ContextfulVal
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.parser.BinaryOperation
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import com.wavesplatform.lang.v1.{BaseGlobal, CTX}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.{Success, Try}

object PureContext {
  private val global: BaseGlobal = com.wavesplatform.lang.Global

  implicit def intToLong(num: Int): Long = num.toLong

  private val defaultThrowMessage      = "Explicit script termination"
  val MaxListLengthV4                  = 1000

  lazy val mulLong: BaseFunction[NoContext] =
    createTryOp(MUL_OP, LONG, LONG, MUL_LONG)((a, b) => Math.multiplyExact(a, b))
  lazy val divLong: BaseFunction[NoContext] =
    createTryOp(DIV_OP, LONG, LONG, DIV_LONG)((a, b) => Math.floorDiv(a, b))
  lazy val modLong: BaseFunction[NoContext] =
    createTryOp(MOD_OP, LONG, LONG, MOD_LONG)((a, b) => Math.floorMod(a, b))
  lazy val sumLong: BaseFunction[NoContext] =
    createTryOp(SUM_OP, LONG, LONG, SUM_LONG)((a, b) => Math.addExact(a, b))
  lazy val subLong: BaseFunction[NoContext] =
    createTryOp(SUB_OP, LONG, LONG, SUB_LONG)((a, b) => Math.subtractExact(a, b))
  lazy val sumString: BaseFunction[NoContext] =
    createRawOp(
      SUM_OP,
      STRING,
      STRING,
      SUM_STRING,
      Map[StdLibVersion, Long](V1 -> 10L, V2 -> 10L, V3 -> 10L, V4 -> 20L)
    ) {
      case (CONST_STRING(a), CONST_STRING(b)) =>
        if (a.length + b.length <= Terms.DataEntryValueMax) {
          CONST_STRING(a + b)
        } else {
          Left(s"String length = ${a.length + b.length} exceeds ${Terms.DataEntryValueMax}")
        }
      case args =>
        Left(s"Unexpected args $args for string concatenation operator")
    }
  lazy val sumByteStr: BaseFunction[NoContext] =
    createRawOp(
      SUM_OP,
      BYTESTR,
      BYTESTR,
      SUM_BYTES,
      Map[StdLibVersion, Long](V1 -> 10L, V2 -> 10L, V3 -> 10L, V4 -> 2L),
    ) {
      case (CONST_BYTESTR(a), CONST_BYTESTR(b)) =>
        if (a.arr.length + b.arr.length <= Terms.DataEntryValueMax) {
          CONST_BYTESTR(a ++ b)
        } else {
          Left(s"ByteVector size = ${a.arr.length + b.arr.length} bytes exceeds ${Terms.DataEntryValueMax}")
        }
      case args =>
        Left(s"Unexpected args $args for bytes concatenation operator")
    }
  lazy val ge: BaseFunction[NoContext] = createOp(GE_OP, LONG, BOOLEAN, GE_LONG)(_ >= _)
  lazy val gt: BaseFunction[NoContext] =
    createOp(GT_OP, LONG, BOOLEAN, GT_LONG)(_ > _)

  lazy val eq: BaseFunction[NoContext] =
    NativeFunction(EQ_OP.func, 1, EQ, BOOLEAN, ("a", TYPEPARAM('T')), ("b", TYPEPARAM('T'))) {
      case a :: b :: Nil =>
        Either.cond(b.weight <= MaxCmpWeight || a.weight <= MaxCmpWeight, CONST_BOOLEAN(a == b), "Comparable value too heavy.")
      case xs => notImplemented[Id, EVALUATED](s"${EQ_OP.func}(a: T, b: T)", xs)
    }

  lazy val ne: BaseFunction[NoContext] =
    UserFunction(NE_OP.func, Map[StdLibVersion, Long](V1 -> 26, V2 -> 26, V3 -> 1, V4 -> 1), BOOLEAN, ("@a", TYPEPARAM('T')), ("@b", TYPEPARAM('T'))) {
      FUNCTION_CALL(uNot, List(FUNCTION_CALL(eq, List(REF("@a"), REF("@b")))))
    }

  lazy val throwWithMessage: BaseFunction[NoContext] = NativeFunction("throw", 1, THROW, NOTHING, ("err", STRING)) {
    case CONST_STRING(s) :: Nil => Left(s)
    case _                      => Left(defaultThrowMessage)
  }

  lazy val throwNoMessage: BaseFunction[NoContext] = UserFunction(
    "throw",
    Map[StdLibVersion, Long](V1 -> 2, V2 -> 2, V3 -> 1, V4 -> 1),
    NOTHING
  ) {
    FUNCTION_CALL(throwWithMessage, List(CONST_STRING(defaultThrowMessage).explicitGet()))
  }

  lazy val extract: BaseFunction[NoContext] =
    UserFunction.deprecated(
      "extract",
      13,
      TYPEPARAM('T'),
      ("@a", PARAMETERIZEDUNION(List(TYPEPARAM('T'), UNIT)))
    ) {
      IF(
        FUNCTION_CALL(eq, List(REF("@a"), REF("unit"))),
        FUNCTION_CALL(throwWithMessage, List(CONST_STRING("extract() called on unit value").explicitGet())),
        REF("@a")
      )
    }

  lazy val value: BaseFunction[NoContext] =
    UserFunction(
      "value",
      Map[StdLibVersion, Long](V1 -> 13, V2 -> 13, V3 -> 13, V4 -> 2),
      TYPEPARAM('T'),
      ("@a", PARAMETERIZEDUNION(List(TYPEPARAM('T'), UNIT)): TYPE)
    ) {
      IF(
        FUNCTION_CALL(eq, List(REF("@a"), REF("unit"))),
        FUNCTION_CALL(throwWithMessage, List(CONST_STRING("value() called on unit value").explicitGet())),
        REF("@a")
      )
    }

  lazy val valueOrElse: BaseFunction[NoContext] =
    UserFunction(
      "valueOrElse",
      2,
      TYPEPARAM('T'),
      ("@value", PARAMETERIZEDUNION(List(TYPEPARAM('T'), UNIT))),
      ("@alternative", TYPEPARAM('T'))
    ) {
      IF(
        FUNCTION_CALL(eq, List(REF("@value"), REF("unit"))),
        REF("@alternative"),
        REF("@value")
      )
    }

  lazy val valueOrErrorMessage: BaseFunction[NoContext] =
    UserFunction(
      "valueOrErrorMessage",
      Map[StdLibVersion, Long](V1 -> 13, V2 -> 13, V3 -> 13, V4 -> 2),
      TYPEPARAM('T'),
      ("@a", PARAMETERIZEDUNION(List(TYPEPARAM('T'), UNIT))),
      ("@msg", STRING)
    ) {
      IF(
        FUNCTION_CALL(eq, List(REF("@a"), REF("unit"))),
        FUNCTION_CALL(throwWithMessage, List(REF("@msg"))),
        REF("@a")
      )
    }

  lazy val isDefined: BaseFunction[NoContext] =
    UserFunction(
      "isDefined",
      Map[StdLibVersion, Long](V1 -> 35, V2 -> 35, V3 -> 1, V4 -> 1),
      BOOLEAN,
      ("@a", PARAMETERIZEDUNION(List(TYPEPARAM('T'), UNIT)))
    ) {
      FUNCTION_CALL(ne, List(REF("@a"), REF("unit")))
    }

  lazy val fraction: BaseFunction[NoContext] =
    NativeFunction(
      "fraction",
      1,
      FRACTION,
      LONG,
      ("value", LONG),
      ("numerator", LONG),
      ("denominator", LONG)
    ) {
      case CONST_LONG(v) :: CONST_LONG(n) :: CONST_LONG(d) :: Nil =>
        lazy val result = BigInt(v) * n / d
        for {
          _ <- Either.cond(result < Long.MaxValue, (), s"Long overflow: value `$result` greater than 2^63-1")
          _ <- Either.cond(result > Long.MinValue, (), s"Long overflow: value `$result` less than -2^63-1")
        } yield CONST_LONG(result.toLong)
      case xs => notImplemented[Id, EVALUATED]("fraction(value: Int, numerator: Int, denominator: Int)", xs)
    }

  lazy val _isInstanceOf: BaseFunction[NoContext] =
    NativeFunction("_isInstanceOf", 1, ISINSTANCEOF, BOOLEAN, ("obj", TYPEPARAM('T')), ("of", STRING)) {
      case (value: EVALUATED) :: CONST_STRING(expectedType) :: Nil =>
        Right(CONST_BOOLEAN(value.getType.name == expectedType))
      case _ =>
        Right(FALSE)
    }

  lazy val sizeBytes: BaseFunction[NoContext] = NativeFunction("size", 1, SIZE_BYTES, LONG, ("byteVector", BYTESTR)) {
    case CONST_BYTESTR(bv) :: Nil => Right(CONST_LONG(bv.arr.length))
    case xs                       => notImplemented[Id, EVALUATED]("size(byteVector: ByteVector)", xs)
  }

  lazy val toBytesBoolean: BaseFunction[NoContext] =
    NativeFunction("toBytes", 1, BOOLEAN_TO_BYTES, BYTESTR, ("b", BOOLEAN)) {
      case TRUE :: Nil  => CONST_BYTESTR(ByteStr.fromBytes(1))
      case FALSE :: Nil => CONST_BYTESTR(ByteStr.fromBytes(0))
      case xs           => notImplemented[Id, EVALUATED]("toBytes(b: Boolean)", xs)
    }

  lazy val toBytesLong: BaseFunction[NoContext] = NativeFunction("toBytes", 1, LONG_TO_BYTES, BYTESTR, ("n", LONG)) {
    case CONST_LONG(n) :: Nil => CONST_BYTESTR(ByteStr.fromLong(n))
    case xs                   => notImplemented[Id, EVALUATED]("toBytes(u: Int)", xs)
  }

  lazy val toBytesString: BaseFunction[NoContext] =
    NativeFunction(
      "toBytes",
      Map[StdLibVersion, Long](V1 -> 1L, V2 -> 1L, V3 -> 1L, V4 -> 8L),
      STRING_TO_BYTES,
      BYTESTR,
      ("s", STRING)
    ) {
      case CONST_STRING(s) :: Nil => CONST_BYTESTR(ByteStr(s.getBytes(StandardCharsets.UTF_8)))
      case xs                     => notImplemented[Id, EVALUATED]("toBytes(s: String)", xs)
    }

  lazy val sizeString: BaseFunction[NoContext] = NativeFunction("size", 1, SIZE_STRING, LONG, ("xs", STRING)) {
    case CONST_STRING(bv) :: Nil => Right(CONST_LONG(bv.length.toLong))
    case xs                      => notImplemented[Id, EVALUATED]("size(xs: String)", xs)
  }

  lazy val toStringBoolean: BaseFunction[NoContext] =
    NativeFunction("toString", 1, BOOLEAN_TO_STRING, STRING, ("b", BOOLEAN)) {
      case TRUE :: Nil  => CONST_STRING("true")
      case FALSE :: Nil => CONST_STRING("false")
      case xs           => notImplemented[Id, EVALUATED]("toString(b: Boolean)", xs)
    }

  lazy val toStringLong: BaseFunction[NoContext] = NativeFunction("toString", 1, LONG_TO_STRING, STRING, ("n", LONG)) {
    case CONST_LONG(n) :: Nil => CONST_STRING(n.toString)
    case xs                   => notImplemented[Id, EVALUATED]("toString(u: Int)", xs)
  }

  lazy val takeBytes: BaseFunction[NoContext] =
    NativeFunction(
      "take",
      Map[StdLibVersion, Long](V1 -> 1L, V2 -> 1L, V3 -> 1L, V4 -> 6L),
      TAKE_BYTES,
      BYTESTR,
      ("xs", BYTESTR), ("number", LONG)
    ) {
      case CONST_BYTESTR(xs) :: CONST_LONG(number) :: Nil => CONST_BYTESTR(xs.take(number))
      case xs                                             => notImplemented[Id, EVALUATED]("take(xs: ByteVector, number: Int)", xs)
    }

  lazy val dropBytes: BaseFunction[NoContext] =
    NativeFunction(
      "drop",
      Map[StdLibVersion, Long](V1 -> 1L, V2 -> 1L, V3 -> 1L, V4 -> 6L),
      DROP_BYTES,
      BYTESTR,
      ("xs", BYTESTR), ("number", LONG)
    ) {
      case CONST_BYTESTR(xs) :: CONST_LONG(number) :: Nil => CONST_BYTESTR(xs.drop(number))
      case xs                                             => notImplemented[Id, EVALUATED]("drop(xs: ByteVector, number: Int)", xs)
    }

  lazy val dropRightBytes: BaseFunction[NoContext] =
    UserFunction(
      "dropRight", "dropRightBytes",
      Map[StdLibVersion, Long](V1 -> 19L, V2 -> 19L, V3 -> 19L, V4 -> 6L),
      BYTESTR,
      ("@xs", BYTESTR), ("@number", LONG)
    ) {
      FUNCTION_CALL(
        takeBytes,
        List(
          REF("@xs"),
          FUNCTION_CALL(
            subLong,
            List(
              FUNCTION_CALL(sizeBytes, List(REF("@xs"))),
              REF("@number")
            )
          )
        )
      )
    }

  lazy val takeRightBytes: BaseFunction[NoContext] =
    UserFunction(
      "takeRight", "takeRightBytes",
      Map[StdLibVersion, Long](V1 -> 19L, V2 -> 19L, V3 -> 19L, V4 -> 6L),
      BYTESTR,
      ("@xs", BYTESTR), ("@number", LONG)
    ) {
      FUNCTION_CALL(
        dropBytes,
        List(
          REF("@xs"),
          FUNCTION_CALL(
            subLong,
            List(
              FUNCTION_CALL(sizeBytes, List(REF("@xs"))),
              REF("@number")
            )
          )
        )
      )
    }

  private def trimLongToInt(x: Long): Int = Math.toIntExact(Math.max(Math.min(x, Int.MaxValue), Int.MinValue))

  lazy val takeString: BaseFunction[NoContext] =
    NativeFunction(
      "take",
      Map[StdLibVersion, Long](V1 -> 1L, V2 -> 1L, V3 -> 1L, V4 -> 20L),
      TAKE_STRING,
      STRING,
      ("xs", STRING), ("number", LONG)
    ) {
      case CONST_STRING(xs) :: CONST_LONG(number) :: Nil => CONST_STRING(xs.take(trimLongToInt(number)))
      case xs                                            => notImplemented[Id, EVALUATED]("take(xs: String, number: Int)", xs)
    }

  def listConstructor(checkSize: Boolean): NativeFunction[NoContext] =
    NativeFunction(
      "cons",
      Map[StdLibVersion, Long](V1 -> 2L, V2 -> 2L, V3 -> 2L, V4 -> 1L),
      CREATE_LIST,
      PARAMETERIZEDLIST(PARAMETERIZEDUNION(List(TYPEPARAM('A'), TYPEPARAM('B')))),
      ("head", TYPEPARAM('A')),
      ("tail", PARAMETERIZEDLIST(TYPEPARAM('B')))
    ) {
      case h :: (a @ ARR(t)) :: Nil => ARR(h +: t, h.weight + a.weight + ELEM_WEIGHT, checkSize)
      case xs                       => notImplemented[Id, EVALUATED]("cons(head: T, tail: LIST[T]", xs)
    }

  lazy val listAppend: NativeFunction[NoContext] =
    NativeFunction(
      LIST_APPEND_OP.func,
      1,
      APPEND_LIST,
      PARAMETERIZEDLIST(PARAMETERIZEDUNION(List(TYPEPARAM('A'), TYPEPARAM('B')))),
      ("list", PARAMETERIZEDLIST(TYPEPARAM('A'))),
      ("element", TYPEPARAM('B'))
    ) {
      case (a @ ARR(list)) :: element :: Nil => ARR(list :+ element, a.weight + element.weight + ELEM_WEIGHT, true)
      case xs                                => notImplemented[Id, EVALUATED](s"list: List[T] ${LIST_APPEND_OP.func} value: T", xs)
    }

  lazy val listConcat: NativeFunction[NoContext] =
    NativeFunction(
      LIST_CONCAT_OP.func,
      4,
      CONCAT_LIST,
      PARAMETERIZEDLIST(PARAMETERIZEDUNION(List(TYPEPARAM('A'), TYPEPARAM('B')))),
      ("list1", PARAMETERIZEDLIST(TYPEPARAM('A'))),
      ("list2", PARAMETERIZEDLIST(TYPEPARAM('B')))
    ) {
      case (a1 @ ARR(l1)) :: (a2 @ ARR(l2)) :: Nil => ARR(l1 ++ l2, a1.weight + a2.weight - EMPTYARR_WEIGHT, true)
      case xs                                      => notImplemented[Id, EVALUATED](s"list1: List[T] ${LIST_CONCAT_OP.func} list2: List[T]", xs)
    }

  lazy val dropString: BaseFunction[NoContext] =
    NativeFunction(
      "drop",
      Map[StdLibVersion, Long](V1 -> 1L, V2 -> 1L, V3 -> 1L, V4 -> 20L),
      DROP_STRING,
      STRING,
      ("xs", STRING), ("number", LONG)
    ) {
      case CONST_STRING(xs) :: CONST_LONG(number) :: Nil => CONST_STRING(xs.drop(trimLongToInt(number)))
      case xs                                            => notImplemented[Id, EVALUATED]("drop(xs: String, number: Int)", xs)
    }

  lazy val takeRightString: BaseFunction[NoContext] =
    UserFunction(
      "takeRight",
      Map[StdLibVersion, Long](V1 -> 19L, V2 -> 19L, V3 -> 19L, V4 -> 20L),
      STRING,
      ("@xs", STRING), ("@number", LONG)
    ) {
      FUNCTION_CALL(
        dropString,
        List(
          REF("@xs"),
          FUNCTION_CALL(
            subLong,
            List(
              FUNCTION_CALL(sizeString, List(REF("@xs"))),
              REF("@number")
            )
          )
        )
      )
    }

  lazy val dropRightString: BaseFunction[NoContext] =
    UserFunction(
      "dropRight",
      Map[StdLibVersion, Long](V1 -> 19L, V2 -> 19L, V3 -> 19L, V4 -> 20L),
      STRING,
      ("@xs", STRING), ("@number", LONG)
    ) {
      FUNCTION_CALL(
        takeString,
        List(
          REF("@xs"),
          FUNCTION_CALL(
            subLong,
            List(
              FUNCTION_CALL(sizeString, List(REF("@xs"))),
              REF("@number")
            )
          )
        )
      )
    }

  val UTF8Decoder = UTF_8.newDecoder

  def toUtf8String(reduceLimit: Boolean): BaseFunction[NoContext] =
    NativeFunction(
      "toUtf8String",
      Map[StdLibVersion, Long](V1 -> 20L, V2 -> 20L, V3 -> 20L, V4 -> 7L),
      UTF8STRING,
      STRING,
      ("u", BYTESTR)
    ) {
      case CONST_BYTESTR(u) :: Nil =>
        Try(ByteBuffer.wrap(u.arr))
          .map(UTF8Decoder.decode)
          .toEither
          .map(_.toString)
          .flatMap(CONST_STRING(_, reduceLimit))
          .leftMap {
            case _: MalformedInputException => "Input contents invalid UTF8 sequence"
            case e                          => e.toString
          }
      case xs => notImplemented[Id, EVALUATED]("toUtf8String(u: ByteVector)", xs)
    }

  lazy val toLong: BaseFunction[NoContext] =
    NativeFunction("toInt",  Map[StdLibVersion, Long](V1 -> 10L, V2 -> 10L, V3 -> 10L, V4 -> 1L), BININT, LONG, ("bin", BYTESTR)) {
      case CONST_BYTESTR(u) :: Nil =>
        Try(CONST_LONG(ByteBuffer.wrap(u.arr).getLong())).toEither.left.map {
          case _: BufferUnderflowException => "Buffer underflow"
          case e                           => e.toString
        }
      case xs => notImplemented[Id, EVALUATED]("toInt(u: ByteVector)", xs)
    }

  lazy val toLongOffset: BaseFunction[NoContext] =
    NativeFunction("toInt", Map[StdLibVersion, Long](V1 -> 10L, V2 -> 10L, V3 -> 10L, V4 -> 1L), BININT_OFF, LONG, ("bin", BYTESTR), ("offset", LONG)) {
      case CONST_BYTESTR(ByteStr(u)) :: CONST_LONG(o) :: Nil =>
        if (o >= 0 && o <= u.size - 8) {
          Try(CONST_LONG(ByteBuffer.wrap(u).getLong(o.toInt))).toEither.left.map {
            case _: BufferUnderflowException => "Buffer underflow"
            case e                           => e.toString
          }
        } else {
          Left("IndexOutOfBounds")
        }
      case xs => notImplemented[Id, EVALUATED]("toInt(u: ByteVector, off: Int)", xs)
    }

  lazy val indexOf: BaseFunction[NoContext] =
    NativeFunction(
      "indexOf",
      Map[StdLibVersion, Long](V1 -> 20L, V2 -> 20L, V3 -> 20L, V4 -> 3L),
      INDEXOF,
      optionLong,
      ("str", STRING),
      ("substr", STRING)
    ) {
      case CONST_STRING(m) :: CONST_STRING(sub) :: Nil =>
        Right({
          val i = m.indexOf(sub)
          if (i != -1) {
            CONST_LONG(i.toLong)
          } else {
            unit
          }
        })
      case xs => notImplemented[Id, EVALUATED]("indexOf(str: String, substr: String)", xs)
    }

  lazy val indexOfN: BaseFunction[NoContext] =
    NativeFunction(
      "indexOf",
      Map[StdLibVersion, Long](V1 -> 20L, V2 -> 20L, V3 -> 20L, V4 -> 3L),
      INDEXOFN,
      optionLong,
      ("str", STRING), ("substr", STRING), ("offset", LONG)
    ) {
      case CONST_STRING(m) :: CONST_STRING(sub) :: CONST_LONG(off) :: Nil =>
        Right(if (off >= 0 && off <= m.length) {
          val i = m.indexOf(sub, off.toInt)
          if (i != -1) {
            CONST_LONG(i.toLong)
          } else {
            unit
          }
        } else {
          unit
        })
      case xs => notImplemented[Id, EVALUATED]("indexOf(str: String, substr: String, offset: Int)", xs)
    }

  lazy val lastIndexOf: BaseFunction[NoContext] =
    NativeFunction(
      "lastIndexOf",
      Map[StdLibVersion, Long](V1 -> 20L, V2 -> 20L, V3 -> 20L, V4 -> 3L),
      LASTINDEXOF,
      optionLong,
      ("str", STRING),
      ("substr", STRING)
    ) {
      case CONST_STRING(m) :: CONST_STRING(sub) :: Nil =>
        Right({
          val i = m.lastIndexOf(sub)
          if (i != -1) {
            CONST_LONG(i.toLong)
          } else {
            unit
          }
        })
      case xs => notImplemented[Id, EVALUATED]("lastIndexOf(str: String, substr: String)", xs)
    }

  lazy val lastIndexOfWithOffset: BaseFunction[NoContext] =
    NativeFunction(
      "lastIndexOf",
      Map[StdLibVersion, Long](V1 -> 20L, V2 -> 20L, V3 -> 20L, V4 -> 3L),
      LASTINDEXOFN,
      optionLong,
      ("str", STRING),
      ("substr", STRING),
      ("offset", LONG)
    ) {
      case CONST_STRING(m) :: CONST_STRING(sub) :: CONST_LONG(off) :: Nil =>
        Right(if (off >= 0) {
          val offset = Math.min(off, Int.MaxValue.toLong).toInt
          val i      = m.lastIndexOf(sub, offset)
          if (i != -1) {
            CONST_LONG(i.toLong)
          } else {
            unit
          }
        } else {
          unit
        })
      case xs => notImplemented[Id, EVALUATED]("lastIndexOf(str: String, substr: String, offset: Int)", xs)
    }

  lazy val splitStr: BaseFunction[NoContext] =
    NativeFunction("split", Map(V3 -> 100L, V4 -> 75L), SPLIT, listString, ("str", STRING), ("separator", STRING)) {
      case CONST_STRING(str) :: CONST_STRING(sep) :: Nil =>
          ARR(split(str, sep).toIndexedSeq, limited = true)
      case xs =>
        notImplemented[Id, EVALUATED]("split(str: String, separator: String)", xs)
    }

  private def split(str: String, sep: String): Iterable[CONST_STRING] = {
    if (str == "") listWithEmptyStr
    else if (sep == "")
      (1 to str.length)
        .map(i => CONST_STRING(String.valueOf(str.charAt(i - 1))).explicitGet())
    else splitRec(str, sep)
  }

  private val listWithEmptyStr = List(CONST_STRING("").explicitGet())

  @tailrec private def splitRec(
      str: String,
      sep: String,
      offset: Int = 0,
      splitted: ArrayBuffer[CONST_STRING] = ArrayBuffer()
  ): ArrayBuffer[CONST_STRING] = {
    val index = str.indexOf(sep, offset)
    if (index == -1) splitted += CONST_STRING(str.substring(offset, str.length)).explicitGet()
    else
      splitRec(
        str,
        sep,
        index + sep.length,
        splitted += CONST_STRING(str.substring(offset, index)).explicitGet()
      )
  }

  lazy val makeString: BaseFunction[NoContext] =
    NativeFunction("makeString", 30, MAKESTRING, STRING, ("list", LIST(STRING)), ("separator", STRING)) {
      case (arr: ARR) :: CONST_STRING(separator) :: Nil =>
        val separatorStringSize =
          if (arr.xs.length > 1) (arr.xs.length - 1) * separator.length
          else 0
        val expectedStringSize = arr.elementsWeightSum + separatorStringSize
        if (expectedStringSize <= DataEntryValueMax)
          CONST_STRING(arr.xs.mkString(separator))
        else
          Left(s"Constructing string size = $expectedStringSize bytes will exceed $DataEntryValueMax")
      case xs =>
        notImplemented[Id, EVALUATED]("makeString(list: List[String], separator: String)", xs)
    }

  lazy val contains: BaseFunction[NoContext] =
    UserFunction(
      "contains",
      3,
      BOOLEAN,
      ("@source", STRING), ("@substr", STRING)
    ) {
      FUNCTION_CALL(
        User("isDefined"),
        List(
          FUNCTION_CALL(
            Native(INDEXOF),
            List(REF("@source"), REF("@substr"))
          )
        )
      )
    }

  lazy val parseInt: BaseFunction[NoContext] =
    NativeFunction(
      "parseInt",
      Map[StdLibVersion, Long](V1 -> 20L, V2 -> 20L, V3 -> 20L, V4 -> 2L),
      PARSEINT,
      optionLong,
      ("str", STRING)) {
      case CONST_STRING(u) :: Nil => Try(CONST_LONG(u.toLong)).orElse(Success(unit)).toEither.left.map(_.toString)
      case xs                     => notImplemented[Id, EVALUATED]("parseInt(str: String)", xs)
    }

  lazy val parseIntVal: BaseFunction[NoContext] =
    UserFunction(
      "parseIntValue",
      Map[StdLibVersion, Long](V1 -> 20L, V2 -> 20L, V3 -> 20L, V4 -> 2L),
      LONG,
      ("str", STRING)
    ) {
      val parseO = FUNCTION_CALL(Native(PARSEINT), List(REF("str")))
      FUNCTION_CALL(
        User("valueOrErrorMessage"),
        List(parseO, CONST_STRING("Error while parsing string to integer").explicitGet())
      )
    }

  def createRawOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short, complexity: Int = 1)(
      body: (EVALUATED, EVALUATED) => Either[String, EVALUATED]
  ): BaseFunction[NoContext] =
    NativeFunction(opsToFunctions(op), complexity, func, r, ("a", t), ("b", t)) {
      case a :: b :: Nil => body(a, b)
      case xs            => notImplemented[Id, EVALUATED](s"${opsToFunctions(op)}(a: ${t.toString}, b: ${t.toString})", xs)
    }

  def createRawOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short, complexity: Map[StdLibVersion, Long])(
      body: (EVALUATED, EVALUATED) => Either[String, EVALUATED]
  ): BaseFunction[NoContext] =
    NativeFunction(opsToFunctions(op), complexity, func, r, ("a", t), ("b", t)) {
      case a :: b :: Nil => body(a, b)
      case xs            => notImplemented[Id, EVALUATED](s"${opsToFunctions(op)}(a: ${t.toString}, b: ${t.toString})", xs)
    }

  def createOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short, complexity: Int = 1)(
      body: (Long, Long) => Boolean
  ): BaseFunction[NoContext] =
    NativeFunction(opsToFunctions(op), complexity, func, r, ("a", t), ("b", t)) {
      case CONST_LONG(a) :: CONST_LONG(b) :: Nil => Right(CONST_BOOLEAN(body(a, b)))
      case xs                                    => notImplemented[Id, EVALUATED](s"${opsToFunctions(op)}(a: ${t.toString}, b: ${t.toString})", xs)
    }

  def createTryOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short, complicity: Int = 1)(
      body: (Long, Long) => Long
  ): BaseFunction[NoContext] =
    NativeFunction(opsToFunctions(op), complicity, func, r, ("a", t), ("b", t)) {
      case CONST_LONG(a) :: CONST_LONG(b) :: Nil =>
        try {
          Right(CONST_LONG(body(a, b)))
        } catch {
          case e: Throwable => Left(e.getMessage)
        }
      case xs => notImplemented[Id, EVALUATED](s"${opsToFunctions(op)}(a: ${t.toString}, b: ${t.toString})", xs)
    }

  lazy val getElement: BaseFunction[NoContext] =
    NativeFunction(
      "getElement",
      2,
      GET_LIST,
      TYPEPARAM('T'),
      ("arr", PARAMETERIZEDLIST(TYPEPARAM('T'))),
      ("pos", LONG)
    ) {
      case ARR(arr) :: CONST_LONG(pos) :: Nil =>
        Try(arr(pos.toInt)).toEither.left.map({
          case e: java.lang.IndexOutOfBoundsException => s"Index $pos out of bounds for length ${arr.size}"
          case e: Throwable                           => e.toString
        })
      case xs => notImplemented[Id, EVALUATED](s"getElement(arr: Array, pos: Int)", xs)
    }

  lazy val getListSize: BaseFunction[NoContext] =
    NativeFunction("size", 2, SIZE_LIST, LONG, ("arr", PARAMETERIZEDLIST(TYPEPARAM('T')))) {
      case ARR(arr) :: Nil => Right(CONST_LONG(arr.size.toLong))
      case xs              => notImplemented[Id, EVALUATED](s"size(arr: Array)", xs)
    }

  lazy val listMax: BaseFunction[NoContext] =
    NativeFunction("max", 3, MAX_LIST, LONG, ("list", PARAMETERIZEDLIST(LONG))) {
      case ARR(list) :: Nil =>
        Either.cond(
          list.nonEmpty,
          list.asInstanceOf[IndexedSeq[CONST_LONG]].max,
          "Can't find max for empty list"
        )
      case xs =>
        notImplemented[Id, EVALUATED]("max(list: List[Int])", xs)
    }

  lazy val listMin: BaseFunction[NoContext] =
    NativeFunction("min", 3, MIN_LIST, LONG, ("list", PARAMETERIZEDLIST(LONG))) {
      case ARR(list) :: Nil =>
        Either.cond(
          list.nonEmpty,
          list.asInstanceOf[IndexedSeq[CONST_LONG]].min,
          "Can't find min for empty list"
        )
      case xs =>
        notImplemented[Id, EVALUATED]("min(list: List[Int])", xs)
    }

  lazy val listIndexOf: BaseFunction[NoContext] =
    NativeFunction(
      "indexOf",
      5,
      INDEX_OF_LIST,
      optionLong,
      ("list", PARAMETERIZEDLIST(TYPEPARAM('T'))),
      ("element", TYPEPARAM('T'))
    ) {
      case ARR(list) :: element :: Nil =>
        genericListIndexOf(element, list.indexOf, list.indexWhere)
      case xs =>
        notImplemented[Id, EVALUATED]("indexOf(list: List[T], element: T)", xs)
    }

  lazy val listLastIndexOf: BaseFunction[NoContext] =
    NativeFunction(
      "lastIndexOf",
      5,
      LAST_INDEX_OF_LIST,
      optionLong,
      ("list", PARAMETERIZEDLIST(TYPEPARAM('T'))),
      ("element", TYPEPARAM('T'))
    ) {
      case ARR(list) :: element :: Nil =>
        genericListIndexOf(element, list.lastIndexOf(_), list.lastIndexWhere)
      case xs =>
        notImplemented[Id, EVALUATED]("lastIndexOf(list: List[T], element: T)", xs)
    }

  lazy val listRemoveByIndex: BaseFunction[NoContext] =
    NativeFunction(
      "removeByIndex",
      7,
      REMOVE_BY_INDEX_OF_LIST,
      PARAMETERIZEDLIST(TYPEPARAM('T')),
      ("list", PARAMETERIZEDLIST(TYPEPARAM('T'))),
      ("index", LONG)
    ) {
      case ARR(list) :: CONST_LONG(index) :: Nil =>
        if (list.isEmpty)
          Left("Can't remove an element from empty list")
        else if (index < 0)
          Left(s"Index of the removing element should be positive, but $index was passed")
        else if (index >= list.size)
          Left(s"Index of the removing element should be lower than list size = ${list.length}, but $index was passed")
        else
          ARR(list.take(index.toInt) ++ list.drop(index.toInt + 1), limited = true)
      case xs =>
        notImplemented[Id, EVALUATED]("removeByIndex(list: List[T], index: Int)", xs)
    }

  @VisibleForTesting
  private[v1] def genericListIndexOf(
     element: EVALUATED,
     indexOf: EVALUATED => Int,
     indexWhere: (EVALUATED => Boolean) => Int
  ): Either[String, EVALUATED] =
    if (element.weight <= MaxCmpWeight)
      Right {
        val i = indexOf(element)
        if (i != -1) CONST_LONG(i.toLong) else unit
      }
    else Try {
      indexWhere { listElement =>
        if (listElement.weight > MaxCmpWeight)
          throw new RuntimeException(
            s"Both element to search for `$element` " +
            s"and list element `$listElement` " +
            s"are too heavy to compare"
          )
        listElement == element
      }
    }.toEither
      .bimap(
        _.getMessage,
        index => if (index != -1) CONST_LONG(index.toLong) else unit
      )

  lazy val listContains: BaseFunction[NoContext] =
    UserFunction(
      "containsElement",
      5,
      BOOLEAN,
      ("@list", PARAMETERIZEDLIST(TYPEPARAM('T'))),
      ("@element", TYPEPARAM('T'))
    ) {
      val index = FUNCTION_CALL(Native(INDEX_OF_LIST), List(REF("@list"), REF("@element")))
      FUNCTION_CALL(User("!="), List(index, unit))
    }

  def createTupleN(resultSize: Int): NativeFunction[NoContext] = {
    val typeParams =
      ('A'.toInt until 'A'.toInt + resultSize).map(t => TYPEPARAM(t.toByte)).toList

    NativeFunction(
      s"_Tuple$resultSize",
      1,
      (CREATE_TUPLE + resultSize - 2).toShort,
      PARAMETERIZEDTUPLE(typeParams),
      typeParams.mapWithIndex { case (typeParam, i) => (s"element${i + 1}", typeParam) }: _*
    ) {
      case elements if elements.length == resultSize =>
        val fields = elements.mapWithIndex { case (element, i) => (s"_${i + 1}", element) }.toMap
        Right(CaseObj(runtimeTupleType, fields))
      case xs =>
        notImplemented[Id, EVALUATED](typeParams.map(_.char).mkString("(", ", ", ")"), xs)
    }
  }

  lazy val uMinus: BaseFunction[NoContext] =
    UserFunction("-", Map[StdLibVersion, Long](V1 -> 9, V2 -> 9, V3 -> 1, V4 -> 1), LONG, ("@n", LONG)) {
      FUNCTION_CALL(subLong, List(CONST_LONG(0), REF("@n")))
    }

  lazy val uNot: BaseFunction[NoContext] =
    UserFunction("!", Map[StdLibVersion, Long](V1 -> 11, V2 -> 11, V3 -> 1, V4 -> 1), BOOLEAN, ("@p", BOOLEAN)) {
      IF(REF("@p"), FALSE, TRUE)
    }

  val roundCeiling  = CASETYPEREF("Ceiling", List.empty, true)
  val roundFloor    = CASETYPEREF("Floor", List.empty, true)
  val roundHalfEven = CASETYPEREF("HalfEven", List.empty, true)
  val roundDown     = CASETYPEREF("Down", List.empty, true)
  val roundUp       = CASETYPEREF("Up", List.empty, true)
  val roundHalfUp   = CASETYPEREF("HalfUp", List.empty, true)
  val roundHalfDown = CASETYPEREF("HalfDown", List.empty, true)
  val rounds        = UNION(roundDown, roundUp, roundHalfUp, roundHalfDown, roundCeiling, roundFloor, roundHalfEven)

  private def roundMode(m: EVALUATED): BaseGlobal.Rounds = {
    m match {
      case (p: CaseObj) =>
        p.caseType.name match {
          case "Down"     => BaseGlobal.RoundDown()
          case "Up"       => BaseGlobal.RoundUp()
          case "HalfUp"   => BaseGlobal.RoundHalfUp()
          case "HalfDown" => BaseGlobal.RoundHalfDown()
          case "HalfEven" => BaseGlobal.RoundHalfEven()
          case "Ceiling"  => BaseGlobal.RoundCeiling()
          case "Floor"    => BaseGlobal.RoundFloor()
          case v          => throw new Exception(s"Type error: $v isn't in $rounds")
        }
      case v => throw new Exception(s"Type error: $v isn't rounds CaseObj")
    }
  }

  val pow: BaseFunction[NoContext] =
    NativeFunction("pow", 100, POW, LONG, ("base", LONG), ("bp", LONG), ("exponent", LONG), ("ep", LONG), ("rp", LONG), ("round", rounds)) {
      case CONST_LONG(b) :: CONST_LONG(bp) :: CONST_LONG(e) :: CONST_LONG(ep) :: CONST_LONG(rp) :: round :: Nil =>
        if (bp < 0
          || bp > 8
          || ep < 0
          || ep > 8
          || rp < 0
          || rp > 8) {
          Left("pow: scale out of range 0-8")
        } else {
          global.pow(b, bp, e, ep, rp, roundMode(round)).map(CONST_LONG)
        }
      case xs => notImplemented[Id, EVALUATED]("pow(base: Int, bp: Int, exponent: Int, ep: Int, rp: Int, round: Rounds)", xs)
    }

  val log: BaseFunction[NoContext] =
    NativeFunction("log", 100, LOG, LONG, ("base", LONG), ("bp", LONG), ("exponent", LONG), ("ep", LONG), ("rp", LONG), ("round", rounds)) {
      case CONST_LONG(b) :: CONST_LONG(bp) :: CONST_LONG(e) :: CONST_LONG(ep) :: CONST_LONG(rp) :: round :: Nil =>
        if (bp < 0
          || bp > 8
          || ep < 0
          || ep > 8
          || rp < 0
          || rp > 8) {
          Left("log: scale out of range 0-8")
        } else {
          global.log(b, bp, e, ep, rp, roundMode(round)).map(CONST_LONG)
        }
      case xs => notImplemented[Id, EVALUATED]("log(exponent: Int, ep: Int, base: Int, bp: Int, rp: Int, round: Rounds)", xs)
    }

  val getListMedian: BaseFunction[NoContext] =
    NativeFunction("median", 20, MEDIAN_LIST, LONG, ("arr", PARAMETERIZEDLIST(LONG))) {
      case xs @ (ARR(arr) :: Nil) =>
        if (arr.headOption.forall(_.isInstanceOf[CONST_LONG])) {
          if (arr.nonEmpty)
            Right(CONST_LONG(global.median(arr.asInstanceOf[IndexedSeq[CONST_LONG]].map(_.t))))
          else
          Left(s"Can't find median for empty list")
        } else {
          notImplemented[Id, EVALUATED](s"median(arr: List[Int])", xs)
        }
      case xs => notImplemented[Id, EVALUATED](s"median(arr: List[Int])", xs)
    }

  val unitVarName = "unit"

  private def singleObj(
      ty: CASETYPEREF,
      v: Map[String, EVALUATED] = Map.empty
  ): (CASETYPEREF, ContextfulVal[NoContext]) =
    ty -> ContextfulVal.pure(CaseObj(ty, v))

  private val nil: (String, (LIST, ContextfulVal[NoContext])) =
    ("nil", (LIST(NOTHING), ContextfulVal.pure[NoContext](ARR(IndexedSeq.empty[EVALUATED], EMPTYARR_WEIGHT, limited = false).explicitGet())))

  private val commonVars: Map[String, (FINAL, ContextfulVal[NoContext])] =
    Map(
      (unitVarName, (UNIT, ContextfulVal.pure(unit))),
      ("UP", singleObj(roundUp)),
      ("HALFUP", singleObj(roundHalfUp)),
      ("HALFDOWN", singleObj(roundHalfDown)),
      ("DOWN", singleObj(roundDown)),
      ("HALFEVEN", singleObj(roundHalfEven)),
      ("CEILING", singleObj(roundCeiling)),
      ("FLOOR", singleObj(roundFloor))
    )

  private val commonTypes =
    Seq(
      UNIT,
      LONG,
      BOOLEAN,
      BYTESTR,
      STRING,
      roundDown,
      roundUp,
      roundHalfUp,
      roundHalfDown,
      roundHalfEven,
      roundCeiling,
      roundFloor,
      rounds
    )

  private val v3V4Vars = commonVars + nil

  private val operators: Array[BaseFunction[NoContext]] =
    Array(
      mulLong,
      divLong,
      modLong,
      sumLong,
      subLong,
      sumString,
      sumByteStr,
      eq,
      ne,
      ge,
      gt,
      getElement,
      getListSize,
      uMinus,
      uNot
    )

  private val commonFunctions =
    Array(
      fraction,
      sizeBytes,
      toBytesBoolean,
      toBytesLong,
      toBytesString,
      takeBytes,
      dropBytes,
      takeRightBytes,
      dropRightBytes,
      sizeString,
      toStringBoolean,
      toStringLong,
      takeString,
      dropString,
      takeRightString,
      dropRightString,
      isDefined,
      throwWithMessage,
      _isInstanceOf,
      throwNoMessage
    ) ++ operators

  private val v1V2V3CommonFunctions =
    commonFunctions :+ extract

  private val v3V4CommonFunctions =
    Array(
      value,
      valueOrErrorMessage,
      toLong,
      toLongOffset,
      indexOf,
      indexOfN,
      lastIndexOf,
      lastIndexOfWithOffset,
      splitStr,
      parseInt,
      parseIntVal,
      pow,
      log
    )

  private val v3Functions =
    v1V2V3CommonFunctions ++
    v3V4CommonFunctions ++
      Array(
        toUtf8String(reduceLimit = false),
        listConstructor(checkSize = false)
      )

  private val v4Functions =
    commonFunctions ++
    v3V4CommonFunctions ++
      Array(
        contains,
        valueOrElse,
        listAppend,
        listConcat,
        toUtf8String(reduceLimit = true),
        listConstructor(checkSize = true),
        getListMedian,
        listIndexOf,
        listLastIndexOf,
        listRemoveByIndex,
          listContains,
          listMin,
          listMax,
          makeString,
        ) ++ (MinTupleSize to MaxTupleSize).map(i => createTupleN(i))

  private val v1V2Ctx =
    CTX[NoContext](
      commonTypes,
      commonVars,
      v1V2V3CommonFunctions
    )

  private val v3Ctx =
    CTX[NoContext](
      commonTypes,
      v3V4Vars,
      v3Functions
    )

  private val v4Ctx =
    CTX[NoContext](
      commonTypes,
      v3V4Vars,
      v4Functions
    )

  def build(version: StdLibVersion): CTX[NoContext] =
    version match {
      case V1 | V2 => v1V2Ctx
      case V3      => v3Ctx
      case V4 | V5 => v4Ctx
    }
}
