package com.wavesplatform.lang.v1.evaluator.ctx.impl

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.charset.{MalformedInputException, StandardCharsets}
import java.nio.{BufferUnderflowException, ByteBuffer}

import cats.Id
import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.ContractLimits._
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
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
import scala.util.{Success, Try}

object PureContext {

  implicit def intToLong(num: Int): Long = num.toLong

  private lazy val defaultThrowMessage = "Explicit script termination"
  lazy val MaxStringResult             = Short.MaxValue
  lazy val MaxBytesResult              = 65536
  lazy val MaxListLengthV4             = 1000
  lazy val MaxListSizeForMedianCalc    = 100

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
    createRawOp(SUM_OP, STRING, STRING, SUM_STRING, 10) {
      case (CONST_STRING(a), CONST_STRING(b)) =>
        if(a.length + b.length <= MaxStringResult) {
          CONST_STRING(a + b)
        } else {
          Left("String is too large")
        }
      case _ => ???
    }
  lazy val sumByteStr: BaseFunction[NoContext] =
    createRawOp(SUM_OP, BYTESTR, BYTESTR, SUM_BYTES, 10) {
      case (CONST_BYTESTR(a), CONST_BYTESTR(b)) =>
        if(a.arr.length + b.arr.length <= MaxStringResult) {
          CONST_BYTESTR(a ++ b)
        } else {
          Left("ByteVector is too large")
        }
      case _ => ???
    }
  lazy val ge: BaseFunction[NoContext] = createOp(GE_OP, LONG, BOOLEAN, GE_LONG)(_ >= _)
  lazy val gt: BaseFunction[NoContext] =
    createOp(GT_OP, LONG, BOOLEAN, GT_LONG)(_ > _)

  lazy val eq: BaseFunction[NoContext] =
    NativeFunction(EQ_OP.func, 1, EQ, BOOLEAN, ("a", TYPEPARAM('T')), ("b", TYPEPARAM('T'))) {
      case a :: b :: Nil =>
        Either.cond(b.weight <= MaxCmpWeight || a.weight <= MaxCmpWeight, CONST_BOOLEAN(a == b), "Comparable value too heavy.")
      case xs            => notImplemented[Id, EVALUATED](s"${EQ_OP.func}(a: T, b: T)", xs)
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
      13L,
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
      13L,
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
      13,
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
      case CONST_BOOLEAN(_) :: CONST_STRING("Boolean") :: Nil    => Right(TRUE)
      case CONST_BYTESTR(_) :: CONST_STRING("ByteVector") :: Nil => Right(TRUE)
      case CONST_STRING(_) :: CONST_STRING("String") :: Nil      => Right(TRUE)
      case CONST_LONG(_) :: CONST_STRING("Int") :: Nil           => Right(TRUE)
      case (p: CaseObj) :: CONST_STRING(s) :: Nil                => Right(CONST_BOOLEAN(p.caseType.name == s))
      case _                                                     => Right(FALSE)
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
    NativeFunction("toBytes", 1, STRING_TO_BYTES, BYTESTR, ("s", STRING)) {
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
    NativeFunction("take", 1, TAKE_BYTES, BYTESTR, ("xs", BYTESTR), ("number", LONG)) {
      case CONST_BYTESTR(xs) :: CONST_LONG(number) :: Nil => CONST_BYTESTR(xs.take(number))
      case xs                                             => notImplemented[Id, EVALUATED]("take(xs: ByteVector, number: Int)", xs)
    }

  lazy val dropBytes: BaseFunction[NoContext] =
    NativeFunction("drop", 1, DROP_BYTES, BYTESTR, ("xs", BYTESTR), ("number", LONG)) {
      case CONST_BYTESTR(xs) :: CONST_LONG(number) :: Nil => CONST_BYTESTR(xs.drop(number))
      case xs                                             => notImplemented[Id, EVALUATED]("drop(xs: ByteVector, number: Int)", xs)
    }

  lazy val dropRightBytes: BaseFunction[NoContext] =
    UserFunction("dropRight", "dropRightBytes", 19, BYTESTR, ("@xs", BYTESTR), ("@number", LONG)) {
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
    UserFunction("takeRight", "takeRightBytes", 19, BYTESTR, ("@xs", BYTESTR), ("@number", LONG)) {
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
    NativeFunction("take", 1, TAKE_STRING, STRING, ("xs", STRING), ("number", LONG)) {
      case CONST_STRING(xs) :: CONST_LONG(number) :: Nil => CONST_STRING(xs.take(trimLongToInt(number)))
      case xs                                            => notImplemented[Id, EVALUATED]("take(xs: String, number: Int)", xs)
    }

  def listConstructor(checkSize: Boolean): NativeFunction[NoContext] =
    NativeFunction(
      "cons",
      2,
      CREATE_LIST,
      PARAMETERIZEDLIST(PARAMETERIZEDUNION(List(TYPEPARAM('A'), TYPEPARAM('B')))),
      ("head", TYPEPARAM('A')),
      ("tail", PARAMETERIZEDLIST(TYPEPARAM('B')))
    ) {
      case h :: (a @ ARR(t)) :: Nil => ARR(h +: t, h.weight + a.weight + ELEM_WEIGHT, checkSize)
      case xs                 => notImplemented[Id, EVALUATED]("cons(head: T, tail: LIST[T]", xs)
    }

  lazy val listAppend: NativeFunction[NoContext] =
    NativeFunction(
      LIST_APPEND_OP.func,
      3,
      APPEND_LIST,
      PARAMETERIZEDLIST(PARAMETERIZEDUNION(List(TYPEPARAM('A'), TYPEPARAM('B')))),
      ("list", PARAMETERIZEDLIST(TYPEPARAM('A'))),
      ("element", TYPEPARAM('B'))
    ) {
      case (a @ ARR(list)) :: element :: Nil => ARR(list :+ element, a.weight + element.weight + ELEM_WEIGHT, true)
      case xs                          => notImplemented[Id, EVALUATED](s"list: List[T] ${LIST_APPEND_OP.func} value: T", xs)
    }

  lazy val listConcat: NativeFunction[NoContext] =
    NativeFunction(
      LIST_CONCAT_OP.func,
      10,
      CONCAT_LIST,
      PARAMETERIZEDLIST(PARAMETERIZEDUNION(List(TYPEPARAM('A'), TYPEPARAM('B')))),
      ("list1", PARAMETERIZEDLIST(TYPEPARAM('A'))),
      ("list2", PARAMETERIZEDLIST(TYPEPARAM('B')))
    ) {
      case (a1 @ ARR(l1)) :: (a2 @ ARR(l2)) :: Nil => ARR(l1 ++ l2, a1.weight + a2.weight - EMPTYARR_WEIGHT, true)
      case xs                        => notImplemented[Id, EVALUATED](s"list1: List[T] ${LIST_CONCAT_OP.func} list2: List[T]", xs)
    }

  lazy val dropString: BaseFunction[NoContext] =
    NativeFunction("drop", 1, DROP_STRING, STRING, ("xs", STRING), ("number", LONG)) {
      case CONST_STRING(xs) :: CONST_LONG(number) :: Nil => CONST_STRING(xs.drop(trimLongToInt(number)))
      case xs                                            => notImplemented[Id, EVALUATED]("drop(xs: String, number: Int)", xs)
    }

  lazy val takeRightString: BaseFunction[NoContext] =
    UserFunction("takeRight", 19, STRING, ("@xs", STRING), ("@number", LONG)) {
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
    UserFunction("dropRight", 19, STRING, ("@xs", STRING), ("@number", LONG)) {
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

  lazy val toUtf8String: BaseFunction[NoContext] =
    NativeFunction("toUtf8String", 20, UTF8STRING, STRING, ("u", BYTESTR)) {
      case CONST_BYTESTR(u) :: Nil =>
        Try(ByteBuffer.wrap(u.arr))
          .map(UTF8Decoder.decode)
          .toEither
          .map(_.toString)
          .flatMap(CONST_STRING(_))
          .leftMap {
            case _: MalformedInputException => "Input contents invalid UTF8 sequence"
            case e                          => e.toString
          }
      case xs => notImplemented[Id, EVALUATED]("toUtf8String(u: ByteVector)", xs)
    }

  lazy val toLong: BaseFunction[NoContext] =
    NativeFunction("toInt", 10, BININT, LONG, ("bin", BYTESTR)) {
      case CONST_BYTESTR(u) :: Nil =>
        Try(CONST_LONG(ByteBuffer.wrap(u.arr).getLong())).toEither.left.map {
          case _: BufferUnderflowException => "Buffer underflow"
          case e                           => e.toString
        }
      case xs => notImplemented[Id, EVALUATED]("toInt(u: ByteVector)", xs)
    }

  lazy val toLongOffset: BaseFunction[NoContext] =
    NativeFunction("toInt", 10, BININT_OFF, LONG, ("bin", BYTESTR), ("offset", LONG)) {
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
    NativeFunction("indexOf", 20, INDEXOF, optionLong, ("str", STRING), ("substr", STRING)) {
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
    NativeFunction("indexOf", 20, INDEXOFN, optionLong, ("str", STRING), ("substr", STRING), ("offset", LONG)) {
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
      20,
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
      20,
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
    NativeFunction("split", 100, SPLIT, listString, ("str", STRING), ("separator", STRING)) {
      case CONST_STRING(str) :: CONST_STRING(sep) :: Nil =>
        split(str, sep)
          .traverse(CONST_STRING(_))
          .flatMap(s => ARR(s.toIndexedSeq, true))
      case xs => notImplemented[Id, EVALUATED]("split(str: String, separator: String)", xs)
    }

  private def split(str: String, sep: String) =
    if (str == "") listWithEmptyStr
    else if (sep == "") 1 to str.length map (i => String.valueOf(str.charAt(i - 1))) toList
    else splitRec(str, sep).reverse

  private val listWithEmptyStr = List("")

  @tailrec private def splitRec(
      str: String,
      sep: String,
      offset: Int = 0,
      splitted: List[String] = Nil
  ): List[String] = {
    val index = str.indexOf(sep, offset)
    if (index == -1) str.substring(offset, str.length) :: splitted
    else
      splitRec(
        str,
        sep,
        index + sep.length,
        str.substring(offset, index) :: splitted
      )
  }

  lazy val contains: BaseFunction[NoContext] =
    UserFunction("contains", 20, BOOLEAN, ("@source", STRING), ("@substr", STRING)) {
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
    NativeFunction("parseInt", 20, PARSEINT, optionLong, ("str", STRING)) {
      case CONST_STRING(u) :: Nil => Try(CONST_LONG(u.toLong)).orElse(Success(unit)).toEither.left.map(_.toString)
      case xs                     => notImplemented[Id, EVALUATED]("parseInt(str: String)", xs)
    }

  lazy val parseIntVal: BaseFunction[NoContext] =
    UserFunction(
      "parseIntValue",
      20,
      LONG,
      ("str", STRING)
    ) {
      val parseO = FUNCTION_CALL(Native(PARSEINT), List(REF("str")))
      FUNCTION_CALL(
        User("valueOrErrorMessage"),
        List(parseO, CONST_STRING("Error while parsing string to integer").explicitGet())
      )
    }

  def createRawOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short, complicity: Int = 1)(
      body: (EVALUATED, EVALUATED) => Either[String, EVALUATED]
  ): BaseFunction[NoContext] =
    NativeFunction(opsToFunctions(op), complicity, func, r, ("a", t), ("b", t)) {
      case a :: b :: Nil => body(a, b)
      case xs            => notImplemented[Id, EVALUATED](s"${opsToFunctions(op)}(a: ${t.toString}, b: ${t.toString})", xs)
    }

  def createOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short, complicity: Int = 1)(
      body: (Long, Long) => Boolean
  ): BaseFunction[NoContext] =
    NativeFunction(opsToFunctions(op), complicity, func, r, ("a", t), ("b", t)) {
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

  lazy val getListMedian: BaseFunction[NoContext] =
    NativeFunction("median", 10, MEDIAN_LIST, LONG, ("arr", PARAMETERIZEDLIST(LONG))) {
      case xs @ (ARR(arr) :: Nil) => {

        def getMedian(seq: Seq[Long]): Long = {
          val targetArr = seq.toArray
          scala.util.Sorting.quickSort(targetArr)
          val size     = targetArr.size
          val halfSize = size / 2
          if (size % 2 == 1) {
            targetArr(halfSize)
          } else {
            Math.floorDiv(targetArr(halfSize - 1) + targetArr(halfSize), 2)
          }
        }

        if (arr.headOption.map(_.isInstanceOf[CONST_LONG]).getOrElse(true)) {
          if (arr.size == 1) {
            Right(arr.head)
          } else if (arr.size > 1 && arr.size <= MaxListSizeForMedianCalc) {
            Right(CONST_LONG(getMedian(arr.asInstanceOf[IndexedSeq[CONST_LONG]].map(_.t))))
          } else {
            Left(s"Invalid list size. Size should be between 1 and $MaxListSizeForMedianCalc")
          }
        } else {
          notImplemented[Id, EVALUATED](s"median(arr: List[Int])", xs)
        }
      }
      case xs => notImplemented[Id, EVALUATED](s"median(arr: List[Int])", xs)
    }

  lazy val uMinus: BaseFunction[NoContext] =
    UserFunction("-", Map[StdLibVersion, Long](V1 -> 9, V2 -> 9, V3 -> 1, V4 -> 1), LONG, ("@n", LONG)) {
      FUNCTION_CALL(subLong, List(CONST_LONG(0), REF("@n")))
    }

  lazy val uNot: BaseFunction[NoContext] =
    UserFunction("!", Map[StdLibVersion, Long](V1 -> 11, V2 -> 11, V3 -> 1, V4 -> 1), BOOLEAN, ("@p", BOOLEAN)) {
      IF(REF("@p"), FALSE, TRUE)
    }

  private lazy val operators: Array[BaseFunction[NoContext]] = Array(
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
  private lazy val functions = Array(
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
    _isInstanceOf,
    isDefined,
    extract,
    throwWithMessage,
    throwNoMessage
  ) ++ operators

  val roundCeiling  = CASETYPEREF("Ceiling", List.empty, true)
  val roundFloor    = CASETYPEREF("Floor", List.empty, true)
  val roundHalfEven = CASETYPEREF("HalfEven", List.empty, true)
  val roundDown     = CASETYPEREF("Down", List.empty, true)
  val roundUp       = CASETYPEREF("Up", List.empty, true)
  val roundHalfUp   = CASETYPEREF("HalfUp", List.empty, true)
  val roundHalfDown = CASETYPEREF("HalfDown", List.empty, true)
  val rounds        = UNION(roundDown, roundUp, roundHalfUp, roundHalfDown, roundCeiling, roundFloor, roundHalfEven)

  def roundMode(m: EVALUATED): BaseGlobal.Rounds = {
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

  lazy val unitVarName = "unit"

  private def singleObj(
      ty: CASETYPEREF,
      v: Map[String, EVALUATED] = Map.empty
  ): (CASETYPEREF, ContextfulVal[NoContext]) =
    ty -> ContextfulVal.pure(CaseObj(ty, v))

  private lazy val vars: Map[String, (FINAL, ContextfulVal[NoContext])] =
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

  private lazy val ctx: CTX[NoContext] = CTX[NoContext](
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
    ),
    vars,
    functions
  )

  def build(math: BaseGlobal, version: StdLibVersion): CTX[NoContext] = {
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
            math.pow(b, bp, e, ep, rp, roundMode(round)).right.map(CONST_LONG)
          }
        case xs => notImplemented[Id, EVALUATED]("pow(base: Int, bp: Int, exponent: Int, ep: Int, rp: Int, round: Rounds)", xs)
      }

    val log: BaseFunction[NoContext] =
      NativeFunction("log", 100, LOG, LONG, ("exponent", LONG), ("ep", LONG), ("base", LONG), ("bp", LONG), ("rp", LONG), ("round", rounds)) {
        case CONST_LONG(b) :: CONST_LONG(bp) :: CONST_LONG(e) :: CONST_LONG(ep) :: CONST_LONG(rp) :: round :: Nil =>
          if (bp < 0
              || bp > 8
              || ep < 0
              || ep > 8
              || rp < 0
              || rp > 8) {
            Left("log: scale out of range 0-8")
          } else {
            math.log(b, bp, e, ep, rp, roundMode(round)).right.map(CONST_LONG)
          }
        case xs => notImplemented[Id, EVALUATED]("log(exponent: Int, ep: Int, base: Int, bp: Int, rp: Int, round: Rounds)", xs)
      }

    val fromV3Funcs = Array(
      value,
      valueOrErrorMessage,
      toUtf8String,
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

    val v3Ctx = Monoid.combine(
      ctx,
      CTX[NoContext](
        Seq.empty,
        Map(("nil", (LIST(NOTHING), ContextfulVal.pure[NoContext](ARR(IndexedSeq.empty[EVALUATED], EMPTYARR_WEIGHT, false).explicitGet)))),
        fromV3Funcs :+ listConstructor(checkSize = false)
      )
    )

    val v4Functions =
      ctx.functions ++
        fromV3Funcs ++
        Array(contains, valueOrElse, listAppend, listConcat, listConstructor(checkSize = true), getListMedian)

    version match {
      case V1 | V2 => ctx
      case V3      => v3Ctx
      case V4      => v3Ctx.copy(functions = v4Functions)
    }
  }
}
