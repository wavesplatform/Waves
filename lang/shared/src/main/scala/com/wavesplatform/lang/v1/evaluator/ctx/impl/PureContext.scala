package com.wavesplatform.lang.v1.evaluator.ctx.impl

import java.nio.charset.StandardCharsets

import cats.data.EitherT
import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.StdLibVersion
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.parser.BinaryOperation
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import scala.collection.mutable.ArrayBuffer
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.ByteBuffer

import scala.util.{Try, Success}

object PureContext {

  import StdLibVersion._

  implicit def intToLong(num: Int): Long = num.toLong

  private lazy val defaultThrowMessage = "Explicit script termination"
  lazy val MaxStringResult             = Short.MaxValue
  lazy val MaxBytesResult              = 65536

  lazy val mulLong: BaseFunction =
    createTryOp(MUL_OP, LONG, LONG, MUL_LONG, "Integer multiplication", "multiplyer", "multiplyer")((a, b) => Math.multiplyExact(a, b))
  lazy val divLong: BaseFunction =
    createTryOp(DIV_OP, LONG, LONG, DIV_LONG, "Integer devision", "divisible", "divisor")((a, b) => Math.floorDiv(a, b))
  lazy val modLong: BaseFunction =
    createTryOp(MOD_OP, LONG, LONG, MOD_LONG, "Modulo", "divisible", "divisor")((a, b) => Math.floorMod(a, b))
  lazy val sumLong: BaseFunction =
    createTryOp(SUM_OP, LONG, LONG, SUM_LONG, "Integer sum", "term", "term")((a, b) => Math.addExact(a, b))
  lazy val subLong: BaseFunction =
    createTryOp(SUB_OP, LONG, LONG, SUB_LONG, "Integer substitution", "term", "term")((a, b) => Math.subtractExact(a, b))
  lazy val sumString: BaseFunction =
    createRawOp(SUM_OP, STRING, STRING, SUM_STRING, "Limited strings concatination", "prefix", "suffix", 10) {
      case (CONST_STRING(a), CONST_STRING(b)) =>
        lazy val al = a.length
        lazy val bl = b.length
        Either.cond(al + bl <= MaxStringResult, CONST_STRING(a + b), "String is too large")
      case _ => ???
    }
  lazy val sumByteStr: BaseFunction =
    createRawOp(SUM_OP, BYTESTR, BYTESTR, SUM_BYTES, "Limited bytes vectors concatination", "prefix", "suffix", 10) {
      case (CONST_BYTESTR(a), CONST_BYTESTR(b)) =>
        lazy val al = a.arr.length
        lazy val bl = b.arr.length
        Either.cond(al + bl <= MaxBytesResult, CONST_BYTESTR(a ++ b), "ByteStr is too large")
      case _ => ???
    }
  lazy val ge: BaseFunction = createOp(GE_OP, LONG, BOOLEAN, GE_LONG, "Integer grater or equal comparation", "term", "term")(_ >= _)
  lazy val gt: BaseFunction =
    createOp(GT_OP, LONG, BOOLEAN, GT_LONG, "Integer grater comparation", "term", "term")(_ > _)

  lazy val eq: BaseFunction =
    NativeFunction(EQ_OP.func, 1, EQ, BOOLEAN, "Equality", ("a", TYPEPARAM('T'), "value"), ("b", TYPEPARAM('T'), "value")) {
      case a :: b :: Nil => Right(CONST_BOOLEAN(a == b))
      case _             => ???
    }

  lazy val ne: BaseFunction =
    UserFunction(NE_OP.func,
                 Map[StdLibVersion, Long](V1 -> 26, V2 -> 26, V3 -> 1),
                 BOOLEAN,
                 "Inequality",
                 ("@a", TYPEPARAM('T'), "value"),
                 ("@b", TYPEPARAM('T'), "value")) {
      FUNCTION_CALL(uNot, List(FUNCTION_CALL(eq, List(REF("@a"), REF("@b")))))
    }

  lazy val throwWithMessage: BaseFunction = NativeFunction("throw", 1, THROW, NOTHING, "Fail script", ("err", STRING, "Error message")) {
    case (err @ CONST_STRING(s)) :: Nil => Left(s)
    case _                              => Left(defaultThrowMessage)
  }

  lazy val throwNoMessage: BaseFunction = UserFunction("throw", Map[StdLibVersion, Long](V1 -> 2, V2 -> 2, V3 -> 1), NOTHING, "Fail script") {
    FUNCTION_CALL(throwWithMessage, List(CONST_STRING(defaultThrowMessage)))
  }

  lazy val extract: BaseFunction =
    UserFunction.deprecated("extract",
                 13,
                 TYPEPARAM('T'),
                 "Extract value from option or fail",
                 ("@a", PARAMETERIZEDUNION(List(TYPEPARAM('T'), UNIT)), "Optional value")) {
      IF(
        FUNCTION_CALL(eq, List(REF("@a"), REF("unit"))),
        FUNCTION_CALL(throwWithMessage, List(CONST_STRING("extract() called on unit value"))),
        REF("@a")
      )
    }

  lazy val value: BaseFunction =
    UserFunction("value",
      13,
      TYPEPARAM('T'),
      "Extract value from option or fail",
      ("@a", PARAMETERIZEDUNION(List(TYPEPARAM('T'), UNIT)), "Optional value")) {
      IF(
        FUNCTION_CALL(eq, List(REF("@a"), REF("unit"))),
        FUNCTION_CALL(throwWithMessage, List(CONST_STRING("value() called on unit value"))),
        REF("@a")
      )
    }

  lazy val isDefined: BaseFunction =
    UserFunction(
      "isDefined",
      Map[StdLibVersion, Long](V1 -> 35, V2 -> 35, V3 -> 1),
      BOOLEAN,
      "Check the value is defined",
      ("@a", PARAMETERIZEDUNION(List(TYPEPARAM('T'), UNIT)), "Option value")
    ) {
      FUNCTION_CALL(ne, List(REF("@a"), REF("unit")))
    }

  lazy val fraction: BaseFunction = NativeFunction(
    "fraction",
    1,
    FRACTION,
    LONG,
    "Multiply and dividion with big integer intermediate representation",
    ("value", LONG, "multiplyer"),
    ("numerator", LONG, "multiplyer"),
    ("denominator", LONG, "divisor")
  ) {
    case CONST_LONG(v) :: CONST_LONG(n) :: CONST_LONG(d) :: Nil =>
      lazy val result = BigInt(v) * n / d
      for {
        _ <- Either.cond(result < Long.MaxValue, (), s"Long overflow: value `$result` greater than 2^63-1")
        _ <- Either.cond(result > Long.MinValue, (), s"Long overflow: value `$result` less than -2^63-1")
      } yield CONST_LONG(result.toLong)
    case _ => ???
  }

  lazy val _isInstanceOf: BaseFunction = NativeFunction("_isInstanceOf",
                                                        1,
                                                        ISINSTANCEOF,
                                                        BOOLEAN,
                                                        "Internal function to check value type",
                                                        ("obj", TYPEPARAM('T'), "value"),
                                                        ("of", STRING, "type name")) {
    case CONST_BOOLEAN(_) :: CONST_STRING("Boolean") :: Nil    => Right(TRUE)
    case CONST_BYTESTR(_) :: CONST_STRING("ByteVector") :: Nil => Right(TRUE)
    case CONST_STRING(_) :: CONST_STRING("String") :: Nil      => Right(TRUE)
    case CONST_LONG(_) :: CONST_STRING("Int") :: Nil           => Right(TRUE)
    case (p: CaseObj) :: CONST_STRING(s) :: Nil                => Right(CONST_BOOLEAN(p.caseType.name == s))
    case _                                                     => Right(FALSE)
  }

  lazy val sizeBytes: BaseFunction = NativeFunction("size", 1, SIZE_BYTES, LONG, "Size of bytes str", ("byteVector", BYTESTR, "vector")) {
    case CONST_BYTESTR(bv) :: Nil => Right(CONST_LONG(bv.arr.length))
    case xs                       => notImplemented("size(byte[])", xs)
  }

  lazy val toBytesBoolean: BaseFunction =
    NativeFunction("toBytes", 1, BOOLEAN_TO_BYTES, BYTESTR, "Bytes array representation", ("b", BOOLEAN, "value")) {
      case TRUE :: Nil  => Right(CONST_BYTESTR(ByteStr.fromBytes(1)))
      case FALSE :: Nil => Right(CONST_BYTESTR(ByteStr.fromBytes(0)))
      case _            => ???
    }

  lazy val toBytesLong: BaseFunction = NativeFunction("toBytes", 1, LONG_TO_BYTES, BYTESTR, "Bytes array representation", ("n", LONG, "value")) {
    case CONST_LONG(n) :: Nil => Right(CONST_BYTESTR(ByteStr.fromLong(n)))
    case _                    => ???
  }

  lazy val toBytesString: BaseFunction =
    NativeFunction("toBytes", 1, STRING_TO_BYTES, BYTESTR, "Bytes array representation", ("s", STRING, "value")) {
      case CONST_STRING(s) :: Nil => Right(CONST_BYTESTR(ByteStr(s.getBytes(StandardCharsets.UTF_8))))
      case _                      => ???
    }

  lazy val sizeString: BaseFunction = NativeFunction("size", 1, SIZE_STRING, LONG, "Scting size in characters", ("xs", STRING, "string")) {
    case CONST_STRING(bv) :: Nil => Right(CONST_LONG(bv.length.toLong))
    case xs                      => notImplemented("size(String)", xs)
  }

  lazy val toStringBoolean: BaseFunction =
    NativeFunction("toString", 1, BOOLEAN_TO_STRING, STRING, "String representation", ("b", BOOLEAN, "value")) {
      case TRUE :: Nil  => Right(CONST_STRING("true"))
      case FALSE :: Nil => Right(CONST_STRING("false"))
      case _            => ???
    }

  lazy val toStringLong: BaseFunction = NativeFunction("toString", 1, LONG_TO_STRING, STRING, "String representation", ("n", LONG, "value")) {
    case CONST_LONG(n) :: Nil => Right(CONST_STRING(n.toString))
    case _                    => ???
  }

  lazy val takeBytes: BaseFunction =
    NativeFunction("take", 1, TAKE_BYTES, BYTESTR, "Take firsts bytes subvector", ("xs", BYTESTR, "vector"), ("number", LONG, "Bytes number")) {
      case CONST_BYTESTR(xs) :: CONST_LONG(number) :: Nil => Right(CONST_BYTESTR(xs.take(number)))
      case xs                                             => notImplemented("take(xs: byte[], number: Long)", xs)
    }

  lazy val dropBytes: BaseFunction =
    NativeFunction("drop", 1, DROP_BYTES, BYTESTR, "Skip firsts bytes", ("xs", BYTESTR, "vector"), ("number", LONG, "Bytes number")) {
      case CONST_BYTESTR(xs) :: CONST_LONG(number) :: Nil => Right(CONST_BYTESTR(xs.drop(number)))
      case xs                                             => notImplemented("drop(xs: byte[], number: Long)", xs)
    }

  lazy val dropRightBytes: BaseFunction =
    UserFunction("dropRight", "dropRightBytes", 19, BYTESTR, "Cut vectors tail", ("@xs", BYTESTR, "vector"), ("@number", LONG, "cuting size")) {
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

  lazy val takeRightBytes: BaseFunction =
    UserFunction("takeRight", "takeRightBytes", 19, BYTESTR, "Take vector tail", ("@xs", BYTESTR, "vector"), ("@number", LONG, "taking size")) {
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

  lazy val takeString: BaseFunction =
    NativeFunction("take", 1, TAKE_STRING, STRING, "Take string prefix", ("xs", STRING, "sctring"), ("number", LONG, "prefix size in characters")) {
      case CONST_STRING(xs) :: CONST_LONG(number) :: Nil => Right(CONST_STRING(xs.take(trimLongToInt(number))))
      case xs                                            => notImplemented("take(xs: String, number: Long)", xs)
    }

  lazy val listConstructor: NativeFunction =
    NativeFunction(
      "cons",
      2,
      CREATE_LIST,
      PARAMETERIZEDLIST(PARAMETERIZEDUNION(List(TYPEPARAM('A'), TYPEPARAM('B')))),
      "Construct a new List[T]",
      ("head", TYPEPARAM('A'), "head"),
      ("tail", PARAMETERIZEDLIST(TYPEPARAM('B')), "tail")
    ) {
      case h :: ARR(t) :: Nil => Right(ARR(h +: t))
      case xs                 => notImplemented("cons(head: T, tail: LIST[T]", xs)
    }

  lazy val dropString: BaseFunction =
    NativeFunction("drop", 1, DROP_STRING, STRING, "Remmove sring prefix", ("xs", STRING, "string"), ("number", LONG, "prefix size")) {
      case CONST_STRING(xs) :: CONST_LONG(number) :: Nil => Right(CONST_STRING(xs.drop(trimLongToInt(number))))
      case xs                                            => notImplemented("drop(xs: String, number: Long)", xs)
    }

  lazy val takeRightString: BaseFunction =
    UserFunction("takeRight", 19, STRING, "Take string suffix", ("@xs", STRING, "String"), ("@number", LONG, "suffix size in characters")) {
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

  lazy val dropRightString: BaseFunction =
    UserFunction("dropRight", 19, STRING, "Remove string suffix", ("@xs", STRING, "string"), ("@number", LONG, "suffix size in characters")) {
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

  lazy val toUtf8String: BaseFunction =
    NativeFunction("toUtf8String", 20, UTF8STRING, STRING, "Convert UTF8 bytes to string", ("u", BYTESTR, "utf8")) {
      case CONST_BYTESTR(u) :: Nil => Try(CONST_STRING(UTF8Decoder.decode(ByteBuffer.wrap(u.arr)).toString)).toEither.left.map(_.toString)
      case xs                      => notImplemented("toUtf8String(u: byte[])", xs)
    }

  lazy val toLong: BaseFunction =
    NativeFunction("toInt", 10, BININT, LONG, "Deserialize big endian 8-bytes value", ("bin", BYTESTR, "8-bytes BE binaries")) {
      case CONST_BYTESTR(u) :: Nil => Try(CONST_LONG(ByteBuffer.wrap(u.arr).getLong())).toEither.left.map(_.toString)
      case xs                      => notImplemented("toInt(u: byte[])", xs)
    }

  lazy val toLongOffset: BaseFunction =
    NativeFunction("toInt", 10, BININT_OFF, LONG, "Deserialize big endian 8-bytes value", ("bin", BYTESTR, "8-bytes BE binaries"), ("offet", LONG, "bytes offset")) {
      case CONST_BYTESTR(ByteStr(u)) :: CONST_LONG(o) :: Nil => if( o >= 0 && o <= u.size - 8) {
          Try(CONST_LONG(ByteBuffer.wrap(u).getLong(o.toInt))).toEither.left.map(_.toString)
      } else {
        Left("IndexOutOfBounds")
      }
      case xs                      => notImplemented("toInt(u: byte[], off: int)", xs)
    }

  lazy val indexOf: BaseFunction =
    NativeFunction("indexOf", 20, INDEXOF, optionLong, "index of substring", ("str", STRING, "String for analize"), ("substr", STRING, "String for searching")) {
      case CONST_STRING(m) :: CONST_STRING(sub) :: Nil => Right({
        val i = m.indexOf(sub)
         if( i != -1 ) {
           CONST_LONG(i.toLong)
         } else {
           unit
         }
      })
      case xs                      => notImplemented("indexOf(STRING, STRING)", xs)
    }

  lazy val indexOfN: BaseFunction =
    NativeFunction("indexOf", 20, INDEXOFN, optionLong, "index of substring after offset", ("str", STRING, "String for analize"), ("substr", STRING, "String for searching"), ("offset", LONG, "offset")) {
      case CONST_STRING(m) :: CONST_STRING(sub) :: CONST_LONG(off) :: Nil => Right( if(off >= 0 && off <= m.length) {
         val i = m.indexOf(sub, off.toInt)
         if( i != -1 ) {
           CONST_LONG(i.toLong)
         } else {
           unit
         }
      } else {
        unit
      } )
      case xs                      => notImplemented("indexOf(STRING, STRING)", xs)
    }

  def split(m: String, sep: String, buffer: ArrayBuffer[CONST_STRING] =  ArrayBuffer[CONST_STRING](), start: Int = 0): IndexedSeq[CONST_STRING] = {
    m.indexOf(sep, start) match {
      case -1 =>
        buffer += CONST_STRING(m.substring(start))
        buffer.result
      case n =>
        buffer += CONST_STRING(m.substring(0, n))
        split(m, sep, buffer, n + sep.length)
    }
  }

  lazy val splitStr: BaseFunction =
    NativeFunction("split", 100, SPLIT, listString, "split string by separator", ("str", STRING, "String for splitting"), ("separator", STRING, "separator")) {
      case CONST_STRING(m) :: CONST_STRING(sep) :: Nil => Right( ARR(split(m, sep)))
      case xs                      => notImplemented("split(STRING, STRING)", xs)
    }

  lazy val parseInt: BaseFunction =
    NativeFunction("parseInt", 20, PARSEINT, optionLong, "parse string to integer", ("str", STRING, "String for parsing")) {
      case CONST_STRING(u) :: Nil => Try(CONST_LONG(u.toInt)).orElse(Success(unit)).toEither.left.map(_.toString)
      case xs                      => notImplemented("parseInt(STRING)", xs)
    }

  lazy val parseIntVal: BaseFunction =
    NativeFunction("parseIntValue", 20, PARSEINTV, LONG, "parse string to integer with fail on errors", ("str", STRING, "String for parsing")) {
      case CONST_STRING(u) :: Nil => Try(CONST_LONG(u.toInt)).toEither.left.map(_.toString)
      case xs                      => notImplemented("parseInt(STRING)", xs)
    }

  def createRawOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short, docString: String, arg1Doc: String, arg2Doc: String, complicity: Int = 1)(
      body: (EVALUATED, EVALUATED) => Either[String, EVALUATED]): BaseFunction =
    NativeFunction(opsToFunctions(op), complicity, func, r, docString, ("a", t, arg1Doc), ("b", t, arg2Doc)) {
      case a :: b :: Nil => body(a, b)
      case _             => ???
    }

  def createOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short, docString: String, arg1Doc: String, arg2Doc: String, complicity: Int = 1)(
      body: (Long, Long) => Boolean): BaseFunction =
    NativeFunction(opsToFunctions(op), complicity, func, r, docString, ("a", t, arg1Doc), ("b", t, arg2Doc)) {
      case CONST_LONG(a) :: CONST_LONG(b) :: Nil => Right(CONST_BOOLEAN(body(a, b)))
      case _                                     => ???
    }

  def createTryOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short, docString: String, arg1Doc: String, arg2Doc: String, complicity: Int = 1)(
      body: (Long, Long) => Long): BaseFunction =
    NativeFunction(opsToFunctions(op), complicity, func, r, docString, ("a", t, arg1Doc), ("b", t, arg2Doc)) {
      case CONST_LONG(a) :: CONST_LONG(b) :: Nil =>
        try {
          Right(CONST_LONG(body(a, b)))
        } catch {
          case e: Throwable => Left(e.getMessage)
        }
    }

  lazy val getElement: BaseFunction =
    NativeFunction(
      "getElement",
      2,
      GET_LIST,
      TYPEPARAM('T'),
      "Get list element by position",
      ("arr", PARAMETERIZEDLIST(TYPEPARAM('T')), "list"),
      ("pos", LONG, "element position")
    ) {
      case ARR(arr) :: CONST_LONG(pos) :: Nil => Try(arr(pos.toInt)).toEither.left.map(_.toString)
      case _                                  => ???
    }

  lazy val getListSize: BaseFunction =
    NativeFunction("size", 2, SIZE_LIST, LONG, "Size of list", ("arr", PARAMETERIZEDLIST(TYPEPARAM('T')), "list")) {
      case ARR(arr) :: Nil => Right(CONST_LONG(arr.size.toLong))
      case _               => ???
    }

  lazy val uMinus: BaseFunction =
    UserFunction("-", Map[StdLibVersion, Long](V1 -> 9, V2 -> 9, V3 -> 1), LONG, "Change integer sign", ("@n", LONG, "value")) {
      FUNCTION_CALL(subLong, List(CONST_LONG(0), REF("@n")))
    }

  lazy val uNot: BaseFunction =
    UserFunction("!", Map[StdLibVersion, Long](V1 -> 11, V2 -> 11, V3 -> 1), BOOLEAN, "unary negation", ("@p", BOOLEAN, "boolean")) {
      IF(REF("@p"), FALSE, TRUE)
    }

  lazy val ensure: BaseFunction =
    UserFunction("ensure", 16, BOOLEAN, "Ensure parameter is true", ("@b", BOOLEAN, "condition"), ("@msg", STRING, "error message")) {
      IF(REF("@b"), TRUE, FUNCTION_CALL(throwWithMessage, List(REF("@msg"))))
    }

  private lazy val operators: Array[BaseFunction] = Array(
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

  private lazy val vars: Map[String, ((FINAL, String), LazyVal)] = Map(
    ("unit", ((UNIT, "Single instance value"), LazyVal(EitherT.pure(unit))))
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
    throwNoMessage,
  ) ++ operators

  private lazy val ctx = CTX(
    Seq(
      new DefinedType { lazy val name = "Unit"; lazy val typeRef       = UNIT    },
      new DefinedType { lazy val name = "Int"; lazy val typeRef        = LONG    },
      new DefinedType { lazy val name = "Boolean"; lazy val typeRef    = BOOLEAN },
      new DefinedType { lazy val name = "ByteVector"; lazy val typeRef = BYTESTR },
      new DefinedType { lazy val name = "String"; lazy val typeRef     = STRING  }
    ),
    vars,
    functions
  )

  def build(version: StdLibVersion): CTX =
    version match {
      case V1 | V2 => ctx
      case V3 =>
        Monoid.combine(
          ctx,
          CTX(
            Seq.empty,
            Map(("nil", ((LIST(NOTHING), "empty list of any type"), LazyVal(EitherT.pure(ARR(IndexedSeq.empty[EVALUATED])))))),
            Array(value, listConstructor, ensure, toUtf8String, toLong, toLongOffset, indexOf, indexOfN, splitStr, parseInt, parseIntVal)
          )
        )
    }

}
