package com.wavesplatform.lang.v1.evaluator.ctx.impl

import cats.implicits.*
import cats.{Id, Monad}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.*
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.utils.getDecompilerContext
import com.wavesplatform.lang.v1.ContractLimits.*
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BYTESTR.NoLimit
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.FunctionIds.*
import com.wavesplatform.lang.v1.evaluator.ctx.*
import com.wavesplatform.lang.v1.evaluator.{ContextfulUserFunction, ContextfulVal}
import com.wavesplatform.lang.v1.parser.BinaryOperation
import com.wavesplatform.lang.v1.parser.BinaryOperation.*
import com.wavesplatform.lang.v1.{BaseGlobal, CTX, FunctionHeader, compiler}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.charset.{MalformedInputException, StandardCharsets}
import java.nio.{BufferUnderflowException, ByteBuffer}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.{Success, Try}

object PureContext {
  private val global: BaseGlobal = com.wavesplatform.lang.Global

  implicit def intToLong(num: Int): Long  = num.toLong
  private def trimLongToInt(x: Long): Int = Math.toIntExact(Math.max(Math.min(x, Int.MaxValue), Int.MinValue))

  private val defaultThrowMessage = "Explicit script termination"

  val BigIntMax: BigInt = BigInt(2).pow(511) - 1
  val BigIntMin: BigInt = -BigIntMax - 1
  val MaxListLengthV4   = 1000

  // As an optimization, JVM might throw an ArithmeticException with empty stack trace and null message.
  // The workaround below rethrows an exception with the message explicitly set.
  lazy val divLong: BaseFunction[NoContext] =
    createTryOp(DIV_OP, LONG, LONG, DIV_LONG) { (a, b) =>
      try Math.floorDiv(a, b)
      catch { case _: ArithmeticException => throw new ArithmeticException("/ by zero") }
    }
  lazy val modLong: BaseFunction[NoContext] =
    createTryOp(MOD_OP, LONG, LONG, MOD_LONG) { (a, b) =>
      try Math.floorMod(a, b)
      catch { case _: ArithmeticException => throw new ArithmeticException("/ by zero") }
    }

  lazy val mulLong: BaseFunction[NoContext] =
    createTryOp(MUL_OP, LONG, LONG, MUL_LONG)((a, b) => Math.multiplyExact(a, b))
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
      Map[StdLibVersion, Long](V1 -> 10L, V2 -> 10L, V3 -> 10L, V4 -> 20L, V5 -> 20L, V6 -> 1L)
    ) {
      case (s1 @ CONST_STRING(a), s2 @ CONST_STRING(b)) =>
        val sumWeight = (s1.weight + s2.weight).toInt
        if (sumWeight <= Terms.DataEntryValueMax) {
          CONST_STRING(a + b, bytesLength = Some(sumWeight))
        } else {
          Left(s"String size = $sumWeight exceeds ${Terms.DataEntryValueMax} bytes")
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
      Map[StdLibVersion, Long](V1 -> 10L, V2 -> 10L, V3 -> 10L, V4 -> 2L)
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
  lazy val geBigInt: BaseFunction[NoContext] = bigIntConditionOp(GE_OP, GE_BIGINT)(_ >= _)
  lazy val gtBigInt: BaseFunction[NoContext] = bigIntConditionOp(GT_OP, GT_BIGINT)(_ > _)

  lazy val eq: BaseFunction[NoContext] =
    NativeFunction(EQ_OP.func, 1, EQ, BOOLEAN, ("a", TYPEPARAM('T')), ("b", TYPEPARAM('T'))) {
      case a :: b :: Nil =>
        Either.cond(b.weight <= MaxCmpWeight || a.weight <= MaxCmpWeight, CONST_BOOLEAN(a == b), "Comparable value too heavy.")
      case xs => notImplemented[Id, EVALUATED](s"${EQ_OP.func}(a: T, b: T)", xs)
    }

  lazy val ne: BaseFunction[NoContext] =
    UserFunction(
      NE_OP.func,
      Map[StdLibVersion, Long](V1 -> 26, V2 -> 26, V3 -> 1, V4 -> 1),
      BOOLEAN,
      ("@a", TYPEPARAM('T')),
      ("@b", TYPEPARAM('T'))
    ) {
      FUNCTION_CALL(uNot, List(FUNCTION_CALL(eq, List(REF("@a"), REF("@b")))))
    }

  lazy val intToBigInt: BaseFunction[NoContext] =
    NativeFunction("toBigInt", 1, TO_BIGINT, BIGINT, ("n", LONG)) {
      case CONST_LONG(n) :: Nil => Right(CONST_BIGINT(BigInt(n)))
      case xs                   => notImplemented[Id, EVALUATED]("toBigInt(n: Int)", xs)
    }

  lazy val bigIntToInt: BaseFunction[NoContext] =
    NativeFunction("toInt", 1, BIGINT_TO_INT, LONG, ("n", BIGINT)) {
      case CONST_BIGINT(n) :: Nil =>
        Either.cond(Long.MaxValue >= n && n >= Long.MinValue, CONST_LONG(n.toLong), s"toInt: BigInt $n out of integers range")
      case xs => notImplemented[Id, EVALUATED]("toBigInt(n: Int)", xs)
    }

  lazy val bigIntToString: BaseFunction[NoContext] =
    NativeFunction("toString", Map(V5 -> 65L, V6 -> 1L), BIGINT_TO_STRING, STRING, ("n", BIGINT)) {
      case CONST_BIGINT(n) :: Nil => CONST_STRING(n.toString)
      case xs                     => notImplemented[Id, EVALUATED]("toString(n: BigInt)", xs)
    }

  lazy val stringToBigInt: BaseFunction[NoContext] =
    NativeFunction("parseBigIntValue", Map(V5 -> 65L, V6 -> 65L, V7 -> 65L, V8 -> 1L), STRING_TO_BIGINT, BIGINT, ("n", STRING)) {
      case CONST_STRING(n) :: Nil =>
        Either
          .cond(n.length <= 155, BigInt(n), s"String too long for 512-bits big integers (${n.length} when max is 155)")
          .filterOrElse(v => v <= BigIntMax && v >= BigIntMin, "Value too big for 512-bits big integer")
          .map(CONST_BIGINT.apply)
          .leftMap(CommonError(_))
      case xs => notImplemented[Id, EVALUATED]("parseBigIntValue(n: String)", xs)
    }

  lazy val stringToBigIntOpt: BaseFunction[NoContext] =
    NativeFunction("parseBigInt", Map(V5 -> 65L, V6 -> 65L, V7 -> 65L, V8 -> 1L), STRING_TO_BIGINTOPT, UNION(BIGINT, UNIT), ("n", STRING)) {
      case CONST_STRING(n) :: Nil =>
        Right((if (n.length <= 155) {
                 try {
                   val v = BigInt(n)
                   if (v <= BigIntMax && v >= BigIntMin) {
                     CONST_BIGINT(v)
                   } else {
                     unit
                   }
                 } catch {
                   case e: java.lang.NumberFormatException => unit
                 }
               } else {
                 unit
               }))
      case xs => notImplemented[Id, EVALUATED]("parseBigInt(n: String)", xs)
    }

  lazy val bigIntToBytes: BaseFunction[NoContext] =
    NativeFunction("toBytes", Map(V5 -> 65L, V6 -> 65L, V7 -> 65L, V8 -> 1L), BIGINT_TO_BYTES, BYTESTR, ("n", BIGINT)) {
      case CONST_BIGINT(n) :: Nil => CONST_BYTESTR(ByteStr(n.toByteArray))
      case xs                     => notImplemented[Id, EVALUATED]("toBytes(n: BigInt)", xs)
    }

  lazy val bytesToBigIntLim: BaseFunction[NoContext] =
    NativeFunction(
      "toBigInt",
      Map(V5 -> 65L, V6 -> 65L, V7 -> 65L, V8 -> 1L),
      BYTES_TO_BIGINT_LIM,
      BIGINT,
      ("n", BYTESTR),
      ("off", LONG),
      ("size", LONG)
    ) {
      case CONST_BYTESTR(ByteStr(n)) :: CONST_LONG(off) :: CONST_LONG(s) :: Nil =>
        Either.cond(
          off >= 0 && off <= n.length && s <= 64 && s > 0,
          CONST_BIGINT(BigInt(n.slice(off.toInt, (off + s).toInt))),
          s"ByteStr too long ($s > 64 bytes)"
        )
      case xs => notImplemented[Id, EVALUATED]("toBigInt(n: ByteStr, offset: Int, size: Int)", xs)
    }

  lazy val bytesToBigInt: BaseFunction[NoContext] =
    NativeFunction("toBigInt", Map(V5 -> 65L, V6 -> 65L, V7 -> 65L, V8 -> 1L), BYTES_TO_BIGINT, BIGINT, ("n", BYTESTR)) {
      case CONST_BYTESTR(ByteStr(n)) :: Nil =>
        Either.cond(n.length <= 64, CONST_BIGINT(BigInt(n)), s"Too big ByteVector for BigInt (${n.length} > 64 bytes)")
      case xs => notImplemented[Id, EVALUATED]("toBigInt(n: ByteStr)", xs)
    }

  def bigIntArithmeticOp(op: BinaryOperation, func: Short, complexity: Map[StdLibVersion, Long])(
      body: (BigInt, BigInt) => BigInt
  ): BaseFunction[NoContext] = {
    createRawOp(
      op,
      BIGINT,
      BIGINT,
      func,
      complexity
    ) {
      case (CONST_BIGINT(a), CONST_BIGINT(b)) =>
        Try(body(a, b)).toEither
          .leftMap(_.getMessage)
          .filterOrElse(
            r => r >= BigIntMin && r <= BigIntMax,
            s"$a ${op.func} $b is out of range."
          )
          .map(CONST_BIGINT.apply)
          .leftMap(CommonError(_))
      case args =>
        Left(s"Unexpected args $args for BigInt operator '${op.func}'")
    }
  }

  lazy val sumToBigInt: BaseFunction[NoContext] =
    bigIntArithmeticOp(SUM_OP, SUM_BIGINT, Map[StdLibVersion, Long](V5 -> 8L, V6 -> 8L, V7 -> 8L, V8 -> 1L)) { _ + _ }
  lazy val subToBigInt: BaseFunction[NoContext] =
    bigIntArithmeticOp(SUB_OP, SUB_BIGINT, Map[StdLibVersion, Long](V5 -> 8L, V6 -> 8L, V7 -> 8L, V8 -> 1L)) { _ - _ }
  lazy val mulToBigInt: BaseFunction[NoContext] =
    bigIntArithmeticOp(MUL_OP, MUL_BIGINT, Map[StdLibVersion, Long](V5 -> 64L, V6 -> 64L, V7 -> 64L, V8 -> 1L)) { _ * _ }
  lazy val divToBigInt: BaseFunction[NoContext] =
    bigIntArithmeticOp(DIV_OP, DIV_BIGINT, Map[StdLibVersion, Long](V5 -> 64L, V6 -> 64L, V7 -> 64L, V8 -> 1L)) { _ / _ }
  lazy val modToBigInt: BaseFunction[NoContext] =
    bigIntArithmeticOp(MOD_OP, MOD_BIGINT, Map[StdLibVersion, Long](V5 -> 64L, V6 -> 64L, V7 -> 64L, V8 -> 1L)) { _ % _ }

  lazy val negativeBigInt: BaseFunction[NoContext] =
    NativeFunction("-", Map(V5 -> 8L, V6 -> 8L, V7 -> 8L, V8 -> 1L), UMINUS_BIGINT, BIGINT, ("n", BIGINT)) {
      case CONST_BIGINT(n) :: Nil => Either.cond(n != BigIntMin, CONST_BIGINT(-n), s"Positive BigInt overflow")
      case xs                     => notImplemented[Id, EVALUATED]("-(n: BigInt)", xs)
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
        FUNCTION_CALL(eq, List(REF("@a"), REF(GlobalValNames.Unit))),
        FUNCTION_CALL(throwWithMessage, List(CONST_STRING("extract() called on unit value").explicitGet())),
        REF("@a")
      )
    }

  lazy val value: BaseFunction[NoContext] =
    UserFunction.withEnvironment[NoContext](
      "value",
      "value",
      Map[StdLibVersion, Long](V1 -> 13, V2 -> 13, V3 -> 13, V4 -> 2),
      TYPEPARAM('T'),
      ("@a", PARAMETERIZEDUNION(List(TYPEPARAM('T'), UNIT)): TYPE)
    ) {
      new ContextfulUserFunction[NoContext] {
        override def apply[F[_]: Monad](env: NoContext[F], startArgs: List[EXPR]): EXPR = {
          val ctx  = getDecompilerContext(DirectiveDictionary[StdLibVersion].all.last, Expression)
          val base = "value() called on unit value"
          def call(h: FunctionHeader, postfix: Boolean = true) = {
            val name = h match {
              case Native(id) => ctx.opCodes.get(id).orElse(ctx.binaryOps.get(id)).getOrElse(id.toString)
              case u: User    => u.name
            }
            s"on function '$name${if (postfix) "Value" else ""}' call"
          }
          def address(args: List[EXPR]) = s"base58'${args.head.asInstanceOf[CaseObj].fields.head._2}'"
          val errorMessage = startArgs.head match {
            case FUNCTION_CALL(h, args) if stateDataHeaders(h) => s"value by key '${args(1)}' not found for the address ${address(args)} ${call(h)}"
            case FUNCTION_CALL(h, args) if stateDataSelfHeaders(h)    => s"value by key '${args(0)}' not found for the contract address ${call(h)}"
            case FUNCTION_CALL(h, args) if arrayDataByKeyHeaders(h)   => s"value by key '${args(1)}' not found in the list ${call(h)}"
            case FUNCTION_CALL(h, args) if arrayDataByIndexHeaders(h) => s"value by index ${args(1)} not found in the list ${call(h)}"
            case FUNCTION_CALL(h, _)                                  => s"$base ${call(h, postfix = false)}"
            case GETTER(_, field)                                     => s"$base while accessing field '$field'"
            case REF(key)                                             => s"$base by reference '$key'"
            case LET_BLOCK(_, _)                                      => s"$base after let block evaluation"
            case BLOCK(_, _)                                          => s"$base after block evaluation"
            case IF(_, _, _)                                          => s"$base after condition evaluation"
            case _                                                    => base
          }
          IF(
            FUNCTION_CALL(PureContext.eq, List(REF("@a"), REF(GlobalValNames.Unit))),
            FUNCTION_CALL(throwWithMessage, List(CONST_STRING(errorMessage).explicitGet())),
            REF("@a")
          )
        }
      }
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
        FUNCTION_CALL(eq, List(REF("@value"), REF(GlobalValNames.Unit))),
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
        FUNCTION_CALL(eq, List(REF("@a"), REF(GlobalValNames.Unit))),
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
      FUNCTION_CALL(ne, List(REF("@a"), REF(GlobalValNames.Unit)))
    }

  def fraction(fixLimitCheck: Boolean): BaseFunction[NoContext] =
    NativeFunction(
      "fraction",
      Map[StdLibVersion, Long](V1 -> 1, V2 -> 1, V3 -> 1, V4 -> 1, V5 -> 14, V6 -> 1),
      FRACTION,
      LONG,
      ("value", LONG),
      ("numerator", LONG),
      ("denominator", LONG)
    ) {
      case CONST_LONG(v) :: CONST_LONG(n) :: CONST_LONG(d) :: Nil =>
        val (checkMax, checkMin) =
          if (fixLimitCheck)
            ((_: BigInt) <= Long.MaxValue, (_: BigInt) >= Long.MinValue)
          else
            ((_: BigInt) < Long.MaxValue, (_: BigInt) > Long.MinValue)
        val r = for {
          _ <- Either.cond(d != 0, (), "Fraction: division by zero")
          result = BigInt(v) * n / d
          _ <- Either.cond(checkMax(result), (), s"Long overflow: value `$result` greater than 2^63-1")
          _ <- Either.cond(checkMin(result), (), s"Long overflow: value `$result` less than -2^63-1")
        } yield CONST_LONG(result.toLong)
        r.leftMap(CommonError(_))
      case xs => notImplemented[Id, EVALUATED]("fraction(value: Int, numerator: Int, denominator: Int)", xs)
    }

  def fractionIntRounds(roundTypes: UNION): BaseFunction[NoContext] =
    UserFunction(
      "fraction",
      Map(V5 -> 17L, V6 -> 4L),
      LONG,
      ("@value", LONG),
      ("@numerator", LONG),
      ("@denominator", LONG),
      ("@round", roundTypes)
    ) {
      val r = FUNCTION_CALL(
        Native(FRACTION_BIGINT_ROUNDS),
        List(
          FUNCTION_CALL(Native(TO_BIGINT), List(REF("@value"))),
          FUNCTION_CALL(Native(TO_BIGINT), List(REF("@numerator"))),
          FUNCTION_CALL(Native(TO_BIGINT), List(REF("@denominator"))),
          REF("@round")
        )
      )
      FUNCTION_CALL(Native(BIGINT_TO_INT), List(r))
    }

  val fractionIntRoundsNative: BaseFunction[NoContext] =
    NativeFunction(
      "fraction",
      1L,
      FRACTION_ROUNDS,
      LONG,
      ("value", LONG),
      ("numerator", LONG),
      ("denominator", LONG),
      ("round", UNION(fromV5RoundTypes))
    ) {
      case CONST_LONG(v) :: CONST_LONG(n) :: CONST_LONG(d) :: (round: CaseObj) :: Nil =>
        val r = for {
          _        <- Either.cond(d != 0, (), "Fraction: division by zero")
          division <- global.divide(BigInt(v) * BigInt(n), d, Rounding.byValue(round))
          _        <- Either.cond(division.isValidLong, (), s"Fraction result $division out of integers range")
        } yield CONST_LONG(division.longValue)
        r.leftMap(CommonError(_))
      case xs =>
        notImplemented[Id, EVALUATED](
          "fraction(value: Int, numerator: Int, denominator: Int, round: Ceiling|Down|Floor|HalfEven|HalfUp)",
          xs
        )
    }

  val fractionBigInt: BaseFunction[NoContext] =
    NativeFunction(
      "fraction",
      Map(V5 -> 128L, V6 -> 1L),
      FRACTION_BIGINT,
      BIGINT,
      ("value", BIGINT),
      ("numerator", BIGINT),
      ("denominator", BIGINT)
    ) {
      case CONST_BIGINT(v) :: CONST_BIGINT(n) :: CONST_BIGINT(d) :: Nil =>
        val r = for {
          _ <- Either.cond(d != 0, (), "Fraction: division by zero")
          result = v * n / d
          _ <- Either.cond(result <= BigIntMax, (), s"Long overflow: value `$result` greater than 2^511-1")
          _ <- Either.cond(result >= BigIntMin, (), s"Long overflow: value `$result` less than -2^511")
        } yield CONST_BIGINT(result)
        r.leftMap(CommonError(_))
      case xs => notImplemented[Id, EVALUATED]("fraction(value: BigInt, numerator: BigInt, denominator: BigInt)", xs)
    }

  def fractionBigIntRounds(roundTypes: UNION): BaseFunction[NoContext] =
    NativeFunction(
      "fraction",
      Map(V5 -> 128L, V6 -> 1L),
      FRACTION_BIGINT_ROUNDS,
      BIGINT,
      ("value", BIGINT),
      ("numerator", BIGINT),
      ("denominator", BIGINT),
      ("round", roundTypes)
    ) {
      case CONST_BIGINT(v) :: CONST_BIGINT(n) :: CONST_BIGINT(d) :: (round: CaseObj) :: Nil =>
        val r = for {
          _ <- Either.cond(d != 0, (), "Fraction: division by zero")
          r <- global.divide(v * n, d, Rounding.byValue(round))
          _ <- Either.cond(r <= BigIntMax, (), s"Long overflow: value `$r` greater than 2^511-1")
          _ <- Either.cond(r >= BigIntMin, (), s"Long overflow: value `$r` less than -2^511")
        } yield CONST_BIGINT(r)
        r.leftMap(CommonError(_))
      case xs =>
        notImplemented[Id, EVALUATED](
          "fraction(value: BigInt, numerator: BigInt, denominator: BigInt, round: Ceiling|Down|Floor|HalfEven|HalfUp)",
          xs
        )
    }

  lazy val _isInstanceOf: BaseFunction[NoContext] =
    NativeFunction(compiler.IsInstanceOf, 1, ISINSTANCEOF, BOOLEAN, ("obj", TYPEPARAM('T')), ("of", STRING)) {
      case (value: EVALUATED) :: CONST_STRING(expectedType) :: Nil =>
        Right(CONST_BOOLEAN(value.getType.name == expectedType))
      case _ =>
        Right(FALSE)
    }

  lazy val _getType: BaseFunction[NoContext] =
    NativeFunction(compiler.GetType, 1, GET_TYPE, BOOLEAN, ("obj", TYPEPARAM('T'))) {
      case (value: EVALUATED) :: Nil =>
        CONST_STRING(value.getType.name)
      case xs =>
        notImplemented[Id, EVALUATED]("_getType(obj: T)", xs)
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

  lazy val sizeStringFixed: BaseFunction[NoContext] = NativeFunction("size", 1, SIZE_STRING, LONG, ("xs", STRING)) {
    case CONST_STRING(bv) :: Nil => Right(CONST_LONG(bv.codePointCount(0, bv.length).toLong))
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

  private def takeBytes(checkLimits: Boolean): BaseFunction[NoContext] =
    NativeFunction(
      "take",
      Map[StdLibVersion, Long](V1 -> 1L, V2 -> 1L, V3 -> 1L, V4 -> 6L),
      TAKE_BYTES,
      BYTESTR,
      ("xs", BYTESTR),
      ("number", LONG)
    ) {
      case CONST_BYTESTR(xs) :: CONST_LONG(number) :: Nil =>
        val limit = Terms.DataTxMaxProtoBytes
        if (checkLimits) {
          if (number < 0)
            Left(s"Unexpected negative number = $number passed to take()")
          else if (number > limit)
            Left(s"Number = $number passed to take() exceeds ByteVector limit = $limit")
          else
            CONST_BYTESTR(xs.take(number.toInt), NoLimit)
        } else
          CONST_BYTESTR(xs.take(number.toInt))
      case xs =>
        notImplemented[Id, EVALUATED]("take(xs: ByteVector, number: Int)", xs)
    }

  private def dropBytes(checkLimits: Boolean): BaseFunction[NoContext] =
    NativeFunction(
      "drop",
      Map[StdLibVersion, Long](V1 -> 1L, V2 -> 1L, V3 -> 1L, V4 -> 6L),
      DROP_BYTES,
      BYTESTR,
      ("xs", BYTESTR),
      ("number", LONG)
    ) {
      case CONST_BYTESTR(xs) :: CONST_LONG(number) :: Nil =>
        val limit = Terms.DataTxMaxProtoBytes
        if (checkLimits) {
          if (number < 0)
            Left(s"Unexpected negative number = $number passed to drop()")
          else if (number > limit)
            Left(s"Number = $number passed to drop() exceeds ByteVector limit = $limit")
          else
            CONST_BYTESTR(xs.drop(number.toInt), NoLimit)
        } else
          CONST_BYTESTR(xs.drop(number.toInt))
      case xs =>
        notImplemented[Id, EVALUATED]("drop(xs: ByteVector, number: Int)", xs)
    }

  private val dropBytesBeforeV6 = dropBytes(checkLimits = false)
  private val dropBytesFromV6   = dropBytes(checkLimits = true)
  private val takeBytesBeforeV6 = takeBytes(checkLimits = false)
  private val takeBytesFromV6   = takeBytes(checkLimits = true)

  private val dropRightBytesBeforeV6: BaseFunction[NoContext] =
    UserFunction(
      "dropRight",
      "dropRightBytes",
      Map[StdLibVersion, Long](V1 -> 19L, V2 -> 19L, V3 -> 19L, V4 -> 6L),
      BYTESTR,
      ("@xs", BYTESTR),
      ("@number", LONG)
    ) {
      FUNCTION_CALL(
        takeBytesBeforeV6,
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

  private val takeRightBytesBeforeV6: BaseFunction[NoContext] =
    UserFunction(
      "takeRight",
      "takeRightBytes",
      Map[StdLibVersion, Long](V1 -> 19L, V2 -> 19L, V3 -> 19L, V4 -> 6L),
      BYTESTR,
      ("@xs", BYTESTR),
      ("@number", LONG)
    ) {
      FUNCTION_CALL(
        dropBytesBeforeV6,
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

  private val takeRightBytesFromV6: BaseFunction[NoContext] =
    NativeFunction(
      "takeRight",
      6,
      TAKE_RIGHT_BYTES,
      BYTESTR,
      ("xs", BYTESTR),
      ("number", LONG)
    ) {
      case CONST_BYTESTR(xs) :: CONST_LONG(number) :: Nil =>
        val limit = Terms.DataTxMaxProtoBytes
        if (number < 0)
          Left(s"Unexpected negative number = $number passed to takeRight()")
        else if (number > limit)
          Left(s"Number = $number passed to takeRight() exceeds ByteVector limit = $limit")
        else
          CONST_BYTESTR(xs.takeRight(number.toInt), NoLimit)
      case xs =>
        notImplemented[Id, EVALUATED]("takeRight(xs: ByteVector, number: Int)", xs)
    }

  private val dropRightBytesFromV6: BaseFunction[NoContext] =
    NativeFunction(
      "dropRight",
      6,
      DROP_RIGHT_BYTES,
      BYTESTR,
      ("xs", BYTESTR),
      ("number", LONG)
    ) {
      case CONST_BYTESTR(xs) :: CONST_LONG(number) :: Nil =>
        val limit = Terms.DataTxMaxProtoBytes
        if (number < 0)
          Left(s"Unexpected negative number = $number passed to dropRight()")
        else if (number > limit)
          Left(s"Number = $number passed to dropRight() exceeds ByteVector limit = $limit")
        else
          CONST_BYTESTR(xs.dropRight(number.toInt), NoLimit)
      case xs =>
        notImplemented[Id, EVALUATED]("dropRight(xs: ByteVector, number: Int)", xs)
    }

  private val takeStringBeforeV6: BaseFunction[NoContext] =
    NativeFunction(
      "take",
      Map[StdLibVersion, Long](V1 -> 1L, V2 -> 1L, V3 -> 1L, V4 -> 20L),
      TAKE_STRING,
      STRING,
      ("xs", STRING),
      ("number", LONG)
    ) {
      case CONST_STRING(xs) :: CONST_LONG(number) :: Nil => CONST_STRING(xs.take(trimLongToInt(number)))
      case xs                                            => notImplemented[Id, EVALUATED]("take(xs: String, number: Int)", xs)
    }

  private def takeStringFixed(checkLimits: Boolean): BaseFunction[NoContext] =
    NativeFunction(
      "take",
      Map[StdLibVersion, Long](V1 -> 1L, V2 -> 1L, V3 -> 1L, V4 -> 20L, V5 -> 20L),
      TAKE_STRING,
      STRING,
      ("xs", STRING),
      ("number", LONG)
    ) {
      case CONST_STRING(xs) :: CONST_LONG(number) :: Nil =>
        val limit = Terms.DataEntryValueMax
        if (checkLimits) {
          if (number < 0) Left(s"Unexpected negative number = $number passed to take()")
          else if (number > limit) Left(s"Number = $number passed to take() exceeds String limit = $limit")
          else {
            val correctedNumber = number.max(0).min(xs.codePointCount(0, xs.length))
            CONST_STRING(xs.take(xs.offsetByCodePoints(0, trimLongToInt(correctedNumber))))
          }
        } else {
          val correctedNumber = number.max(0).min(xs.codePointCount(0, xs.length))
          CONST_STRING(xs.take(xs.offsetByCodePoints(0, trimLongToInt(correctedNumber))))
        }
      case xs =>
        notImplemented[Id, EVALUATED]("take(xs: String, number: Int)", xs)
    }

  private val takeStringFixedBeforeV6 = takeStringFixed(checkLimits = false)
  private val takeStringFixedFromV6   = takeStringFixed(checkLimits = true)

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

  private val dropStringBeforeV6: BaseFunction[NoContext] =
    NativeFunction(
      "drop",
      Map[StdLibVersion, Long](V1 -> 1L, V2 -> 1L, V3 -> 1L, V4 -> 20L),
      DROP_STRING,
      STRING,
      ("xs", STRING),
      ("number", LONG)
    ) {
      case CONST_STRING(xs) :: CONST_LONG(number) :: Nil => CONST_STRING(xs.drop(trimLongToInt(number)))
      case xs                                            => notImplemented[Id, EVALUATED]("drop(xs: String, number: Int)", xs)
    }

  private def dropStringFixed(checkLimits: Boolean): BaseFunction[NoContext] =
    NativeFunction(
      "drop",
      Map[StdLibVersion, Long](V1 -> 1L, V2 -> 1L, V3 -> 1L, V4 -> 20L, V5 -> 20L),
      DROP_STRING,
      STRING,
      ("xs", STRING),
      ("number", LONG)
    ) {
      case CONST_STRING(xs) :: CONST_LONG(number) :: Nil =>
        val limit = Terms.DataEntryValueMax
        if (checkLimits) {
          if (number < 0) Left(s"Unexpected negative number = $number passed to drop()")
          else if (number > limit) Left(s"Number = $number passed to drop() exceeds String limit = $limit")
          else {
            val correctedNumber = number.max(0).min(xs.codePointCount(0, xs.length))
            CONST_STRING(xs.drop(xs.offsetByCodePoints(0, trimLongToInt(correctedNumber))))
          }
        } else {
          val correctedNumber = number.max(0).min(xs.codePointCount(0, xs.length))
          CONST_STRING(xs.drop(xs.offsetByCodePoints(0, trimLongToInt(correctedNumber))))
        }
      case xs =>
        notImplemented[Id, EVALUATED]("drop(xs: String, number: Int)", xs)
    }

  private val dropStringFixedBeforeV6 = dropStringFixed(checkLimits = false)
  private val dropStringFixedFromV6   = dropStringFixed(checkLimits = true)

  private val takeRightStringBeforeV6: BaseFunction[NoContext] =
    UserFunction(
      "takeRight",
      Map[StdLibVersion, Long](V1 -> 19L, V2 -> 19L, V3 -> 19L, V4 -> 20L),
      STRING,
      ("@xs", STRING),
      ("@number", LONG)
    ) {
      FUNCTION_CALL(
        dropStringBeforeV6,
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

  private val takeRightStringFixedBeforeV6: BaseFunction[NoContext] =
    UserFunction(
      "takeRight",
      Map[StdLibVersion, Long](V1 -> 19L, V2 -> 19L, V3 -> 19L, V4 -> 20L, V5 -> 20L),
      STRING,
      ("@xs", STRING),
      ("@number", LONG)
    ) {
      FUNCTION_CALL(
        dropStringFixedBeforeV6,
        List(
          REF("@xs"),
          FUNCTION_CALL(
            subLong,
            List(
              FUNCTION_CALL(sizeStringFixed, List(REF("@xs"))),
              REF("@number")
            )
          )
        )
      )
    }

  private val takeRightStringFromV6: BaseFunction[NoContext] =
    NativeFunction(
      "takeRight",
      20L,
      TAKE_RIGHT_STRING,
      STRING,
      ("xs", STRING),
      ("number", LONG)
    ) {
      case CONST_STRING(xs) :: CONST_LONG(number) :: Nil =>
        val limit = Terms.DataEntryValueMax
        if (number < 0) Left(s"Unexpected negative number = $number passed to takeRight()")
        else if (number > limit) Left(s"Number = $number passed to takeRight() exceeds String limit = $limit")
        else {
          val correctedNumber = number.max(0).min(xs.codePointCount(0, xs.length))
          CONST_STRING(xs.takeRight(xs.offsetByCodePoints(0, trimLongToInt(correctedNumber))))
        }
      case xs =>
        notImplemented[Id, EVALUATED]("takeRight(xs: String, number: Int)", xs)
    }

  private val dropRightStringBeforeV6: BaseFunction[NoContext] =
    UserFunction(
      "dropRight",
      Map[StdLibVersion, Long](V1 -> 19L, V2 -> 19L, V3 -> 19L, V4 -> 20L),
      STRING,
      ("@xs", STRING),
      ("@number", LONG)
    ) {
      FUNCTION_CALL(
        takeStringBeforeV6,
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

  private val dropRightStringFixedBeforeV6: BaseFunction[NoContext] =
    UserFunction(
      "dropRight",
      Map[StdLibVersion, Long](V1 -> 19L, V2 -> 19L, V3 -> 19L, V4 -> 20L, V5 -> 20L),
      STRING,
      ("@xs", STRING),
      ("@number", LONG)
    ) {
      FUNCTION_CALL(
        takeStringFixedBeforeV6,
        List(
          REF("@xs"),
          FUNCTION_CALL(
            subLong,
            List(
              FUNCTION_CALL(sizeStringFixed, List(REF("@xs"))),
              REF("@number")
            )
          )
        )
      )
    }

  private val dropRightStringFromV6: BaseFunction[NoContext] =
    NativeFunction(
      "dropRight",
      20L,
      DROP_RIGHT_STRING,
      STRING,
      ("xs", STRING),
      ("number", LONG)
    ) {
      case CONST_STRING(xs) :: CONST_LONG(number) :: Nil =>
        val limit = Terms.DataEntryValueMax
        if (number < 0) Left(s"Unexpected negative number = $number passed to dropRight()")
        else if (number > limit) Left(s"Number = $number passed to dropRight() exceeds String limit = $limit")
        else {
          val correctedNumber = number.max(0).min(xs.codePointCount(0, xs.length))
          CONST_STRING(xs.dropRight(xs.offsetByCodePoints(0, trimLongToInt(correctedNumber))))
        }
      case xs =>
        notImplemented[Id, EVALUATED]("dropRight(xs: String, number: Int)", xs)
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
    NativeFunction("toInt", Map[StdLibVersion, Long](V1 -> 10L, V2 -> 10L, V3 -> 10L, V4 -> 1L), BININT, LONG, ("bin", BYTESTR)) {
      case CONST_BYTESTR(u) :: Nil =>
        Try(CONST_LONG(ByteBuffer.wrap(u.arr).getLong())).toEither.left.map {
          case _: BufferUnderflowException => "Buffer underflow"
          case e                           => e.toString
        }
      case xs => notImplemented[Id, EVALUATED]("toInt(u: ByteVector)", xs)
    }

  lazy val toLongOffset: BaseFunction[NoContext] =
    NativeFunction(
      "toInt",
      Map[StdLibVersion, Long](V1 -> 10L, V2 -> 10L, V3 -> 10L, V4 -> 1L),
      BININT_OFF,
      LONG,
      ("bin", BYTESTR),
      ("offset", LONG)
    ) {
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

  lazy val indexOfFixed: BaseFunction[NoContext] =
    NativeFunction(
      "indexOf",
      Map[StdLibVersion, Long](V1 -> 20L, V2 -> 20L, V3 -> 20L, V4 -> 3L, V5 -> 3L),
      INDEXOF,
      optionLong,
      ("str", STRING),
      ("substr", STRING)
    ) {
      case CONST_STRING(m) :: CONST_STRING(sub) :: Nil =>
        Right {
          val i = m.indexOf(sub)
          if (sub.isWellFormed && i != -1)
            CONST_LONG(m.codePointCount(0, i).toLong)
          else
            unit
        }
      case xs => notImplemented[Id, EVALUATED]("indexOf(str: String, substr: String)", xs)
    }

  lazy val indexOfN: BaseFunction[NoContext] =
    NativeFunction(
      "indexOf",
      Map[StdLibVersion, Long](V1 -> 20L, V2 -> 20L, V3 -> 20L, V4 -> 3L),
      INDEXOFN,
      optionLong,
      ("str", STRING),
      ("substr", STRING),
      ("offset", LONG)
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

  lazy val indexOfNFixed: BaseFunction[NoContext] =
    NativeFunction(
      "indexOf",
      Map[StdLibVersion, Long](V1 -> 20L, V2 -> 20L, V3 -> 20L, V4 -> 3L, V5 -> 3L),
      INDEXOFN,
      optionLong,
      ("str", STRING),
      ("substr", STRING),
      ("offset", LONG)
    ) {
      case CONST_STRING(m) :: CONST_STRING(sub) :: CONST_LONG(off) :: Nil =>
        val l = m.codePointCount(0, m.length)
        Right(if (sub.isWellFormed && off >= 0 && off <= l) {
          val i = m.indexOf(sub, m.offsetByCodePoints(0, off.toInt))
          if (i != -1) {
            CONST_LONG(m.codePointCount(0, i).toLong)
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

  lazy val lastIndexOfFixed: BaseFunction[NoContext] =
    NativeFunction(
      "lastIndexOf",
      Map[StdLibVersion, Long](V1 -> 20L, V2 -> 20L, V3 -> 20L, V4 -> 3L, V5 -> 3L),
      LASTINDEXOF,
      optionLong,
      ("str", STRING),
      ("substr", STRING)
    ) {
      case CONST_STRING(m) :: CONST_STRING(sub) :: Nil =>
        Right({
          val i = m.lastIndexOf(sub)
          if (sub.isWellFormed && i != -1) {
            CONST_LONG(m.codePointCount(0, i).toLong)
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

  lazy val lastIndexOfWithOffsetFixed: BaseFunction[NoContext] =
    NativeFunction(
      "lastIndexOf",
      Map[StdLibVersion, Long](V1 -> 20L, V2 -> 20L, V3 -> 20L, V4 -> 3L, V5 -> 3L),
      LASTINDEXOFN,
      optionLong,
      ("str", STRING),
      ("substr", STRING),
      ("offset", LONG)
    ) {
      case CONST_STRING(m) :: CONST_STRING(sub) :: CONST_LONG(off) :: Nil =>
        Right(if (off >= 0) {
          val offset = Math.min(off, m.codePointCount(0, m.length)).toInt
          val i      = m.lastIndexOf(sub, m.offsetByCodePoints(0, offset))
          if (sub.isWellFormed && i != -1) {
            CONST_LONG(m.codePointCount(0, i).toLong)
          } else {
            unit
          }
        } else {
          unit
        })
      case xs => notImplemented[Id, EVALUATED]("lastIndexOf(str: String, substr: String, offset: Int)", xs)
    }

  lazy val splitStr: BaseFunction[NoContext] =
    NativeFunction("split", Map(V3 -> 100L, V4 -> 75L, V5 -> 75L, V6 -> 51L), SPLIT, listString, ("str", STRING), ("separator", STRING)) {
      case CONST_STRING(str) :: CONST_STRING(sep) :: Nil =>
        ARR(split(str, sep, unicode = false).toIndexedSeq, limited = true)
      case xs =>
        notImplemented[Id, EVALUATED]("split(str: String, separator: String)", xs)
    }

  def splitStrFixedF(id: Short, inputLimit: Int, outputLimit: Int, v6Complexity: Long): BaseFunction[NoContext] = {
    val name = if (id == SPLIT) "split" else s"split_${v6Complexity}C"
    NativeFunction(name, Map(V3 -> 100L, V4 -> 75L, V5 -> 75L, V6 -> v6Complexity), id, listString, ("str", STRING), ("separator", STRING)) {
      case (s @ CONST_STRING(str)) :: CONST_STRING(sep) :: Nil =>
        if (s.weight > inputLimit) Left(s"Input string size = ${s.weight} bytes exceeds limit = $inputLimit for $name")
        else {
          val result = split(str, sep, unicode = true).toIndexedSeq
          if (result.size > outputLimit)
            Left(s"Output list size = ${result.size} exceeds limit = $outputLimit for $name")
          else
            ARR(result, limited = true)
        }
      case xs =>
        notImplemented[Id, EVALUATED](s"$name(str: String, separator: String)", xs)
    }
  }

  val splitStrFixed = splitStrFixedF(SPLIT, DataEntryValueMax, MaxListLengthV4, 51)
  val splitStrV6    = splitStrFixedF(SPLIT, 500, 20, 1)
  val splitStr4C    = splitStrFixedF(SPLIT4C, 6000, 100, 4)
  val splitStr51C   = splitStrFixedF(SPLIT51C, DataEntryValueMax, MaxListLengthV4, 51)

  private def split(str: String, sep: String, unicode: Boolean): Iterable[CONST_STRING] = {
    if (str == "") listWithEmptyStr
    else if (unicode && !sep.isWellFormed) List(CONST_STRING(str).explicitGet())
    else if (sep == "")
      if (unicode) {
        (1 to str.codePointCount(0, str.length))
          .map(i => CONST_STRING(str.substring(str.offsetByCodePoints(0, i - 1), str.offsetByCodePoints(0, i))).explicitGet())
      } else {
        (1 to str.length)
          .map(i => CONST_STRING(String.valueOf(str.charAt(i - 1))).explicitGet())
      }
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

  def makeStringF(id: Short, complexityV6: Long, inputLimit: Int, outputLimit: Int, rejectNonStrings: Boolean): BaseFunction[NoContext] = {
    val name = if (id == MAKESTRING) "makeString" else s"makeString_${complexityV6}C"
    NativeFunction(name, Map(V4 -> 30L, V5 -> 30L, V6 -> complexityV6), id, STRING, ("list", LIST(STRING)), ("separator", STRING)) {
      case (arr: ARR) :: CONST_STRING(separator) :: Nil =>
        if (arr.xs.length > inputLimit) Left(s"Input list size = ${arr.xs.length} for $name should not exceed $inputLimit")
        else {
          val separatorStringSize =
            if (arr.xs.length > 1) (arr.xs.length - 1) * separator.length
            else 0
          val expectedStringSize = arr.elementsWeightSum + separatorStringSize
          if (rejectNonStrings && arr.xs.exists(!_.isInstanceOf[CONST_STRING]))
            Left("makeString only accepts strings")
          else if (expectedStringSize > outputLimit)
            Left(s"Constructing string size = $expectedStringSize bytes will exceed $outputLimit")
          else
            CONST_STRING(arr.xs.mkString(separator))
        }
      case xs =>
        notImplemented[Id, EVALUATED](s"$name(list: List[String], separator: String)", xs)
    }
  }

  val makeString: BaseFunction[NoContext]        = makeStringF(MAKESTRING, 11, MaxListLengthV4, DataEntryValueMax, rejectNonStrings = false)
  val makeString_V6: BaseFunction[NoContext]     = makeStringF(MAKESTRING, 1, 70, 500, rejectNonStrings = true)
  val makeString_V6_2C: BaseFunction[NoContext]  = makeStringF(MAKESTRING2C, 2, 100, 6000, rejectNonStrings = true)
  val makeString_V6_11C: BaseFunction[NoContext] = makeStringF(MAKESTRING11C, 11, MaxListLengthV4, DataEntryValueMax, rejectNonStrings = true)

  lazy val contains: BaseFunction[NoContext] =
    UserFunction(
      "contains",
      3,
      BOOLEAN,
      ("@source", STRING),
      ("@substr", STRING)
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
    NativeFunction("parseInt", Map[StdLibVersion, Long](V1 -> 20L, V2 -> 20L, V3 -> 20L, V4 -> 2L), PARSEINT, optionLong, ("str", STRING)) {
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
      body: (EVALUATED, EVALUATED) => Either[ExecutionError, EVALUATED]
  ): BaseFunction[NoContext] =
    NativeFunction(opsToFunctions(op), complexity, func, r, ("a", t), ("b", t)) {
      case a :: b :: Nil => body(a, b)
      case xs            => notImplemented[Id, EVALUATED](s"${opsToFunctions(op)}(a: ${t.toString}, b: ${t.toString})", xs)
    }

  def createRawOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short, complexity: Map[StdLibVersion, Long])(
      body: (EVALUATED, EVALUATED) => Either[ExecutionError, EVALUATED]
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

  def bigIntConditionOp(op: BinaryOperation, func: Short)(
      body: (BigInt, BigInt) => Boolean
  ): BaseFunction[NoContext] =
    NativeFunction(opsToFunctions(op), Map(V5 -> 8L, V6 -> 8L, V7 -> 8L, V8 -> 1L), func, BOOLEAN, ("a", BIGINT), ("b", BIGINT)) {
      case CONST_BIGINT(a) :: CONST_BIGINT(b) :: Nil => Try(body(a, b)).toEither.bimap(_.getMessage, CONST_BOOLEAN.apply)
      case xs                                        => notImplemented[Id, EVALUATED](s"${opsToFunctions(op)}(a: BIGINT, b: BIGINT)", xs)
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

  lazy val listBigIntMax: BaseFunction[NoContext] =
    NativeFunction("max", Map(V5 -> 192L, V6 -> 192L, V7 -> 192L, V8 -> 6L), MAX_LIST_BIGINT, BIGINT, ("list", PARAMETERIZEDLIST(BIGINT))) {
      case ARR(list) :: Nil =>
        Either.cond(
          list.nonEmpty,
          list.asInstanceOf[IndexedSeq[CONST_BIGINT]].max,
          "Can't find max for empty list"
        )
      case xs =>
        notImplemented[Id, EVALUATED]("max(list: List[BigInt])", xs)
    }

  lazy val listBigIntMin: BaseFunction[NoContext] =
    NativeFunction("min", Map(V5 -> 192L, V6 -> 192L, V7 -> 192L, V8 -> 6L), MIN_LIST_BIGINT, BIGINT, ("list", PARAMETERIZEDLIST(BIGINT))) {
      case ARR(list) :: Nil =>
        Either.cond(
          list.nonEmpty,
          list.asInstanceOf[IndexedSeq[CONST_BIGINT]].min,
          "Can't find min for empty list"
        )
      case xs =>
        notImplemented[Id, EVALUATED]("min(list: List[BigInt])", xs)
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
      Map(V4 -> 7L, V5 -> 7L, V6 -> 7L, V7 -> 7L, V8 -> 4L),
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

  private val listReplaceByIndex: BaseFunction[NoContext] =
    NativeFunction(
      "replaceByIndex",
      4,
      REPLACE_BY_INDEX_OF_LIST,
      PARAMETERIZEDLIST(TYPEPARAM('T')),
      ("list", PARAMETERIZEDLIST(TYPEPARAM('T'))),
      ("index", LONG),
      ("element", TYPEPARAM('T'))
    ) {
      case ARR(list) :: CONST_LONG(index) :: element :: Nil =>
        if (list.isEmpty)
          Left("Can't replace an element in empty list")
        else if (index < 0)
          Left(s"Index of the replacing element should be positive, but $index was passed")
        else if (index >= list.size)
          Left(s"Index of the replacing element should be lower than list size = ${list.length}, but $index was passed")
        else
          ARR(list.updated(index.toInt, element), limited = true)
      case xs =>
        notImplemented[Id, EVALUATED]("replaceByIndex(list: List[T], index: Int, element: T)", xs)
    }

  private[v1] def genericListIndexOf(
      element: EVALUATED,
      indexOf: EVALUATED => Int,
      indexWhere: (EVALUATED => Boolean) => Int
  ): Either[ExecutionError, EVALUATED] =
    if (element.weight <= MaxCmpWeight)
      Right {
        val i = indexOf(element)
        if (i != -1) CONST_LONG(i.toLong) else unit
      }
    else
      Try {
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
      s"${compiler.TuplePrefix}$resultSize",
      1,
      (CREATE_TUPLE + resultSize - 2).toShort,
      PARAMETERIZEDTUPLE(typeParams),
      typeParams.mapWithIndex { case (typeParam, i) => (s"element${i + 1}", typeParam) }*
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

  def pow(roundTypes: UNION, useNewPrecision: Boolean): BaseFunction[NoContext] = {
    NativeFunction(
      "pow",
      Map(V3 -> 100L, V4 -> 100L, V5 -> 100L, V6 -> 28L),
      POW,
      LONG,
      ("base", LONG),
      ("bp", LONG),
      ("exponent", LONG),
      ("ep", LONG),
      ("rp", LONG),
      ("round", roundTypes)
    ) {
      case CONST_LONG(b) :: CONST_LONG(bp) :: CONST_LONG(e) :: CONST_LONG(ep) :: CONST_LONG(rp) :: round :: Nil =>
        if (
          bp < 0
          || bp > 8
          || ep < 0
          || ep > 8
          || rp < 0
          || rp > 8
        ) {
          Left("pow: scale out of range 0-8")
        } else {
          global.pow(b, bp.toInt, e, ep.toInt, rp.toInt, Rounding.byValue(round), useNewPrecision).map(CONST_LONG.apply).leftMap(CommonError(_))
        }
      case xs => notImplemented[Id, EVALUATED]("pow(base: Int, bp: Int, exponent: Int, ep: Int, rp: Int, round: Rounds)", xs)
    }
  }

  val sqrtInt: BaseFunction[NoContext] =
    UserFunction("sqrt", 2, LONG, ("@number", LONG), ("@precision", LONG), ("@resultPrecision", LONG), ("@round", UNION(fromV5RoundTypes))) {
      FUNCTION_CALL(
        Native(POW),
        List(
          REF("@number"),
          REF("@precision"),
          CONST_LONG(5),
          CONST_LONG(1),
          REF("@resultPrecision"),
          REF("@round")
        )
      )
    }

  def log(roundTypes: UNION): BaseFunction[NoContext] = {
    NativeFunction("log", 100, LOG, LONG, ("base", LONG), ("bp", LONG), ("exponent", LONG), ("ep", LONG), ("rp", LONG), ("round", roundTypes)) {
      case CONST_LONG(b) :: CONST_LONG(bp) :: CONST_LONG(e) :: CONST_LONG(ep) :: CONST_LONG(rp) :: round :: Nil =>
        if (
          bp < 0
          || bp > 8
          || ep < 0
          || ep > 8
          || rp < 0
          || rp > 8
        ) {
          Left(CommonError("log: scale out of range 0-8"))
        } else {
          global.log(b, bp, e, ep, rp, Rounding.byValue(round)).map(CONST_LONG.apply).leftMap(CommonError(_))
        }
      case xs => notImplemented[Id, EVALUATED]("log(exponent: Int, ep: Int, base: Int, bp: Int, rp: Int, round: Rounds)", xs)
    }
  }

  def powBigInt(roundTypes: UNION, useNewPrecision: Boolean): BaseFunction[NoContext] =
    NativeFunction(
      "pow",
      Map(V5 -> 200L, V6 -> 270L),
      POW_BIGINT,
      BIGINT,
      ("base", BIGINT),
      ("bp", LONG),
      ("exponent", BIGINT),
      ("ep", LONG),
      ("rp", LONG),
      ("round", roundTypes)
    ) {
      case CONST_BIGINT(b) :: CONST_LONG(bp) :: CONST_BIGINT(e) :: CONST_LONG(ep) :: CONST_LONG(rp) :: round :: Nil =>
        if (
          bp < 0
          || bp > 18
          || ep < 0
          || ep > 18
          || rp < 0
          || rp > 18
        ) {
          Left("pow: scale out of range 0-18")
        } else {
          global
            .powBigInt(b, bp, e, ep, rp, Rounding.byValue(round), useNewPrecision)
            .filterOrElse(v => v <= BigIntMax && v >= BigIntMin, "Result out of 512-bit range")
            .bimap(e => s"$e on BigInt pow calculation", CONST_BIGINT.apply)
        }
      case xs => notImplemented[Id, EVALUATED]("pow(base: BigInt, bp: Int, exponent:Big Int, ep: Int, rp: Int, round: Rounds)", xs)
    }

  val sqrtBigInt: BaseFunction[NoContext] =
    UserFunction(
      "sqrt",
      "sqrtBigInt",
      5,
      BIGINT,
      ("@number", BIGINT),
      ("@precision", LONG),
      ("@resultPrecision", LONG),
      ("@round", UNION(fromV5RoundTypes))
    ) {
      FUNCTION_CALL(
        Native(POW_BIGINT),
        List(
          REF("@number"),
          REF("@precision"),
          CONST_BIGINT(5),
          CONST_LONG(1),
          REF("@resultPrecision"),
          REF("@round")
        )
      )
    }

  def logBigInt(roundTypes: UNION): BaseFunction[NoContext] =
    NativeFunction(
      "log",
      200,
      LOG_BIGINT,
      BIGINT,
      ("base", BIGINT),
      ("bp", LONG),
      ("exponent", BIGINT),
      ("ep", LONG),
      ("rp", LONG),
      ("round", roundTypes)
    ) {
      case CONST_BIGINT(b) :: CONST_LONG(bp) :: CONST_BIGINT(e) :: CONST_LONG(ep) :: CONST_LONG(rp) :: round :: Nil =>
        val r =
          if (
            bp < 0
            || bp > 18
            || ep < 0
            || ep > 18
            || rp < 0
            || rp > 18
          ) {
            Left("Scale out of range 0-18")
          } else {
            global.logBigInt(b, bp, e, ep, rp, Rounding.byValue(round)).map(CONST_BIGINT.apply)
          }
        r.leftMap(e => s"$e on BigInt log calculation")
      case xs => notImplemented[Id, EVALUATED]("log(exponent: BigInt, ep: Int, base:Big Int, bp: Int, rp: Int, round: Rounds)", xs)
    }

  val getListMedian: BaseFunction[NoContext] =
    NativeFunction("median", 20, MEDIAN_LIST, LONG, ("arr", PARAMETERIZEDLIST(LONG))) {
      case xs @ ARR(arr) :: Nil =>
        if (arr.headOption.forall(_.isInstanceOf[CONST_LONG])) {
          if (arr.nonEmpty)
            Right(CONST_LONG(global.median(arr.asInstanceOf[IndexedSeq[CONST_LONG]].map(_.t).toArray)))
          else
            Left(s"Can't find median for empty list")
        } else {
          notImplemented[Id, EVALUATED](s"median(arr: List[Int])", xs)
        }
      case xs => notImplemented[Id, EVALUATED](s"median(arr: List[Int])", xs)
    }

  val getBigIntListMedian: BaseFunction[NoContext] =
    NativeFunction(
      "median",
      Map(V5 -> 20 * 8L, V6 -> 20 * 8L, V7 -> 20 * 8L, V8 -> 35L),
      MEDIAN_LISTBIGINT,
      BIGINT,
      ("arr", PARAMETERIZEDLIST(BIGINT))
    ) {
      case xs @ ARR(arr) :: Nil =>
        if (arr.headOption.forall(_.isInstanceOf[CONST_BIGINT])) {
          if (arr.nonEmpty)
            Right(CONST_BIGINT(global.median(arr.asInstanceOf[IndexedSeq[CONST_BIGINT]].map(_.t).toArray)))
          else
            Left(s"Can't find median for empty list of BigInt")
        } else {
          notImplemented[Id, EVALUATED](s"median(arr: List[BigInt])", xs)
        }
      case xs => notImplemented[Id, EVALUATED](s"median(arr: List[BigInt])", xs)
    }

  val sizeTuple: BaseFunction[NoContext] = {
    val genericTupleType =
      (MinTupleSize to MaxTupleSize)
        .map(('A' to 'Z').take)
        .map(t => PARAMETERIZEDTUPLE(t.map(b => TYPEPARAM(b.toByte)).toList))
        .toList
    NativeFunction("size", 1, SIZE_TUPLE, LONG, ("tuple", PARAMETERIZEDUNION(genericTupleType))) {
      case CaseObj(`runtimeTupleType`, fields) :: Nil => Right(CONST_LONG(fields.size))
      case xs                                         => notImplemented[Id, EVALUATED](s"size(t: Tuple)", xs)
    }
  }

  private val nil: (String, (LIST, ContextfulVal[NoContext])) =
    (
      GlobalValNames.Nil,
      (LIST(NOTHING), ContextfulVal.pure[NoContext](ARR(IndexedSeq.empty[EVALUATED], EMPTYARR_WEIGHT, limited = false).explicitGet()))
    )

  private val commonVars: Map[String, (FINAL, ContextfulVal[NoContext])] =
    Map(
      (GlobalValNames.Unit, (UNIT, ContextfulVal.pure(unit)))
    )

  private val v1V2Vars: Map[String, (FINAL, ContextfulVal[NoContext])] = commonVars ++ Rounding.all.map(_.definition)
  private val v3V4Vars: Map[String, (FINAL, ContextfulVal[NoContext])] = v1V2Vars + nil
  private val v5Vars: Map[String, (FINAL, ContextfulVal[NoContext])]   = commonVars ++ Rounding.fromV5.map(_.definition) + nil

  private val commonTypes: Seq[REAL] =
    Seq(
      UNIT,
      LONG,
      BOOLEAN,
      BYTESTR,
      STRING
    )

  private val allRoundTypes: List[CASETYPEREF]         = Rounding.all.map(_.`type`)
  private lazy val fromV5RoundTypes: List[CASETYPEREF] = Rounding.fromV5.map(_.`type`)

  private val v1v2v3v4Types: Seq[REAL] = commonTypes ++ allRoundTypes
  private val v5Types: Seq[REAL]       = commonTypes ++ fromV5RoundTypes ++ Seq(BIGINT)

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
      sizeBytes,
      toBytesBoolean,
      toBytesLong,
      toBytesString,
      toStringBoolean,
      toStringLong,
      isDefined,
      throwWithMessage,
      _isInstanceOf,
      throwNoMessage
    ) ++ operators

  private val takeDropBytesBeforeV6 =
    Array(
      takeBytesBeforeV6,
      dropBytesBeforeV6,
      takeRightBytesBeforeV6,
      dropRightBytesBeforeV6
    )

  private val takeDropStringUnfixedBeforeV6 =
    Array(
      takeStringBeforeV6,
      dropStringBeforeV6,
      dropRightStringBeforeV6,
      takeRightStringBeforeV6
    )

  private val takeDropStringFixedBeforeV6 =
    Array(
      takeStringFixedBeforeV6,
      dropStringFixedBeforeV6,
      dropRightStringFixedBeforeV6,
      takeRightStringFixedBeforeV6
    )

  private val v1V2V3CommonFunctionsFixed =
    commonFunctions ++
      takeDropBytesBeforeV6 ++
      takeDropStringFixedBeforeV6 ++
      Array(
        extract,
        fraction(fixLimitCheck = false),
        sizeStringFixed
      )

  private val v1V2V3CommonFunctionsUnfixed =
    commonFunctions ++
      takeDropBytesBeforeV6 ++
      takeDropStringUnfixedBeforeV6 ++
      Array(
        extract,
        fraction(fixLimitCheck = false),
        sizeString
      )

  private def v1V2V3CommonFunctions(fixUnicodeFunctions: Boolean) =
    if (fixUnicodeFunctions) v1V2V3CommonFunctionsFixed else v1V2V3CommonFunctionsUnfixed

  private val fromV3Functions =
    Array(
      value,
      valueOrErrorMessage,
      toLong,
      toLongOffset,
      parseInt,
      parseIntVal
    )

  private def v3V4Functions(useNewPowPrecision: Boolean) =
    Array(
      pow(UNION(allRoundTypes), useNewPowPrecision),
      log(UNION(allRoundTypes))
    )

  private val v3FunctionsUnfixed =
    Array(
      indexOf,
      indexOfN,
      lastIndexOf,
      lastIndexOfWithOffset,
      splitStr
    )

  private val v3FunctionFixed =
    Array(
      indexOfFixed,
      indexOfNFixed,
      lastIndexOfFixed,
      lastIndexOfWithOffsetFixed,
      splitStrFixed
    )

  private def v3Functions(useNewPowPrecision: Boolean) =
    v1V2V3CommonFunctions(useNewPowPrecision) ++
      fromV3Functions ++
      v3V4Functions(useNewPowPrecision) ++
      Array(
        toUtf8String(reduceLimit = false),
        listConstructor(checkSize = false)
      ) ++ (if (useNewPowPrecision) v3FunctionFixed else v3FunctionsUnfixed)

  private val fromV4Functions =
    commonFunctions ++
      fromV3Functions ++
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
        listMax
      ) ++ (MinTupleSize to MaxTupleSize).map(i => createTupleN(i))

  private def v4FunctionsUnfixed =
    fromV4Functions ++
      takeDropBytesBeforeV6 ++
      takeDropStringUnfixedBeforeV6 ++
      v3V4Functions(useNewPowPrecision = false) ++
      Array(
        indexOf,
        indexOfN,
        lastIndexOf,
        lastIndexOfWithOffset,
        splitStr,
        sizeString,
        fraction(fixLimitCheck = false),
        makeString
      )

  private def v4FunctionsFixed =
    fromV4Functions ++
      takeDropBytesBeforeV6 ++
      takeDropStringFixedBeforeV6 ++
      v3V4Functions(useNewPowPrecision = true) ++
      Array(
        indexOfFixed,
        indexOfNFixed,
        lastIndexOfFixed,
        lastIndexOfWithOffsetFixed,
        splitStrFixed,
        sizeStringFixed,
        fraction(fixLimitCheck = false),
        makeString
      )

  private def v4Functions(useNewPowPrecision: Boolean) =
    if (useNewPowPrecision) v4FunctionsFixed else v4FunctionsUnfixed

  private def fromV5Functions(useNewPowPrecision: Boolean) =
    fromV4Functions ++
      Array(
        indexOfFixed,
        indexOfNFixed,
        lastIndexOfFixed,
        lastIndexOfWithOffsetFixed,
        sizeStringFixed,
        intToBigInt,
        bigIntToInt,
        bigIntToString,
        stringToBigInt,
        stringToBigIntOpt,
        bigIntToBytes,
        bytesToBigInt,
        bytesToBigIntLim,
        sumToBigInt,
        subToBigInt,
        mulToBigInt,
        divToBigInt,
        modToBigInt,
        geBigInt,
        gtBigInt,
        listBigIntMax,
        listBigIntMin,
        fractionBigInt,
        fractionBigIntRounds(UNION(fromV5RoundTypes)),
        negativeBigInt,
        getBigIntListMedian,
        powBigInt(UNION(fromV5RoundTypes), useNewPowPrecision),
        logBigInt(UNION(fromV5RoundTypes)),
        pow(UNION(fromV5RoundTypes), useNewPowPrecision),
        log(UNION(fromV5RoundTypes)),
        fraction(fixLimitCheck = true)
      )

  private def v5Functions(useNewPowPrecision: Boolean) =
    fromV5Functions(useNewPowPrecision) ++
      takeDropBytesBeforeV6 ++
      takeDropStringFixedBeforeV6 :+
      fractionIntRounds(UNION(fromV5RoundTypes)) :+
      makeString :+
      splitStrFixed

  private val v6Functions =
    fromV5Functions(true) ++
      Array(
        sizeTuple,
        makeString_V6,
        makeString_V6_2C,
        makeString_V6_11C,
        splitStrV6,
        splitStr4C,
        splitStr51C,
        sqrtInt,
        sqrtBigInt,
        fractionIntRoundsNative,
        takeBytesFromV6,
        dropBytesFromV6,
        dropRightBytesFromV6,
        takeRightBytesFromV6,
        dropStringFixedFromV6,
        takeStringFixedFromV6,
        dropRightStringFromV6,
        takeRightStringFromV6,
        _getType
      )

  private def v1V2Ctx(fixUnicodeFunctions: Boolean) =
    CTX[NoContext](
      v1v2v3v4Types,
      v1V2Vars,
      v1V2V3CommonFunctions(fixUnicodeFunctions)
    )

  private def v3Ctx(useNewPowPrecision: Boolean) =
    CTX[NoContext](
      v1v2v3v4Types,
      v3V4Vars,
      v3Functions(useNewPowPrecision)
    )

  private def v4Ctx(useNewPowPrecision: Boolean) =
    CTX[NoContext](
      v1v2v3v4Types,
      v3V4Vars,
      v4Functions(useNewPowPrecision)
    )

  private def v5Ctx(useNewPowPrecision: Boolean) =
    CTX[NoContext](
      v5Types,
      v5Vars,
      v5Functions(useNewPowPrecision)
    )

  private[this] val v6Ctx =
    CTX[NoContext](
      v5Types,
      v5Vars,
      v6Functions
    )

  private[this] val v8Ctx =
    CTX[NoContext](
      v5Types,
      v5Vars,
      v6Functions :+ listReplaceByIndex
    )

  def build(version: StdLibVersion, useNewPowPrecision: Boolean): CTX[NoContext] =
    version match {
      case V1 | V2 => v1V2Ctx(useNewPowPrecision)
      case V3      => v3Ctx(useNewPowPrecision)
      case V4      => v4Ctx(useNewPowPrecision)
      case V5      => v5Ctx(useNewPowPrecision)
      case V6 | V7 => v6Ctx
      case V8      => v8Ctx
    }
}
