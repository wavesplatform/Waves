package com.wavesplatform.lang.v1.evaluator.ctx.impl

import cats.data.EitherT
import com.wavesplatform.lang.v1.compiler.{CompilerContext, Types}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.{BOOLEAN, BYTEVECTOR, LONG, STRING, TYPEPLACEHOLDER, UNIT}
import com.wavesplatform.lang.v1.compiler.Types.TYPEPLACEHOLDER._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.parser.BinaryOperation
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import monix.eval.Coeval
import scodec.bits.ByteVector

import scala.util.Try

object PureContext {
  private val nothingCoeval: Coeval[Either[String, Nothing]] = Coeval.defer(Coeval(Left("explicit contract termination")))
  val err                                                    = LazyVal(EitherT(nothingCoeval))
  val errRef                                                 = "throw"

  val fraction: BaseFunction = NativeFunction("fraction", 1, FRACTION, LONG, "value" -> LONG, "numerator" -> LONG, "denominator" -> LONG) {
    case (v: Long) :: (n: Long) :: (d: Long) :: Nil =>
      val result = BigInt(v) * n / d
      for {
        _ <- Either.cond(result < Long.MaxValue, (), s"Long overflow: value `$result` greater than 2^63-1")
        _ <- Either.cond(result > Long.MinValue, (), s"Long overflow: value `$result` less than -2^63-1")
      } yield result.toLong
    case _ => ???
  }

  val _isInstanceOf: BaseFunction = NativeFunction("_isInstanceOf", 1, ISINSTANCEOF, BOOLEAN, "obj" -> TYPEPARAM('T'), "of" -> STRING) {
    case (p: Boolean) :: ("Boolean") :: Nil => Right(true)
    case (p: String) :: ("String") :: Nil   => Right(true)
    case (p: Long) :: ("Int") :: Nil        => Right(true)
    case (()) :: ("Unit") :: Nil            => Right(true)
    case (p: CaseObj) :: (s: String) :: Nil => Right(p.caseType.name == s)
    case _                                  => Right(false)
  }

  val sizeBytes: BaseFunction = NativeFunction("size", 1, SIZE_BYTES, LONG, "byteVector" -> BYTEVECTOR) {
    case (bv: ByteVector) :: Nil => Right(bv.size)
    case xs                      => notImplemented("size(byte[])", xs)
  }

  val sizeString: BaseFunction = NativeFunction("size", 1, SIZE_STRING, LONG, "xs" -> STRING) {
    case (bv: String) :: Nil => Right(bv.length.toLong)
    case xs                  => notImplemented("size(String)", xs)
  }

  val takeBytes: BaseFunction = NativeFunction("take", 1, TAKE_BYTES, BYTEVECTOR, "xs" -> BYTEVECTOR, "number" -> LONG) {
    case (xs: ByteVector) :: (number: Long) :: Nil => Right(xs.take(number))
    case xs                                        => notImplemented("take(xs: byte[], number: Long)", xs)
  }

  val dropBytes: BaseFunction = NativeFunction("drop", 1, DROP_BYTES, BYTEVECTOR, "xs" -> BYTEVECTOR, "number" -> LONG) {
    case (xs: ByteVector) :: (number: Long) :: Nil => Right(xs.drop(number))
    case xs                                        => notImplemented("drop(xs: byte[], number: Long)", xs)
  }

  val takeRightBytes: BaseFunction = UserFunction("takeRightBytes", 1, BYTEVECTOR, "xs" -> BYTEVECTOR, "number" -> LONG) {
    case (xs: EXPR) :: (number: EXPR) :: Nil =>
      FUNCTION_CALL(
        FunctionHeader.Native(DROP_BYTES),
        List(
          xs,
          FUNCTION_CALL(
            FunctionHeader.Native(SUB_LONG),
            List(
              FUNCTION_CALL(FunctionHeader.Native(SIZE_BYTES), List(xs)),
              number
            )
          )
        )
      )
    case xs => ???
  }

  val dropRightBytes: BaseFunction = UserFunction("dropRightBytes", 1, BYTEVECTOR, "xs" -> BYTEVECTOR, "number" -> LONG) {
    case (xs: EXPR) :: (number: EXPR) :: Nil =>
      FUNCTION_CALL(
        FunctionHeader.Native(TAKE_BYTES),
        List(
          xs,
          FUNCTION_CALL(
            FunctionHeader.Native(SUB_LONG),
            List(
              FUNCTION_CALL(FunctionHeader.Native(SIZE_BYTES), List(xs)),
              number
            )
          )
        )
      )
    case xs => notImplemented("dropRight(xs: byte[], number: Long)", xs)
  }

  private def trimLongToInt(x: Long): Int = Math.toIntExact(Math.max(Math.min(x, Int.MaxValue), Int.MinValue))

  val takeString: BaseFunction = NativeFunction("take", 1, TAKE_STRING, STRING, "xs" -> STRING, "number" -> LONG) {
    case (xs: String) :: (number: Long) :: Nil => Right(xs.take(trimLongToInt(number)))
    case xs                                    => notImplemented("take(xs: String, number: Long)", xs)
  }

  val dropString: BaseFunction = NativeFunction("drop", 1, DROP_STRING, STRING, "xs" -> STRING, "number" -> LONG) {
    case (xs: String) :: (number: Long) :: Nil => Right(xs.drop(trimLongToInt(number)))
    case xs                                    => notImplemented("drop(xs: String, number: Long)", xs)
  }

  val takeRightString: BaseFunction = UserFunction("takeRight", 1, STRING, "xs" -> STRING, "number" -> LONG) {
    case (xs: EXPR) :: (number: EXPR) :: Nil =>
      FUNCTION_CALL(
        FunctionHeader.Native(DROP_STRING),
        List(
          xs,
          FUNCTION_CALL(
            FunctionHeader.Native(SUB_LONG),
            List(
              FUNCTION_CALL(FunctionHeader.Native(SIZE_STRING), List(xs)),
              number
            )
          )
        )
      )
    case xs => notImplemented("takeRight(xs: String, number: Long)", xs)
  }

  val dropRightString: BaseFunction = UserFunction("dropRight", 1, STRING, "xs" -> STRING, "number" -> LONG) {
    case (xs: EXPR) :: (number: EXPR) :: Nil =>
      FUNCTION_CALL(
        FunctionHeader.Native(TAKE_STRING),
        List(
          xs,
          FUNCTION_CALL(
            FunctionHeader.Native(SUB_LONG),
            List(
              FUNCTION_CALL(FunctionHeader.Native(SIZE_STRING), List(xs)),
              number
            )
          )
        )
      )
    case xs => notImplemented("dropRight(xs: String, number: Long)", xs)
  }

  private def createOp(op: BinaryOperation, t: TYPEPLACEHOLDER, r: TYPEPLACEHOLDER, func: Short)(body: (Any, Any) => Any): BaseFunction =
    NativeFunction(opsToFunctions(op), 1, func, r, "a" -> t, "b" -> t) {
      case a :: b :: Nil => Right(body(a, b))
      case _             => ???
    }

  val getElement: BaseFunction =
    NativeFunction("getElement", 2, GET_LIST, TYPEPARAM('T'), "arr" -> PARAMETERIZEDLIST(TYPEPARAM('T')), "pos" -> LONG) {
      case (arr: IndexedSeq[_]) :: (pos: Long) :: Nil => Try(arr(pos.toInt)).toEither.left.map(_.toString)
      case _                                          => ???
    }

  val getListSize: BaseFunction = NativeFunction("size", 2, SIZE_LIST, LONG, "arr" -> PARAMETERIZEDLIST(TYPEPARAM('T'))) {
    case (arr: IndexedSeq[_]) :: Nil => Right(arr.size.toLong)
    case _                           => ???
  }

  val uMinus: BaseFunction = NativeFunction("-", 1, MINUS_LONG, LONG, "n" -> LONG) {
    case (n: Long) :: Nil => Right(Math.negateExact(n))
    case _                => ???
  }

  val uNot: BaseFunction = NativeFunction("!", 1, NOT_BOOLEAN, BOOLEAN, "p" -> BOOLEAN) {
    case (p: Boolean) :: Nil => Right(!p)
    case _                   => ???
  }

  private def createTryOp(op: BinaryOperation, t: TYPEPLACEHOLDER, r: TYPEPLACEHOLDER, func: Short)(body: (Any, Any) => Any): BaseFunction =
    NativeFunction(opsToFunctions(op), 1, func, r, "a" -> t, "b" -> t) {
      case a :: b :: Nil =>
        try {
          Right(body(a, b))
        } catch {
          case e: Throwable => Left(e.getMessage())
        }
      case _ => ???
    }

  val mulLong: BaseFunction   = createTryOp(MUL_OP, LONG, LONG, MUL_LONG)((a, b) => Math.multiplyExact(a.asInstanceOf[Long], b.asInstanceOf[Long]))
  val divLong: BaseFunction   = createTryOp(DIV_OP, LONG, LONG, DIV_LONG)((a, b) => Math.floorDiv(a.asInstanceOf[Long], b.asInstanceOf[Long]))
  val modLong: BaseFunction   = createTryOp(MOD_OP, LONG, LONG, MOD_LONG)((a, b) => Math.floorMod(a.asInstanceOf[Long], b.asInstanceOf[Long]))
  val sumLong: BaseFunction   = createTryOp(SUM_OP, LONG, LONG, SUM_LONG)((a, b) => Math.addExact(a.asInstanceOf[Long], b.asInstanceOf[Long]))
  val subLong: BaseFunction   = createTryOp(SUB_OP, LONG, LONG, SUB_LONG)((a, b) => Math.subtractExact(a.asInstanceOf[Long], b.asInstanceOf[Long]))
  val sumString: BaseFunction = createOp(SUM_OP, STRING, STRING, SUM_STRING)((a, b) => a.asInstanceOf[String] + b.asInstanceOf[String])
  val sumByteVector: BaseFunction =
    createOp(SUM_OP, BYTEVECTOR, BYTEVECTOR, SUM_BYTES)((a, b) => ByteVector.concat(Seq(a.asInstanceOf[ByteVector], b.asInstanceOf[ByteVector])))
  val ge: BaseFunction = createOp(GE_OP, LONG, BOOLEAN, GE_LONG)((a, b) => a.asInstanceOf[Long] >= b.asInstanceOf[Long])
  val gt: BaseFunction = createOp(GT_OP, LONG, BOOLEAN, GT_LONG)((a, b) => a.asInstanceOf[Long] > b.asInstanceOf[Long])

  val eq: BaseFunction =
    NativeFunction(EQ_OP.func, 1, EQ, BOOLEAN, "a" -> TYPEPARAM('T'), "b" -> TYPEPARAM('T')) {
      case a :: b :: Nil => Right(a == b)
      case _             => ???
    }

  val ne: BaseFunction =
    UserFunction(NE_OP.func, 1, BOOLEAN, "a" -> TYPEPARAM('T'), "b" -> TYPEPARAM('T')) {
      case a :: b :: Nil => FUNCTION_CALL(FunctionHeader.Native(NOT_BOOLEAN), List(FUNCTION_CALL(FunctionHeader.Native(EQ), List(a, b))))
      case _             => ???
    }

  val isDefined: BaseFunction =
    UserFunction("isDefined", 1, BOOLEAN, "a" -> PARAMETERIZEDUNION(List(TYPEPARAM('T'), UNIT))) {
      case a :: Nil => FUNCTION_CALL(ne.header, List(a, REF("unit")))
      case _        => ???
    }

  val extract: BaseFunction =
    UserFunction("extract", 1, TYPEPARAM('T'), "a" -> PARAMETERIZEDUNION(List(TYPEPARAM('T'), UNIT))) {
      case a :: Nil => IF(FUNCTION_CALL(FunctionHeader.Native(EQ), List(a, REF("unit"))), REF("throw"), a)
      case _        => ???
    }

  val operators: Seq[BaseFunction] = Seq(
    mulLong,
    divLong,
    modLong,
    sumLong,
    subLong,
    sumString,
    sumByteVector,
    eq,
    ne,
    ge,
    gt,
    getElement,
    getListSize,
    uMinus,
    uNot
  )

  private val vars: Map[String, (Types.TYPE, LazyVal)] = Map((errRef, (Types.NOTHING, err)), ("unit", (Types.UNIT, LazyVal(EitherT.pure(())))))
  private val functions = Seq(
    fraction,
    sizeBytes,
    takeBytes,
    dropBytes,
    takeRightBytes,
    dropRightBytes,
    sizeString,
    takeString,
    dropString,
    takeRightString,
    dropRightString,
    _isInstanceOf,
    isDefined,
    extract
  ) ++ operators

  lazy val ctx = CTX(
    Seq(
      new DefinedType { val name = "Unit"; val typeRef    = Types.UNIT; val fields    = List.empty },
      new DefinedType { val name = "Int"; val typeRef     = Types.LONG; val fields    = List.empty },
      new DefinedType { val name = "Boolean"; val typeRef = Types.BOOLEAN; val fields = List.empty },
      new DefinedType { val name = "String"; val typeRef  = Types.STRING; val fields  = List.empty }
    ),
    vars,
    functions
  )
  lazy val evalContext                      = ctx.evaluationContext
  lazy val compilerContext: CompilerContext = ctx.compilerContext

  def fromOption[T](v: Option[T]): Any = {
    v.getOrElse((): Any)
  }
}
