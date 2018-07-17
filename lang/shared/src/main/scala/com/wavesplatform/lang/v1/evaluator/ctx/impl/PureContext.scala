package com.wavesplatform.lang.v1.evaluator.ctx.impl

import cats.data.EitherT
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.compiler.{CompilerContext, Types}
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.parser.BinaryOperation
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import monix.eval.Coeval
import scodec.bits.ByteVector

import scala.util.Try

object PureContext {
  private val nothingCoeval: Coeval[Either[String, Nothing]] = Coeval.defer(Coeval(Left("explicit contract termination")))
  val err                                                    = LazyVal(EitherT(nothingCoeval))
  val errRef                                                 = "throw"

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
    UserFunction(NE_OP.func, BOOLEAN, "a" -> TYPEPARAM('T'), "b" -> TYPEPARAM('T')) {
      case a :: b :: Nil => FUNCTION_CALL(uNot, List(FUNCTION_CALL(eq, List(a, b))))
      case _             => ???
    }

  val isDefined: BaseFunction =
    UserFunction("isDefined", BOOLEAN, "a" -> PARAMETERIZEDUNION(List(TYPEPARAM('T'), UNIT))) {
      case a :: Nil => FUNCTION_CALL(ne, List(a, REF("unit")))
      case _        => ???
    }

  val extract: BaseFunction =
    UserFunction("extract", TYPEPARAM('T'), "a" -> PARAMETERIZEDUNION(List(TYPEPARAM('T'), UNIT))) {
      case a :: Nil => IF(FUNCTION_CALL(eq, List(a, REF("unit"))), REF("throw"), a)
      case _        => ???
    }

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

  val dropRightBytes: BaseFunction = UserFunction("dropRight", "dropRightBytes", BYTEVECTOR, "xs" -> BYTEVECTOR, "number" -> LONG) {
    case (xs: EXPR) :: (number: EXPR) :: Nil =>
      FUNCTION_CALL(
        takeBytes,
        List(
          xs,
          FUNCTION_CALL(
            subLong,
            List(
              FUNCTION_CALL(sizeBytes, List(xs)),
              number
            )
          )
        )
      )
    case xs => notImplemented("dropRight(xs: byte[], number: Long)", xs)
  }

  val takeRightBytes: BaseFunction = UserFunction("takeRight", "takeRightBytes", BYTEVECTOR, "xs" -> BYTEVECTOR, "number" -> LONG) {
    case (xs: EXPR) :: (number: EXPR) :: Nil =>
      FUNCTION_CALL(
        dropBytes,
        List(
          xs,
          FUNCTION_CALL(
            subLong,
            List(
              FUNCTION_CALL(sizeBytes, List(xs)),
              number
            )
          )
        )
      )
    case xs => ???
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

  val takeRightString: BaseFunction = UserFunction("takeRight", STRING, "xs" -> STRING, "number" -> LONG) {
    case (xs: EXPR) :: (number: EXPR) :: Nil =>
      FUNCTION_CALL(
        dropString,
        List(
          xs,
          FUNCTION_CALL(
            subLong,
            List(
              FUNCTION_CALL(sizeString, List(xs)),
              number
            )
          )
        )
      )
    case xs => notImplemented("takeRight(xs: String, number: Long)", xs)
  }

  val dropRightString: BaseFunction = UserFunction("dropRight", STRING, "xs" -> STRING, "number" -> LONG) {
    case (xs: EXPR) :: (number: EXPR) :: Nil =>
      FUNCTION_CALL(
        takeString,
        List(
          xs,
          FUNCTION_CALL(
            subLong,
            List(
              FUNCTION_CALL(sizeString, List(xs)),
              number
            )
          )
        )
      )
    case xs => notImplemented("dropRight(xs: String, number: Long)", xs)
  }

  private def createOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short)(body: (Any, Any) => Any): BaseFunction =
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

  val uMinus: BaseFunction = UserFunction("-", LONG, "n" -> LONG) {
    case n :: Nil => FUNCTION_CALL(subLong, List(CONST_LONG(0), n))
    case _        => ???
  }

  val uNot: BaseFunction = UserFunction("!", BOOLEAN, "p" -> BOOLEAN) {
    case p :: Nil => IF(FUNCTION_CALL(eq, List(p, FALSE)), TRUE, FALSE)
    case _        => ???
  }

  private def createTryOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short)(body: (Any, Any) => Any): BaseFunction =
    NativeFunction(opsToFunctions(op), 1, func, r, "a" -> t, "b" -> t) {
      case a :: b :: Nil =>
        try {
          Right(body(a, b))
        } catch {
          case e: Throwable => Left(e.getMessage)
        }
      case _ => ???
    }

  private val operators: Seq[BaseFunction] = Seq(
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

  private val vars: Map[String, (Types.FINAL, LazyVal)] = Map((errRef, (Types.NOTHING, err)), ("unit", (Types.UNIT, LazyVal(EitherT.pure(())))))
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
      new DefinedType { val name = "Unit"; val typeRef       = Types.UNIT       },
      new DefinedType { val name = "Int"; val typeRef        = Types.LONG       },
      new DefinedType { val name = "Boolean"; val typeRef    = Types.BOOLEAN    },
      new DefinedType { val name = "ByteVector"; val typeRef = Types.BYTEVECTOR },
      new DefinedType { val name = "String"; val typeRef     = Types.STRING     }
    ),
    vars,
    functions
  )
  lazy val evalContext: EvaluationContext   = ctx.evaluationContext
  lazy val compilerContext: CompilerContext = ctx.compilerContext

  def fromOption[T](v: Option[T]): Any = {
    v.getOrElse((): Any)
  }
}
