package com.wavesplatform.lang.v1.evaluator.ctx.impl

import cats.data.EitherT
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import com.wavesplatform.lang.v1.compiler.CompilerContext
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.parser.BinaryOperation
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import monix.eval.Coeval
import scodec.bits.ByteVector

import scala.util.Try

object PureContext {
  private val noneCoeval: Coeval[Either[String, Unit]] = Coeval.evalOnce(Right( () ))
  private val nothingCoeval: Coeval[Either[String, Nothing]]      = Coeval.defer(Coeval(Right(throw new Exception("explicit contract termination"))))

  val none: LazyVal = LazyVal(EitherT(noneCoeval).subflatMap(Right(_: Unit))) // IDEA HACK
  val err           = LazyVal(EitherT(nothingCoeval))
  val errRef        = "throw"

  val fraction: BaseFunction = NativeFunction("fraction", 1, FRACTION, LONG, "value" -> LONG, "numerator" -> LONG, "denominator" -> LONG) {
    case (v: Long) :: (n: Long) :: (d: Long) :: Nil =>
      val result = BigInt(v) * n / d
      for {
        _ <- Either.cond(result < Long.MaxValue, (), s"Long overflow: value `$result` greater than 2^63-1")
        _ <- Either.cond(result > Long.MinValue, (), s"Long overflow: value `$result` less than -2^63-1")
      } yield result.toLong
    case _ => ???
  }

  val extract: NativeFunction = NativeFunction.create("extract", 1L, fromNonable(TYPEPARAM('T')) _, List(("opt", TYPEPARAM('T'):TYPEPLACEHOLDER)), EXTRACT) {
    case () :: Nil    => Left("Extract from empty option")
    case v :: Nil => Right(v)
    case _              => ???
  }

  val some: NativeFunction = NativeFunction.create("Some", 1L, asNonable(TYPEPARAM('T')) _, List(("obj", TYPEPARAM('T'):TYPEPLACEHOLDER)), SOME) {
    case v :: Nil => Right(v)
    case _        => ???
  }

  val isDefined: NativeFunction = NativeFunction("isDefined", 1, ISDEFINED, BOOLEAN, ("opt" -> (TYPEPARAM('T'):TYPEPLACEHOLDER))) {
    case (()) :: Nil => Right(false)
    case _           => Right(true)
  }

  val _isInstanceOf: BaseFunction = NativeFunction("_isInstanceOf", 1, ISINSTANCEOF, BOOLEAN, "obj" -> TYPEPARAM('T'), "of" -> STRING) {
    case (p: Boolean) :: ("Boolean") :: Nil => Right(true)
    case (p: String) :: ("String") :: Nil => Right(true)
    case (p: Long) :: ("Int") :: Nil => Right(true)
    case (()) :: ("Unit") :: Nil => Right(true)
    case (p: CaseObj) :: (s: String) :: Nil => Right(p.caseType.name == s)
    case _                                  => Right(false)
  }

  val size: BaseFunction = NativeFunction("size", 1, SIZE_BYTES, LONG, "byteVector" -> BYTEVECTOR) {
    case (bv: ByteVector) :: Nil => Right(bv.size)
    case _                       => ???
  }

  val take: BaseFunction = NativeFunction("take", 1, TAKE_BYTES, BYTEVECTOR, "byteVector" -> BYTEVECTOR, "number" -> LONG) {
    case (bv: ByteVector) :: (number: Long) :: Nil => Right(bv.take(number))
    case _                                         => ???
  }

  private def createOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short)(body: (t.Underlying, t.Underlying) => r.Underlying): BaseFunction =
    NativeFunction(opsToFunctions(op), 1, func, r, "a" -> t, "b" -> t) {
      case a :: b :: Nil =>
        Right(body(a.asInstanceOf[t.Underlying], b.asInstanceOf[t.Underlying]))
      case _ => ???
    }

  val getElement: BaseFunction =
    NativeFunction("getElement", 2, GET_LIST, TYPEPARAM('T'), "arr" -> LISTTYPEPARAM(TYPEPARAM('T')), "pos" -> LONG) {
      case (arr: IndexedSeq[_]) :: (pos: Long) :: Nil => Try(arr(pos.toInt)).toEither.left.map(_.toString)
      case _                                          => ???
    }

  val getListSize: BaseFunction = NativeFunction("size", 2, SIZE_LIST, LONG, "arr" -> LISTTYPEPARAM(TYPEPARAM('T'))) {
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

  private def createTryOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short)(body: (t.Underlying, t.Underlying) => r.Underlying): BaseFunction =
    NativeFunction(opsToFunctions(op), 1, func, r, "a" -> t, "b" -> t) {
      case a :: b :: Nil =>
        try {
          Right(body(a.asInstanceOf[t.Underlying], b.asInstanceOf[t.Underlying]))
        } catch {
          case e: Throwable => Left(e.getMessage())
        }
      case _ => ???
    }

  val mulLong: BaseFunction       = createTryOp(MUL_OP, LONG, LONG, MUL_LONG)(Math.multiplyExact)
  val divLong: BaseFunction       = createTryOp(DIV_OP, LONG, LONG, DIV_LONG)(Math.floorDiv)
  val modLong: BaseFunction       = createTryOp(MOD_OP, LONG, LONG, MOD_LONG)(Math.floorMod)
  val sumLong: BaseFunction       = createTryOp(SUM_OP, LONG, LONG, SUM_LONG)(Math.addExact)
  val subLong: BaseFunction       = createTryOp(SUB_OP, LONG, LONG, SUB_LONG)(Math.subtractExact)
  val sumString: BaseFunction     = createOp(SUM_OP, STRING, STRING, SUM_STRING)(_ + _)
  val sumByteVector: BaseFunction = createOp(SUM_OP, BYTEVECTOR, BYTEVECTOR, SUM_BYTES)((a, b) => ByteVector(a.toArray ++ b.toArray))
  val ge: BaseFunction            = createOp(GE_OP, LONG, BOOLEAN, GE_LONG)(_ >= _)
  val gt: BaseFunction            = createOp(GT_OP, LONG, BOOLEAN, GT_LONG)(_ > _)
  val sge: BaseFunction           = createOp(GE_OP, STRING, BOOLEAN, GE_STRING)(_ >= _)
  val sgt: BaseFunction           = createOp(GT_OP, STRING, BOOLEAN, GT_STRING)(_ > _)

  val eq: BaseFunction = NativeFunction.create(EQ_OP.func, 1, canBeEq(TYPEPARAM('A'), TYPEPARAM('B')) _, List("a" -> TYPEPARAM('A'), "b" -> TYPEPARAM('B')), EQ) {
    case a :: b :: Nil => Right(a == b)
    case _             => ???
  }

  val ne: BaseFunction = UserFunction(NE_OP.func, 1, BOOLEAN, "a" -> TYPEPARAM('T'), "b" -> TYPEPARAM('T')) {
    case a :: b :: Nil => Right(FUNCTION_CALL(FunctionHeader.Native(NOT_BOOLEAN), List(FUNCTION_CALL(FunctionHeader.Native(EQ), List(a, b)))))
    case _             => ???
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
    sge,
    sgt,
    getElement,
    getListSize,
    uMinus,
    uNot
  )

  private val vars      = Map( ("None", (UNION(NOTHING, UNIT), none)), (errRef, (NOTHING, err)))
  private val functions = Seq(fraction, extract, isDefined, some, size, take, _isInstanceOf) ++ operators

  lazy val ctx          = CTX(Seq(
                    new DefinedType { val name = "Unit"; val typeRef: TYPE = UNIT },
                    new DefinedType { val name = "Int"; val typeRef: TYPE = LONG },
                    new DefinedType { val name = "String"; val typeRef: TYPE = STRING }
                  ), vars, functions)
  lazy val evalContext     = ctx.evaluationContext
  lazy val compilerContext: CompilerContext = ctx.compilerContext

  def fromOption[T](v: Option[T]): Any = {
    v.getOrElse(():Any)
  }
}
