package com.wavesplatform.lang.v1.evaluator.ctx.impl

import cats.data.EitherT
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.parser.BinaryOperation
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import monix.eval.Coeval
import scodec.bits.ByteVector

import scala.util.Try

object PureContext {
//  private val optionT                                             = OPTIONTYPEPARAM(TYPEPARAM('T'))
  private val noneCoeval: Coeval[Either[String, Option[Nothing]]] = Coeval.evalOnce(Right(None))
  private val nothingCoeval: Coeval[Either[String, Nothing]]      = Coeval.defer(Coeval(Right(throw new Exception("explicit contract termination"))))

  val none: LazyVal = LazyVal(EitherT(noneCoeval).subflatMap(Right(_: Option[Nothing]))) // IDEA HACK
  val err           = LazyVal(EitherT(nothingCoeval))
  val errRef        = "throw"

  val fraction: PredefFunction = PredefFunction("fraction", 1, LONG, List(("value", LONG), ("numerator", LONG), ("denominator", LONG)), FRACTION) {
    case (v: Long) :: (n: Long) :: (d: Long) :: Nil => {
      val result = BigInt(v) * n / d
      for {
        _ <- Either.cond(result < Long.MaxValue, (), s"Long overflow: value `$result` greater than 2^63-1")
        _ <- Either.cond(result > Long.MinValue, (), s"Long overflow: value `$result` less than -2^63-1")
      } yield result.toLong
    }
    case _ => ???
  }

//  val extract: PredefFunction = PredefFunction("extract", 1, TYPEPARAM('T'), List(("opt", optionT)), EXTRACT) {
//    case Some(v) :: Nil => Right(v)
//    case None :: Nil    => Left("Extract from empty option")
//    case _              => ???
//  }

//  val some: PredefFunction = PredefFunction("Some", 1, optionT, List(("obj", TYPEPARAM('T'))), SOME) {
//    case v :: Nil => Right(Some(v))
//    case _        => ???
//  }

  val extractU: PredefFunction = PredefFunction.create("extractU", 1L, fromNonable(TYPEPARAM('T')) _, List(("opt", TYPEPARAM('T'):TYPEPLACEHOLDER)), 1124: Short) {
    case () :: Nil    => Left("Extract from empty option")
    case v :: Nil => Right(v)
    case _              => ???
  }

  val someU: PredefFunction = PredefFunction.create("SomeU", 1L, asNonable(TYPEPARAM('T')) _, List(("obj", TYPEPARAM('T'):TYPEPLACEHOLDER)), 1125: Short) {
    case v :: Nil => Right(v)
    case _        => ???
  }

  val isDefinedU: PredefFunction = PredefFunction("isDefined", 1, BOOLEAN, List(("opt", TYPEPARAM('T'):TYPEPLACEHOLDER)), 1126) {
    case (()) :: Nil => Right(false)
    case _           => Right(true)
  }

  val _isInstanceOf: PredefFunction = PredefFunction("_isInstanceOf", 1, BOOLEAN, List(("obj", TYPEPARAM('T')), ("of", STRING)), ISINSTANCEOF) {
    case (p: Boolean) :: ("Boolean") :: Nil => Right(true)
    case (p: String) :: ("String") :: Nil => Right(true)
    case (p: Long) :: ("Int") :: Nil => Right(true)
    case (()) :: ("Unit") :: Nil => Right(true)
    case (p: CaseObj) :: (s: String) :: Nil => Right(p.caseType.name == s)
    case _                                  => Right(false)
  }

//  val isDefined: PredefFunction = PredefFunction("isDefined", 1, BOOLEAN, List(("opt", optionT)), ISDEFINED) {
//    case Some(_) :: Nil => Right(true)
//    case None :: Nil    => Right(false)
//    case _              => ???
//  }

  val size: PredefFunction = PredefFunction("size", 1, LONG, List(("byteVector", BYTEVECTOR)), SIZE_BYTES) {
    case (bv: ByteVector) :: Nil => Right(bv.size)
    case _                       => ???
  }

  private def createOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short)(body: (t.Underlying, t.Underlying) => r.Underlying) = {
    PredefFunction(opsToFunctions(op), 1, r, List("a" -> t, "b" -> t), func) {
      case a :: b :: Nil =>
        Right(body(a.asInstanceOf[t.Underlying], b.asInstanceOf[t.Underlying]))
      case _ => ???
    }
  }

  val getElement = PredefFunction("getElement", 2, TYPEPARAM('T'), List("arr" -> LISTTYPEPARAM(TYPEPARAM('T')), "pos" -> LONG), GET_LIST) {
    case (arr: IndexedSeq[_]) :: (pos: Long) :: Nil => Try(arr(pos.toInt)).toEither.left.map(_.toString)
    case _                                          => ???
  }

  val getListSize = PredefFunction("size", 2, LONG, List("arr" -> LISTTYPEPARAM(TYPEPARAM('T'))), SIZE_LIST) {
    case (arr: IndexedSeq[_]) :: Nil => {

      Right(arr.size.toLong)
    }
    case _ => ???
  }

  val uMinus = PredefFunction("-", 1, LONG, List("n" -> LONG), MINUS_LONG) {
    case (n: Long) :: Nil => {
      Right(Math.negateExact(n))
    }
    case _ => ???
  }

  val uNot = PredefFunction("!", 1, BOOLEAN, List("p" -> BOOLEAN), NOT_BOOLEAN) {
    case (p: Boolean) :: Nil => {
      Right(!p)
    }
    case _ => ???
  }

  private def createTryOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short)(body: (t.Underlying, t.Underlying) => r.Underlying) = {
    PredefFunction(opsToFunctions(op), 1, r, List("a" -> t, "b" -> t), func) {
      case a :: b :: Nil =>
        try {
          Right(body(a.asInstanceOf[t.Underlying], b.asInstanceOf[t.Underlying]))
        } catch {
          case e: Throwable => Left(e.getMessage())
        }
      case _ => ???
    }
  }

  val mulLong       = createTryOp(MUL_OP, LONG, LONG, MUL_LONG)(Math.multiplyExact)
  val divLong       = createTryOp(DIV_OP, LONG, LONG, DIV_LONG)(Math.floorDiv)
  val modLong       = createTryOp(MOD_OP, LONG, LONG, MOD_LONG)(Math.floorMod)
  val sumLong       = createTryOp(SUM_OP, LONG, LONG, SUM_LONG)(Math.addExact)
  val subLong       = createTryOp(SUB_OP, LONG, LONG, SUB_LONG)(Math.subtractExact)
  val sumString     = createOp(SUM_OP, STRING, STRING, SUM_STRING)(_ + _)
  val sumByteVector = createOp(SUM_OP, BYTEVECTOR, BYTEVECTOR, SUM_BYTES)((a, b) => ByteVector(a.toArray ++ b.toArray))
  val ge            = createOp(GE_OP, LONG, BOOLEAN, GE_LONG)(_ >= _)
  val gt            = createOp(GT_OP, LONG, BOOLEAN, GT_LONG)(_ > _)
  val sge           = createOp(GE_OP, STRING, BOOLEAN, GE_STRING)(_ >= _)
  val sgt           = createOp(GT_OP, STRING, BOOLEAN, GT_STRING)(_ > _)

  val eq = PredefFunction(EQ_OP.func, 1, BOOLEAN, List("a" -> TYPEPARAM('T'), "b" -> TYPEPARAM('T')), EQ) {
    case a :: b :: Nil => Right(a == b)
    case _ => ???
  }

  val ne = PredefFunction(NE_OP.func, 1, BOOLEAN, List("a" -> TYPEPARAM('T'), "b" -> TYPEPARAM('T')), NE) {
    case a :: b :: Nil => Right(a != b)
    case _ => ???
  }

  val operators: Seq[PredefFunction] = Seq(
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

  private val vars      = Map( /*("None", (OPTION(NOTHING), none)),*/ (errRef, (NOTHING, err)))
  private val functions = Seq(fraction, /*extract, isDefined, some,*/ size, _isInstanceOf, extractU, someU) ++ operators

  lazy val ctx          = CTX(Seq(
                    new DefinedType { val name = "Unit"; val typeRef: TYPE = UNIT },
                    new DefinedType { val name = "Int"; val typeRef: TYPE = LONG },
                    new DefinedType { val name = "String"; val typeRef: TYPE = STRING }
                  ), vars, functions)
  lazy val evalContext     = ctx.evaluationContext
  lazy val compilerContext = ctx.compilerContext

  def fromOption[T](v: Option[T]): Any = {
    v.getOrElse(():Any)
  }
}
