package com.wavesplatform.lang.v1.evaluator.ctx.impl

import cats.data.EitherT
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, LazyVal, PredefFunction}
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import com.wavesplatform.lang.v1.parser.BinaryOperation
import com.wavesplatform.lang.v1.compiler.Terms._
import monix.eval.Coeval
import scodec.bits.ByteVector

object PureContext {
  private val optionT                                             = OPTIONTYPEPARAM(TYPEPARAM('T'))
  private val noneCoeval: Coeval[Either[String, Option[Nothing]]] = Coeval.evalOnce(Right(None))

  val none: LazyVal = LazyVal(OPTION(NOTHING))(EitherT(noneCoeval).subflatMap(Right(_: Option[Nothing]))) // IDEA HACK

  val extract: PredefFunction = PredefFunction("extract", 1, TYPEPARAM('T'), List(("opt", optionT))) {
    case Some(v) :: Nil => Right(v)
    case None :: Nil    => Left("Extract from empty option")
    case _              => ???
  }

  val some: PredefFunction = PredefFunction("Some", 1, optionT, List(("obj", TYPEPARAM('T')))) {
    case v :: Nil => Right(Some(v))
    case _        => ???
  }

  val isDefined: PredefFunction = PredefFunction("isDefined", 1, BOOLEAN, List(("opt", optionT))) {
    case Some(_) :: Nil => Right(true)
    case None :: Nil    => Right(false)
    case _              => ???
  }

  val size: PredefFunction = PredefFunction("size", 1, LONG, List(("byteVector", BYTEVECTOR))) {
    case (bv: ByteVector) :: Nil => Right(bv.size)
    case _                       => ???
  }

  private def createOp(op: BinaryOperation, t: TYPE, r: TYPE)(body: (t.Underlying, t.Underlying) => r.Underlying) = {
    PredefFunction(opsToFunctions(op), 1, r, List("a" -> t, "b" -> t)) {
      case a :: b :: Nil =>
        Right(body(a.asInstanceOf[t.Underlying], b.asInstanceOf[t.Underlying]))
      case _ => ???
    }
  }

  val sumLong       = createOp(SUM_OP, LONG, LONG)(_ + _)
  val sumString     = createOp(SUM_OP, STRING, STRING)(_ + _)
  val sumByteVector = createOp(SUM_OP, BYTEVECTOR, BYTEVECTOR)((a, b) => ByteVector(a.toArray ++ b.toArray))
  val eqLong        = createOp(EQ_OP, LONG, BOOLEAN)(_ == _)
  val eqByteVector  = createOp(EQ_OP, BYTEVECTOR, BOOLEAN)(_ == _)
  val eqBool        = createOp(EQ_OP, BOOLEAN, BOOLEAN)(_ == _)
  val eqString      = createOp(EQ_OP, STRING, BOOLEAN)(_ == _)
  val ge            = createOp(GE_OP, LONG, BOOLEAN)(_ >= _)
  val gt            = createOp(GT_OP, LONG, BOOLEAN)(_ > _)

  val operators: Seq[PredefFunction] = Seq(sumLong, sumString, sumByteVector, eqLong, eqByteVector, eqBool, eqString, ge, gt)

  lazy val instance =
    EvaluationContext.build(types = Seq.empty, letDefs = Map(("None", none)), functions = Seq(extract, isDefined, some, size) ++ operators)

}
