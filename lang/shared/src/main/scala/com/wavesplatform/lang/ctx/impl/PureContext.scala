package com.wavesplatform.lang.ctx.impl

import cats.data.EitherT
import com.wavesplatform.lang.Terms
import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang.ctx.{Context, LazyVal, PredefFunction}
import monix.eval.Coeval
import scodec.bits.ByteVector

object PureContext {
  private val optionT                                             = OPTIONTYPEPARAM(TYPEPARAM('T'))
  private val noneCoeval: Coeval[Either[String, Option[Nothing]]] = Coeval.evalOnce(Right(None))

  val none: LazyVal = LazyVal(OPTION(NOTHING))(EitherT(noneCoeval).subflatMap(Right(_: Option[Nothing]))) // IDEA HACK

  val extract: PredefFunction = PredefFunction("extract", TYPEPARAM('T'), List(("opt", optionT))) {
    case Some(v) :: Nil => Right(v)
    case None :: Nil    => Left("Extract from empty option")
    case _              => ???
  }

  val some: PredefFunction = PredefFunction("Some", optionT, List(("obj", TYPEPARAM('T')))) {
    case v :: Nil => Right(Some(v))
    case _        => ???
  }

  val isDefined: PredefFunction = PredefFunction("isDefined", BOOLEAN, List(("opt", optionT))) {
    case Some(_) :: Nil => Right(true)
    case None :: Nil    => Right(false)
    case _              => ???
  }

  val size: PredefFunction = PredefFunction("size", LONG, List(("byteVector", BYTEVECTOR))) {
    case (bv: ByteVector) :: Nil => Right(bv.size)
    case _                       => ???
  }

  private def createOp(op: BINARY_OP_KIND, t: TYPE, r: TYPE)(body: (t.Underlying, t.Underlying) => r.Underlying) = {
    PredefFunction(Terms.opsToFunctions(op), r, List("a" -> t, "b" -> t)) {
      case a :: b :: Nil =>
        Right(body(a.asInstanceOf[t.Underlying], b.asInstanceOf[t.Underlying]))
      case _ => ???
    }
  }

  val sumLong       = createOp(SUM_OP, LONG, LONG)((a, b) => a + b)
  val sumString     = createOp(SUM_OP, STRING, STRING)((a, b) => a + b)
  val sumByteVector = createOp(SUM_OP, BYTEVECTOR, BYTEVECTOR)((a, b) => ByteVector(a.toArray ++ b.toArray))
  val eqLong        = createOp(EQ_OP, LONG, BOOLEAN)((a, b) => a == b)
  val eqByteVector  = createOp(EQ_OP, BYTEVECTOR, BOOLEAN)((a, b) => a == b)
  val eqBool        = createOp(EQ_OP, BOOLEAN, BOOLEAN)((a, b) => a == b)
  val eqString      = createOp(EQ_OP, STRING, BOOLEAN)((a, b) => a == b)
  val ge            = createOp(GE_OP, LONG, BOOLEAN)((a, b) => a >= b)
  val gt            = createOp(GT_OP, LONG, BOOLEAN)((a, b) => a > b)

  val operators: Seq[PredefFunction] = Seq(sumLong, sumString, sumByteVector, eqLong, eqByteVector, eqBool, eqString, ge, gt)

  lazy val instance = Context.build(types = Seq.empty, letDefs = Map(("None", none)), functions = Seq(extract, isDefined, some, size) ++ operators)

}
