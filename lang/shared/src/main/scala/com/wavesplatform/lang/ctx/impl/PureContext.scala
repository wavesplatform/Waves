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

  val operators: Seq[PredefFunction] = {
    def createOp(op: BINARY_OP_KIND, t: TYPE, r: TYPE)(body: (t.Underlying, t.Underlying) => r.Underlying) = {
      PredefFunction(Terms.opsToFunctions(op), r, List("a" -> t, "b" -> t)) {
        case a :: b :: Nil =>
          Right(body(a.asInstanceOf[t.Underlying], b.asInstanceOf[t.Underlying]))
        case _ => ???
      }
    }
    Seq(
      createOp(SUM_OP, LONG, LONG)((a, b) => a + b),
      createOp(SUM_OP, STRING, STRING)((a, b) => a + b),
      createOp(SUM_OP, BYTEVECTOR, BYTEVECTOR)((a, b) => ByteVector(a.toArray ++ b.toArray)),
      createOp(EQ_OP, LONG, BOOLEAN)((a, b) => a == b),
      createOp(EQ_OP, BYTEVECTOR, BOOLEAN)((a, b) => a == b),
      createOp(EQ_OP, BOOLEAN, BOOLEAN)((a, b) => a == b),
      createOp(GE_OP, LONG, BOOLEAN)((a, b) => a >= b),
      createOp(GT_OP, LONG, BOOLEAN)((a, b) => a > b)
    )
  }
  lazy val instance = Context.build(types = Seq.empty, letDefs = Map(("None", none)), functions = Seq(extract, isDefined, some, size) ++ operators)

}
