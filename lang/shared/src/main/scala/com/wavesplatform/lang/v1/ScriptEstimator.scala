package com.wavesplatform.lang.v1

import cats.data.EitherT
import com.wavesplatform.lang.v1.compiler.Terms._
import monix.eval.Coeval

object ScriptEstimator {
  def apply(functionCosts: Map[FunctionHeader, Long], t: EXPR): Either[String, Long] = {
    type Result[T] = EitherT[Coeval, String, T]

    def aux(t: Result[EXPR]): Result[Long] = t.flatMap {
      case _: TYPELIST => EitherT.pure(0)
      case _: CONST_LONG | _: CONST_BYTEVECTOR | _: CONST_STRING | TRUE | FALSE => EitherT.pure(1)

      case t: GETTER => aux(EitherT.pure(t.expr)).map(_ + 2)
      case t: BLOCK  => aux(EitherT.pure(t.body)).map(_ + 5)
      case t: IF =>
        for {
          cond  <- aux(EitherT.pure(t.cond))
          right <- aux(EitherT.pure(t.ifTrue))
          left  <- aux(EitherT.pure(t.ifFalse))
        } yield 1 + cond + math.max(right, left)
      case _: REF => EitherT.pure(2)
      case t: FUNCTION_CALL =>
        import cats.instances.list._
        import cats.syntax.traverse._

        val callCost = functionCosts.get(t.function).fold[Either[String, Long]](Left(s"Unknown function ${t.function}"))(Right(_))
        for {
          callCost <- EitherT.fromEither[Coeval](callCost)
          args     <- t.args.map(x => aux(EitherT.pure[Coeval, String](x))).sequence[Result, Long]
        } yield callCost + args.sum
    }

    aux(EitherT.pure(t)).value()
  }
}
