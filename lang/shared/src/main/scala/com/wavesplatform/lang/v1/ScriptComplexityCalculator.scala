package com.wavesplatform.lang.v1

import cats.data.EitherT
import com.wavesplatform.lang.v1.Terms._
import monix.eval.Coeval

object ScriptComplexityCalculator {
  def apply(functionCosts: Map[FunctionHeader, Long], t: Typed.EXPR): Either[String, Long] = {
    type Result[T] = EitherT[Coeval, String, T]

    def aux(t: Result[Typed.EXPR]): Result[Long] = t.flatMap {
      case _: Typed.CONST_LONG | _: Typed.CONST_BYTEVECTOR | _: Typed.CONST_STRING | Typed.TRUE | Typed.FALSE => EitherT.pure(1)

      case t: Typed.GETTER => aux(EitherT.pure(t.ref)).map(_ + 2)
      case t: Typed.BLOCK  => aux(EitherT.pure(t.body)).map(_ + 5)
      case t: Typed.IF =>
        for {
          cond  <- aux(EitherT.pure(t.cond))
          right <- aux(EitherT.pure(t.ifTrue))
          left  <- aux(EitherT.pure(t.ifFalse))
        } yield 1 + cond + math.max(right, left)

      case _: Typed.REF => EitherT.pure(2)
      case t: Typed.FUNCTION_CALL =>
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
