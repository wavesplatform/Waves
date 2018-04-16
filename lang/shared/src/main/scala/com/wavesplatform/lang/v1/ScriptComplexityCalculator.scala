package com.wavesplatform.lang.v1

import com.wavesplatform.lang.v1.Terms._

object ScriptComplexityCalculator {
  val MaxCost = 1000

  type Result[T] = Either[String, T]

  def apply(t: Typed.EXPR, functionCosts: Map[FunctionHeader, Long]): Result[Long] = {
    def aux(t: Typed.EXPR): Result[Long] = t match {
      case _: Typed.CONST_LONG | _: Typed.CONST_BYTEVECTOR | _: Typed.CONST_STRING | Typed.TRUE | Typed.FALSE => Right(1)

      case t: Typed.GETTER => aux(t.ref).map(_ + 2)
      case t: Typed.BLOCK  => aux(t.body).map(_ + t.let.fold(0L)(_ => 5L))
      case t: Typed.IF =>
        for {
          cond  <- aux(t.cond)
          right <- aux(t.ifTrue)
          left  <- aux(t.ifFalse)
        } yield 1 + cond + math.max(right, left)

      case _: Typed.REF => Right(2)
      case t: Typed.FUNCTION_CALL =>
        import cats.instances.either._
        import cats.instances.list._
        import cats.syntax.traverse._

        val callCost = functionCosts.get(t.function).fold[Result[Long]](Left(s"Unknown function ${t.function}"))(Right(_))
        (callCost :: t.args.map(aux)).sequence[Result, Long].map(_.sum)
    }

    aux(t)
  }
}
