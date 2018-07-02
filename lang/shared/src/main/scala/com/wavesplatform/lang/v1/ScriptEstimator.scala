package com.wavesplatform.lang.v1

import cats.data.EitherT
import com.wavesplatform.lang.v1.compiler.Terms._
import monix.eval.Coeval

object ScriptEstimator {
  def apply(functionCosts: collection.Map[FunctionHeader, Coeval[Long]], t: EXPR): Either[String, Long] = {
    type Result[T] = EitherT[Coeval, String, T]

    def aux(t: Result[EXPR], syms: Map[String, EXPR]): Result[(Long, Map[String, EXPR])] = t.flatMap {
      case _: CONST_LONG | _: CONST_BYTEVECTOR | _: CONST_STRING | TRUE | FALSE => EitherT.pure((1, syms))
      case t: GETTER                                                            => aux(EitherT.pure(t.expr), syms).map { case (comp, out) => (comp + 2, out) }

      case t: BLOCK =>
        aux(EitherT.pure(t.body), syms + (t.let.name -> t.let.value))
          .map { case (comp, out) => (comp + 5, out) }

      case t: REF =>
        syms
          .get(t.key)
          .map(expr => aux(EitherT.pure(expr), syms - t.key))
          .getOrElse(EitherT.pure[Coeval, String]((0L, syms)))
          .map { case (comp, out) => (comp + 2, out) }

      case t: IF =>
        for {
          cond <- aux(EitherT.pure(t.cond), syms)
          (condComp, condSyms) = cond
          right <- aux(EitherT.pure(t.ifTrue), condSyms)
          left  <- aux(EitherT.pure(t.ifFalse), condSyms)
          (bodyComp, bodySyms) = if (right._1 > left._1) right else left
        } yield (condComp + bodyComp + 1, bodySyms)

      case t: FUNCTION_CALL =>
        for {
          callCost <- EitherT.fromOption[Coeval](functionCosts.get(t.function), s"Unknown function ${t.function}")
          args <- t.args.foldLeft(EitherT.pure[Coeval, String]((0L, syms))) {
            case (accEi, arg) =>
              for {
                acc <- accEi
                (accComp, accSyms) = acc
                v <- aux(EitherT.pure[Coeval, String](arg), accSyms)
                (comp, out) = v
              } yield (accComp + comp, out)
          }
          (argsComp, argsSyms) = args
        } yield (callCost() + argsComp, argsSyms)
    }

    aux(EitherT.pure(t), Map.empty).value().map(_._1)
  }
}
