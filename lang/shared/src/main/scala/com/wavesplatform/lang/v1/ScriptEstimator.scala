package com.wavesplatform.lang.v1

import cats.data.EitherT
import com.wavesplatform.lang.v1.compiler.Terms._
import monix.eval.Coeval

object ScriptEstimator {
  def apply(functionCosts: Map[FunctionHeader, Long], t: EXPR): Either[String, Long] = {
    type Result[T] = EitherT[Coeval, String, T]

    def aux(t: Result[EXPR], syms: Map[String, Long]): Result[(Long, Map[String, Long])] = t.flatMap {
      case _: CONST_LONG | _: CONST_BYTEVECTOR | _: CONST_STRING | TRUE | FALSE =>
        Console.err.println(s"CONST")///
        EitherT.pure((1, syms))

      case t: GETTER =>
        Console.err.println(s"GETTER")///
        aux(EitherT.pure(t.expr), syms).map { case (comp, out) => (comp + 2, out) }

      case t: BLOCK =>
        Console.err.println(s"BLK")///
        for {
          let <- aux(EitherT.pure(t.let.value), syms)
          (letComp, letSyms) = let
          body <- aux(EitherT.pure(t.body), letSyms + (t.let.name -> letComp)) /// let a=b+c; b
          (bodyComp, bodySyms) = body
        } yield (bodyComp + 5, bodySyms)

      case t: IF =>
        Console.err.println(s"IF")///
        for {
          cond <- aux(EitherT.pure(t.cond), syms)
          (condComp, condSyms) = cond
          right <- aux(EitherT.pure(t.ifTrue), condSyms)
          (rightComp, rightSyms) = right
          left <- aux(EitherT.pure(t.ifFalse), condSyms)
          (leftComp, leftSyms) = left
          (bodyComp, bodySyms) = if (rightComp > leftComp) (rightComp, rightSyms) else (leftComp, leftSyms)
        } yield (1 + condComp + bodyComp, bodySyms)

      case t: REF =>
        Console.err.println(s"REF")///
        EitherT.pure((2 + syms.getOrElse(t.key, 0L), syms - t.key))

      case t: FUNCTION_CALL =>
        Console.err.println(s"FUNC ${t.function.name}(${t.args.size} args)")///
        for {
          callCost <- EitherT.fromOption[Coeval](functionCosts.get(t.function), s"Unknown function ${t.function}")
          args <- t.args.foldLeft(EitherT.pure[Coeval, String]((0L, syms))) { case (accEi, arg) =>
            for {
              acc <- accEi
              (accComp, accSyms) = acc
              v <- aux(EitherT.pure[Coeval, String](arg), accSyms)
              (comp, out) = v
            } yield (accComp + comp, out)
          }
          (argsComp, argsSyms) = args
        } yield (callCost + argsComp, argsSyms)
    }

    aux(EitherT.pure(t), Map.empty).value().map(_._1)
  }
}
