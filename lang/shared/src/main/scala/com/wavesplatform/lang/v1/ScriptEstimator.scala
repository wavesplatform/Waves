package com.wavesplatform.lang.v1

import cats.data.EitherT
import com.wavesplatform.lang.v1.compiler.Terms._
import monix.eval.Coeval

object ScriptEstimator {
  def apply(declaredVals: Set[String], functionCosts: collection.Map[FunctionHeader, Coeval[Long]], t: EXPR): Either[String, Long] = {
    type Result[T] = EitherT[Coeval, String, T]

    def aux(t: Result[EXPR], syms: Map[String, (EXPR, Boolean)]): Result[(Long, Map[String, (EXPR, Boolean)])] = t.flatMap {
      case _: CONST_LONG | _: CONST_BYTESTR | _: CONST_STRING | _: CONST_BOOLEAN => EitherT.pure((1, syms))
      case t: GETTER                                                                => aux(EitherT.pure(t.expr), syms).map { case (comp, out) => (comp + 2, out) }

      case LET_BLOCK(let: LET, body) =>
        aux(EitherT.pure(body), syms + ((let.name, (let.value, false))))
          .map { case (comp, out) => (comp + 5, out) }
      case BLOCK(let: LET, body) =>
        aux(EitherT.pure(body), syms + ((let.name, (let.value, false))))
          .map { case (comp, out) => (comp + 5, out) }
      case BLOCK(f: FUNC, body) => ???
      case REF(key) =>
        val ei: EitherT[Coeval, String, (Long, Map[String, (EXPR, Boolean)])] = syms.get(key) match {
          case None                => EitherT.fromEither(Left(s"ScriptValidator: Undeclared variable '$key'"))
          case Some((_, true))     => EitherT.pure[Coeval, String]((0L, syms))
          case Some((expr, false)) => aux(EitherT.pure(expr), syms + ((key, (expr, true))))
        }
        ei.map { case (comp: Long, out: Map[String, (EXPR, Boolean)]) => (comp + 2, out) }

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
          callCost <- EitherT.fromOption[Coeval](functionCosts.get(t.function), s"ScriptValidator: Unknown function '${t.function}'")
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

      case _ => ??? //TODO: FIx exhaustivness
    }

    aux(EitherT.pure(t), declaredVals.map(_ -> ((TRUE, true))).toMap).value().map(_._1)
  }
}
