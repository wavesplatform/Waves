package com.wavesplatform.lang.v1.estimator

import cats.data.EitherT
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import monix.eval.Coeval

object ScriptEstimatorV1 extends ScriptEstimator {
  override val version: Int = 1

  override def apply(declaredVals: Set[String], functionCosts: Map[FunctionHeader, Coeval[Long]], t: EXPR): Either[String, Long] = {
    type Result[T] = EitherT[Coeval, String, T]

    def aux(
        t: Result[EXPR],
        syms: Map[String, (EXPR, Boolean)],
        funcs: Map[FunctionHeader, Coeval[Long]]
    ): Result[(Long, Map[String, (EXPR, Boolean)])] = t.flatMap {

      case _: CONST_LONG | _: CONST_BYTESTR | _: CONST_STRING | _: CONST_BOOLEAN => EitherT.pure((1, syms))
      case t: GETTER => aux(EitherT.pure(t.expr), syms, funcs).map { case (comp, out) => (comp + 2, out) }

      case LET_BLOCK(let: LET, body) =>
        aux(EitherT.pure(body), syms + ((let.name, (let.value, false))), funcs)
          .map { case (comp, out) => (comp + 5, out) }

      case BLOCK(let: LET, body) =>
        aux(EitherT.pure(body), syms + ((let.name, (let.value, false))), funcs)
          .map { case (comp, out) => (comp + 5, out) }

      case BLOCK(f: FUNC, body) => {
        aux(EitherT.pure(f.body), syms ++ f.args.map(arg => (arg, (TRUE, false))).toMap, funcs)
          .flatMap { case (funcComplexity, _) =>
            aux(
              EitherT.pure(body),
              syms,
              funcs + (FunctionHeader.User(f.name) -> Coeval.evalOnce(funcComplexity + f.args.size * 5))
            ).map { case (comp, out) => (comp + 5, out) }
          }
      }

      case REF(key) =>
        val ei: EitherT[Coeval, String, (Long, Map[String, (EXPR, Boolean)])] = syms.get(key) match {
          case None                => EitherT.fromEither(Left(s"ScriptValidator: Undeclared variable '$key'"))
          case Some((_, true))     => EitherT.pure[Coeval, String]((0L, syms))
          case Some((expr, false)) => aux(EitherT.pure(expr), syms + ((key, (expr, true))), funcs)
        }
        ei.map { case (comp: Long, out: Map[String, (EXPR, Boolean)]) => (comp + 2, out) }

      case t: IF =>
        for {
          cond <- aux(EitherT.pure(t.cond), syms, funcs)
          (condComp, condSyms) = cond
          right <- aux(EitherT.pure(t.ifTrue), condSyms, funcs)
          left  <- aux(EitherT.pure(t.ifFalse), condSyms, funcs)
          (bodyComp, bodySyms) = if (right._1 > left._1) right else left
        } yield (condComp + bodyComp + 1, bodySyms)

      case t: FUNCTION_CALL =>
        for {
          callCost <- EitherT
            .fromOption[Coeval](functionCosts.get(t.function).orElse(funcs.get(t.function)), s"ScriptValidator: Unknown function '${t.function}'")
          args <- t.args.foldLeft(EitherT.pure[Coeval, String]((0L, syms))) { case (accEi, arg) =>
            for {
              acc <- accEi
              (accComp, accSyms) = acc
              v <- aux(EitherT.pure[Coeval, String](arg), accSyms, funcs)
              (comp, out) = v
            } yield (accComp + comp, out)
          }
          (argsComp, argsSyms) = args
        } yield (callCost() + argsComp, argsSyms)

      case _ => ??? // TODO: FIx exhaustivness
    }

    aux(EitherT.pure(t), declaredVals.map(_ -> ((TRUE, true))).toMap, Map.empty).value().map(_._1)
  }
}
