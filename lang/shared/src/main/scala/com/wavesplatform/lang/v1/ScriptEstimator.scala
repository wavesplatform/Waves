package com.wavesplatform.lang.v1

import cats.data.EitherT
import com.wavesplatform.lang.v1.compiler.Terms._
import monix.eval.Coeval

object ScriptEstimator {
  def apply(declaredVals: Set[String], functionCosts: collection.Map[FunctionHeader, Coeval[Long]], t: EXPR): Either[String, Long] = {
    type Result[T] = EitherT[Coeval, String, T]

    def aux(t: Result[EXPR], syms: Map[String, (EXPR, Boolean)]): Result[(Long, Map[String, (EXPR, Boolean)])] = t.flatMap {
      case _: CONST_LONG | _: CONST_BYTEVECTOR | _: CONST_STRING | TRUE | FALSE => EitherT.pure((1, syms))
      case t: GETTER                                                            => aux(EitherT.pure(t.expr), syms).map { case (comp, out) => (comp + 2, out) }

      case BLOCK(let, body) =>
        aux(EitherT.pure(body), syms + ((let.name, (let.value, false))))
          .map { case (comp, out) => (comp + 5, out) }

      case REF(key) =>
        val ei: EitherT[Coeval, String, (Long, Map[String, (EXPR, Boolean)])] = syms.get(key) match {
          case None                => EitherT.fromEither(Left(s"Undeclared variable '$key'"))
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
          callCost <- EitherT.fromOption[Coeval](functionCosts.get(t.function), s"Unknown function '${t.function}'")
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

    aux(EitherT.pure(t), declaredVals.map(_ -> ((TRUE, true))).toMap).value().map(_._1)
  }

  def denyDuplicateNames(t: EXPR): Either[String, Unit] = {
    type DenyDuplicates[T] = EitherT[Coeval, String, T]
    def aux(t: DenyDuplicates[EXPR], declared: Set[String]): DenyDuplicates[Set[String]] = {
      t flatMap {
        case _: CONST_LONG | _: CONST_BYTEVECTOR | _: CONST_STRING | TRUE | FALSE | REF(_) => EitherT.pure(declared)
        case BLOCK(LET(name, expr), body) =>
          EitherT
            .cond[Coeval](!(declared contains name) || name(0) == '$' || name(0) == '@',
                          declared + name,
                          s"ScriptValidator: duplicate variable names are not allowed: '$name'")
            .flatMap(aux(EitherT.pure(expr), _))
            .flatMap(aux(EitherT.pure(body), _))
        case IF(cond, ifTrue, ifFalse) =>
          aux(EitherT.pure(cond), declared)
            .flatMap(aux(EitherT.pure(ifTrue), _))
            .flatMap(aux(EitherT.pure(ifFalse), _))
        case GETTER(expr, _) => aux(EitherT.pure(expr), declared)
        case FUNCTION_CALL(_, args) =>
          args.foldLeft(EitherT.pure[Coeval, String](declared)) {
            case (declEi, arg) => declEi.flatMap(aux(EitherT.pure[Coeval, String](arg), _))
          }
      }
    }
    aux(EitherT.pure(t), Set.empty).value().map(_ => ())
  }
}
