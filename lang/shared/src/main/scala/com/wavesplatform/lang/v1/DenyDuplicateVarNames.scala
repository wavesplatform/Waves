package com.wavesplatform.lang.v1

import cats.data.EitherT
import com.wavesplatform.lang.ScriptVersion
import com.wavesplatform.lang.ScriptVersion.Versions._
import com.wavesplatform.lang.v1.compiler.Terms._
import monix.eval.Coeval

object DenyDuplicateVarNames {
  def apply(version: ScriptVersion, initial: Set[String], t: EXPR): Either[String, Unit] = {
    type DenyDuplicates[T] = EitherT[Coeval, String, T]

    def isVarValid(declared: Set[String], name: String): Either[String, Set[String]] = name(0) match {
      case '$' => Right(declared)
      case '@' => Left("VarNames: Can't declare var starting with @")
      case _ =>
        Either.cond(!(declared contains name), declared + name, throw new Exception(s"VarNames: duplicate variable names are temporarily denied: '$name'"))
    }

    def aux(t: DenyDuplicates[EXPR], declared: Set[String]): DenyDuplicates[Set[String]] = {
      t flatMap {
        case _: CONST_LONG | _: CONST_BYTEVECTOR | _: CONST_STRING | _: CONST_BOOLEAN | REF(_) => EitherT.pure(declared)
        case BLOCK(LET(name, expr), body) =>
          EitherT
            .fromEither[Coeval](isVarValid(declared, name))
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

    version match {
      case V1 => aux(EitherT.pure(t), initial).value().map(_ => ())
      case V2 => Right(())
    }
  }
}
