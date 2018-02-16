package com.wavesplatform.lang

import cats.data.EitherT
import cats.syntax.all._
import com.wavesplatform.lang.Terms._
import monix.eval.Coeval

import scala.util.{Failure, Success, Try}

object TypeChecker {

  type Defs = Map[String, Type]

  case class Context(predefTypes: Map[String, CUSTOMTYPE], varDefs: Defs)
  object Context {
    val empty = Context(Map.empty, Map.empty)
  }

  type TypeResolutionError = String
  type TypeCheckResult[T] = Either[TypeResolutionError, T]
  private type SetTypeResult[T] = EitherT[Coeval, String, T]

  class TypeResolveException(ref: Expr) extends IllegalStateException(s"Type of $ref was not resolved")

  private def setType(ctx: Context, t: SetTypeResult[Expr]): SetTypeResult[Expr] = t.flatMap {
    case _: CONST_INT | _: CONST_BYTEVECTOR | TRUE | FALSE | NONE => t

    case getter: GETTER =>
      setType(ctx, EitherT.pure(getter.ref))
        .map(_.asInstanceOf[Block])
        .subflatMap { ref =>
          ref.exprType match {
            case Some(typeRef: TYPEREF) =>
              val refTpe = ctx.predefTypes.get(typeRef.name).map(Right(_)).getOrElse(Left(s"Undefined type: ${typeRef.name}"))
              val fieldTpe = refTpe.flatMap { ct =>
                val fieldTpe = ct.fields.collectFirst {
                  case (fieldName, tpe) if fieldName == getter.field => tpe
                }

                fieldTpe.map(Right(_)).getOrElse(Left(s"Undefined field ${typeRef.name}.${getter.field}"))
              }

              fieldTpe.right.map(tpe => getter.copy(ref = ref, exprType = Some(tpe)))
            case Some(_) => Left(s"Can't access to '${getter.field}' of a primitive type")
            case None => throw new TypeResolveException(ref)
          }
        }

    case sum: SUM =>
      (setType(ctx, EitherT.pure(sum.i1)), setType(ctx, EitherT.pure(sum.i2)))
        .mapN { (first, second) => sum.copy(i1 = first.asInstanceOf[Block], i2 = second.asInstanceOf[Block]) }
        .subflatMap { sum =>
          val firstType = sum.i1.exprType.getOrElse(throw new TypeResolveException(sum.i1))
          val secondType = sum.i2.exprType.getOrElse(throw new TypeResolveException(sum.i2))

          if (firstType != INT) Left(s"The first operand is expected to be INT, but got $firstType")
          else if (secondType != INT) Left(s"The second operand is expected to be INT, but got $secondType")
          else Right(sum)
        }

    case and: AND =>
      (setType(ctx, EitherT.pure(and.t1)), setType(ctx, EitherT.pure(and.t2)))
        .mapN { (first, second) => and.copy(t1 = first.asInstanceOf[Block], t2 = second.asInstanceOf[Block]) }
        .subflatMap { and =>
          val firstType = and.t1.exprType.getOrElse(throw new TypeResolveException(and.t1))
          val secondType = and.t2.exprType.getOrElse(throw new TypeResolveException(and.t2))

          if (firstType != BOOLEAN) Left(s"The first operand is expected to be BOOLEAN, but got $firstType")
          else if (secondType != BOOLEAN) Left(s"The second operand is expected to be BOOLEAN, but got $secondType")
          else Right(and)
        }

    case or: OR =>
      (setType(ctx, EitherT.pure(or.t1)), setType(ctx, EitherT.pure(or.t2)))
        .mapN { (first, second) => or.copy(t1 = first.asInstanceOf[Block], t2 = second.asInstanceOf[Block]) }
        .subflatMap { or =>
          val firstType = or.t1.exprType.getOrElse(throw new TypeResolveException(or.t1))
          val secondType = or.t2.exprType.getOrElse(throw new TypeResolveException(or.t2))

          if (firstType != BOOLEAN) Left(s"The first operand is expected to be BOOLEAN, but got $firstType")
          else if (secondType != BOOLEAN) Left(s"The second operand is expected to be BOOLEAN, but got $secondType")
          else Right(or)
        }

    case eq: EQ =>
      (setType(ctx, EitherT.pure(eq.t1)), setType(ctx, EitherT.pure(eq.t2)))
        .mapN { (first, second) => eq.copy(t1 = first.asInstanceOf[Block], t2 = second.asInstanceOf[Block]) }
        .subflatMap { eq =>
          val firstType = eq.t1.exprType.getOrElse(throw new TypeResolveException(eq.t1))
          val secondType = eq.t2.exprType.getOrElse(throw new TypeResolveException(eq.t2))

          findCommonType(firstType, secondType)
            .map(_ => Right(eq))
            .getOrElse(Left(s"Can't find common type for $firstType and $secondType"))
        }

    case gt: GT =>
      (setType(ctx, EitherT.pure(gt.t1)), setType(ctx, EitherT.pure(gt.t2)))
        .mapN { (first, second) => gt.copy(t1 = first.asInstanceOf[Block], t2 = second.asInstanceOf[Block]) }
        .subflatMap { gt =>
          val firstType = gt.t1.exprType.getOrElse(throw new TypeResolveException(gt.t1))
          val secondType = gt.t2.exprType.getOrElse(throw new TypeResolveException(gt.t2))

          if (firstType != INT) Left(s"The first operand is expected to be INT, but got $firstType")
          else if (secondType != INT) Left(s"The second operand is expected to be INT, but got $secondType")
          else Right(gt)
        }

    case ge: GE =>
      (setType(ctx, EitherT.pure(ge.t1)), setType(ctx, EitherT.pure(ge.t2)))
        .mapN { (first, second) => ge.copy(t1 = first.asInstanceOf[Block], t2 = second.asInstanceOf[Block]) }
        .subflatMap { ge =>
          val firstType = ge.t1.exprType.getOrElse(throw new TypeResolveException(ge.t1))
          val secondType = ge.t2.exprType.getOrElse(throw new TypeResolveException(ge.t2))

          if (firstType != INT) Left(s"The first operand is expected to be INT, but got $firstType")
          else if (secondType != INT) Left(s"The second operand is expected to be INT, but got $secondType")
          else Right(ge)
        }

    case sigVerify: SIG_VERIFY =>
      (setType(ctx, EitherT.pure(sigVerify.message)), setType(ctx, EitherT.pure(sigVerify.signature)), setType(ctx, EitherT.pure(sigVerify.publicKey)))
        .mapN { (resolvedMessage, resolvedSignature, resolvedPublicKey) =>
          sigVerify.copy(
            message = resolvedMessage.asInstanceOf[Block],
            signature = resolvedSignature.asInstanceOf[Block],
            publicKey = resolvedPublicKey.asInstanceOf[Block]
          )
        }

    case isDefined: IS_DEFINED =>
      setType(ctx, EitherT.pure(isDefined.t))
        .map(_.asInstanceOf[Block])
        .map(of => isDefined.copy(t = of))

    case let: LET =>
      setType(ctx, EitherT.pure(let.value))
        .map(_.asInstanceOf[Block])
        .map(value => let.copy(value = value))

    case block: Block =>
      block.let match {
        case None =>
          setType(ctx, EitherT.pure(block.t)).map { resolvedT => block.copy(t = resolvedT, exprType = resolvedT.exprType) }

        case Some(let) =>
          setType(ctx, EitherT.pure(let))
            .flatMap {
              case letExpr: LET =>
                val updatedCtx = ctx.copy(varDefs = ctx.varDefs + (let.name -> letExpr.value.exprType.getOrElse(throw new TypeResolveException(letExpr.value))))
                setType(updatedCtx, EitherT.pure(block.t))
                  .map { inExpr =>
                    block.copy(
                      let = Some(letExpr),
                      t = inExpr,
                      exprType = inExpr.exprType
                    )
                  }
              case x => EitherT.fromEither(Left(s"Inferred '$x' during type check. Expected LET"))
            }
      }

    case ifExpr: IF =>
      (setType(ctx, EitherT.pure(ifExpr.cond)), setType(ctx, EitherT.pure(ifExpr.ifTrue)), setType(ctx, EitherT.pure(ifExpr.ifFalse)))
        .mapN { (resolvedCond, resolvedTrueBranch, resolvedFalseBranch) =>
          ifExpr.copy(
            cond = resolvedCond.asInstanceOf[Block],
            ifTrue = resolvedTrueBranch.asInstanceOf[Block],
            ifFalse = resolvedFalseBranch.asInstanceOf[Block]
          )
        }
        .subflatMap { ifExpr =>
          val ifTrueType = ifExpr.ifTrue.exprType.getOrElse(throw new TypeResolveException(ifExpr.ifTrue))
          val ifFalseType = ifExpr.ifFalse.exprType.getOrElse(throw new TypeResolveException(ifExpr.ifFalse))
          findCommonType(ifTrueType, ifFalseType)
            .map(tpe => Right(ifExpr.copy(exprType = Some(tpe))))
            .getOrElse(Left(s"Can't find common type for $ifTrueType and $ifFalseType"))
        }

    case ref: REF => EitherT.fromEither {
      ctx.varDefs
        .get(ref.key)
        .map { tpe => ref.copy(exprType = Some(tpe)) }
        .toRight(s"A definition of '${ref.key}' is not found")
    }

    case get: GET =>
      setType(ctx, EitherT.pure(get.t))
        .map(_.asInstanceOf[Block])
        .subflatMap { expr =>
          expr.exprType match {
            case Some(OPTION(in)) => Right(get.copy(t = expr, exprType = Some(in)))
            case Some(x) => Left(s"GET called on $x, but only call on OPTION[_] is allowed")
            case None => throw new TypeResolveException(expr)
          }
        }

    case some: SOME =>
      setType(ctx, EitherT.pure(some.t))
        .map(_.asInstanceOf[Block])
        .map { t => some.copy(t = t, exprType = t.exprType.map(OPTION)) }
  }

  def apply(c: Context, expr: Expr): TypeCheckResult[Expr] = {
    val result = setType(c, EitherT.pure(expr)).value().left.map { e => s"Typecheck failed: $e" }
    Try(result) match {
      case Failure(ex) => Left(ex.toString)
      case Success(res) => res
    }
  }
}
