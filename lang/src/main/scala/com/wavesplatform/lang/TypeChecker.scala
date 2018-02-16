package com.wavesplatform.lang

import cats.data.EitherT
import cats.syntax.all._
import com.wavesplatform.lang.Terms._
import monix.eval.Coeval

import scala.util.{Failure, Success, Try}

object TypeChecker {

  type Defs = Map[String, TYPE]

  case class Context(predefTypes: Map[String, CUSTOMTYPE], varDefs: Defs)
  object Context {
    val empty = Context(Map.empty, Map.empty)
  }

  type TypeResolutionError = String
  type TypeCheckResult[T] = Either[TypeResolutionError, T]
  private type SetTypeResult[T] = EitherT[Coeval, String, T]

  private def setType(ctx: Context, t: SetTypeResult[Untyped.EXPR]): SetTypeResult[Typed.EXPR] = t.flatMap {
    case x: Untyped.CONST_INT => EitherT.pure(Typed.CONST_INT(x.t))
    case x: Untyped.CONST_BYTEVECTOR => EitherT.pure(Typed.CONST_BYTEVECTOR(x.bs))
    case Untyped.TRUE => EitherT.pure(Typed.TRUE)
    case Untyped.FALSE => EitherT.pure(Typed.FALSE)
    case Untyped.NONE => EitherT.pure(Typed.NONE)

    case getter: Untyped.GETTER =>
      setType(ctx, EitherT.pure(getter.ref))
        .map(_.asInstanceOf[Typed.BLOCK])
        .subflatMap { ref =>
          ref.tpe match {
            case typeRef: TYPEREF =>
              val refTpe = ctx.predefTypes.get(typeRef.name).map(Right(_)).getOrElse(Left(s"Undefined type: ${typeRef.name}"))
              val fieldTpe = refTpe.flatMap { ct =>
                val fieldTpe = ct.fields.collectFirst {
                  case (fieldName, tpe) if fieldName == getter.field => tpe
                }

                fieldTpe.map(Right(_)).getOrElse(Left(s"Undefined field ${typeRef.name}.${getter.field}"))
              }

              fieldTpe.right.map(tpe => Typed.GETTER(ref = ref, field = getter.field, tpe = tpe))
            case x => Left(s"Can't access to '${getter.field}' of a primitive type $x")
          }
        }

    case sum: Untyped.SUM =>
      (setType(ctx, EitherT.pure(sum.a)), setType(ctx, EitherT.pure(sum.b)))
        .mapN { (first, second) => Typed.SUM(a = first.asInstanceOf[Typed.BLOCK], b = second.asInstanceOf[Typed.BLOCK]) }
        .subflatMap { sum =>
          val firstType = sum.a.tpe
          val secondType = sum.b.tpe

          if (firstType != INT) Left(s"The first operand is expected to be INT, but got $firstType")
          else if (secondType != INT) Left(s"The second operand is expected to be INT, but got $secondType")
          else Right(sum)
        }

    case and: Untyped.AND =>
      (setType(ctx, EitherT.pure(and.a)), setType(ctx, EitherT.pure(and.b)))
        .mapN { (first, second) => Typed.AND(a = first.asInstanceOf[Typed.BLOCK], b = second.asInstanceOf[Typed.BLOCK]) }
        .subflatMap { and =>
          val firstType = and.a.tpe
          val secondType = and.b.tpe

          if (firstType != BOOLEAN) Left(s"The first operand is expected to be BOOLEAN, but got $firstType")
          else if (secondType != BOOLEAN) Left(s"The second operand is expected to be BOOLEAN, but got $secondType")
          else Right(and)
        }

    case or: Untyped.OR =>
      (setType(ctx, EitherT.pure(or.a)), setType(ctx, EitherT.pure(or.b)))
        .mapN { (first, second) => Typed.OR(a = first.asInstanceOf[Typed.BLOCK], b = second.asInstanceOf[Typed.BLOCK]) }
        .subflatMap { or =>
          val firstType = or.a.tpe
          val secondType = or.b.tpe

          if (firstType != BOOLEAN) Left(s"The first operand is expected to be BOOLEAN, but got $firstType")
          else if (secondType != BOOLEAN) Left(s"The second operand is expected to be BOOLEAN, but got $secondType")
          else Right(or)
        }

    case eq: Untyped.EQ =>
      (setType(ctx, EitherT.pure(eq.a)), setType(ctx, EitherT.pure(eq.b)))
        .mapN { (first, second) => Typed.EQ(a = first.asInstanceOf[Typed.BLOCK], b = second.asInstanceOf[Typed.BLOCK]) }
        .subflatMap { eq =>
          val firstType = eq.a.tpe
          val secondType = eq.b.tpe

          findCommonType(firstType, secondType)
            .map(_ => Right(eq))
            .getOrElse(Left(s"Can't find common type for $firstType and $secondType"))
        }

    case gt: Untyped.GT =>
      (setType(ctx, EitherT.pure(gt.a)), setType(ctx, EitherT.pure(gt.b)))
        .mapN { (first, second) => Typed.GT(a = first.asInstanceOf[Typed.BLOCK], b = second.asInstanceOf[Typed.BLOCK]) }
        .subflatMap { gt =>
          val firstType = gt.a.tpe
          val secondType = gt.b.tpe

          if (firstType != INT) Left(s"The first operand is expected to be INT, but got $firstType")
          else if (secondType != INT) Left(s"The second operand is expected to be INT, but got $secondType")
          else Right(gt)
        }

    case ge: Untyped.GE =>
      (setType(ctx, EitherT.pure(ge.a)), setType(ctx, EitherT.pure(ge.b)))
        .mapN { (first, second) => Typed.GE(a = first.asInstanceOf[Typed.BLOCK], b = second.asInstanceOf[Typed.BLOCK]) }
        .subflatMap { ge =>
          val firstType = ge.a.tpe
          val secondType = ge.b.tpe

          if (firstType != INT) Left(s"The first operand is expected to be INT, but got $firstType")
          else if (secondType != INT) Left(s"The second operand is expected to be INT, but got $secondType")
          else Right(ge)
        }

    case sigVerify: Untyped.SIG_VERIFY =>
      (setType(ctx, EitherT.pure(sigVerify.message)), setType(ctx, EitherT.pure(sigVerify.signature)), setType(ctx, EitherT.pure(sigVerify.publicKey)))
        .mapN { (resolvedMessage, resolvedSignature, resolvedPublicKey) =>
          Typed.SIG_VERIFY(
            message = resolvedMessage.asInstanceOf[Typed.BLOCK],
            signature = resolvedSignature.asInstanceOf[Typed.BLOCK],
            publicKey = resolvedPublicKey.asInstanceOf[Typed.BLOCK]
          )
        }

    case isDefined: Untyped.IS_DEFINED =>
      setType(ctx, EitherT.pure(isDefined.opt))
        .map(_.asInstanceOf[Typed.BLOCK])
        .map(of => Typed.IS_DEFINED(of))

    case let: Untyped.LET =>
      setType(ctx, EitherT.pure(let.value))
        .map(_.asInstanceOf[Typed.BLOCK])
        .map(value => Typed.LET(name = let.name, value = value))

    case block: Untyped.BLOCK =>
      block.let match {
        case None =>
          setType(ctx, EitherT.pure(block.body)).map { resolvedT => Typed.BLOCK(let = None, body = resolvedT, tpe = resolvedT.tpe) }

        case Some(let) =>
          setType(ctx, EitherT.pure(let))
            .flatMap {
              case letExpr: Typed.LET =>
                val updatedCtx = ctx.copy(varDefs = ctx.varDefs + (let.name -> letExpr.value.tpe))
                setType(updatedCtx, EitherT.pure(block.body))
                  .map { inExpr =>
                    Typed.BLOCK(
                      let = Some(letExpr),
                      body = inExpr,
                      tpe = inExpr.tpe
                    )
                  }
              case x => EitherT.fromEither(Left(s"Inferred '$x' during type check. Expected LET"))
            }
      }

    case ifExpr: Untyped.IF =>
      (setType(ctx, EitherT.pure(ifExpr.cond)), setType(ctx, EitherT.pure(ifExpr.ifTrue)), setType(ctx, EitherT.pure(ifExpr.ifFalse)))
        .tupled
        .subflatMap[String, Typed.EXPR] { case (resolvedCond, resolvedTrueBranch, resolvedFalseBranch) =>
        val ifTrueType = resolvedTrueBranch.tpe
        val ifFalseType = resolvedFalseBranch.tpe
        findCommonType(ifTrueType, ifFalseType)
          .map { tpe =>
            Right(Typed.IF(
              cond = resolvedCond.asInstanceOf[Typed.BLOCK],
              ifTrue = resolvedTrueBranch.asInstanceOf[Typed.BLOCK],
              ifFalse = resolvedFalseBranch.asInstanceOf[Typed.BLOCK],
              tpe = tpe
            ))
          }
          .getOrElse(Left(s"Can't find common type for $ifTrueType and $ifFalseType"))
      }

    case ref: Untyped.REF => EitherT.fromEither {
      ctx.varDefs
        .get(ref.key)
        .map { tpe => Typed.REF(key = ref.key, tpe = tpe) }
        .toRight(s"A definition of '${ref.key}' is not found")
    }

    case get: Untyped.GET =>
      setType(ctx, EitherT.pure(get.opt))
        .map(_.asInstanceOf[Typed.BLOCK])
        .subflatMap { expr =>
          expr.tpe match {
            case OPTION(in) => Right(Typed.GET(opt = expr, tpe = in))
            case x => Left(s"GET called on $x, but only call on OPTION[_] is allowed")
          }
        }

    case some: Untyped.SOME =>
      setType(ctx, EitherT.pure(some.t))
        .map(_.asInstanceOf[Typed.BLOCK])
        .map { t => Typed.SOME(t = t, tpe = OPTION(t.tpe)) }
  }

  def apply(c: Context, expr: Untyped.EXPR): TypeCheckResult[Typed.EXPR] = {
    val result = setType(c, EitherT.pure(expr)).value().left.map { e => s"Typecheck failed: $e" }
    Try(result) match {
      case Failure(ex) => Left(ex.toString)
      case Success(res) => res
    }
  }
}
