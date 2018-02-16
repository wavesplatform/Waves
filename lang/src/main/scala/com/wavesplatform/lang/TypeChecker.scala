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

  private def setType(ctx: Context, t: SetTypeResult[Expr]): SetTypeResult[Typed.Expr] = t.flatMap {
    case x: CONST_INT => EitherT.pure(Typed.CONST_INT(x.t))
    case x: CONST_BYTEVECTOR => EitherT.pure(Typed.CONST_BYTEVECTOR(x.bs))
    case TRUE => EitherT.pure(Typed.TRUE)
    case FALSE => EitherT.pure(Typed.FALSE)
    case NONE => EitherT.pure(Typed.NONE)

    case getter: GETTER =>
      setType(ctx, EitherT.pure(getter.ref))
        .map(_.asInstanceOf[Typed.Block])
        .subflatMap { ref =>
          ref.exprType match {
            case typeRef: TYPEREF =>
              val refTpe = ctx.predefTypes.get(typeRef.name).map(Right(_)).getOrElse(Left(s"Undefined type: ${typeRef.name}"))
              val fieldTpe = refTpe.flatMap { ct =>
                val fieldTpe = ct.fields.collectFirst {
                  case (fieldName, tpe) if fieldName == getter.field => tpe
                }

                fieldTpe.map(Right(_)).getOrElse(Left(s"Undefined field ${typeRef.name}.${getter.field}"))
              }

              fieldTpe.right.map(tpe => Typed.GETTER(ref = ref, field = getter.field, exprType = tpe))
            case x => Left(s"Can't access to '${getter.field}' of a primitive type $x")
          }
        }

    case sum: SUM =>
      (setType(ctx, EitherT.pure(sum.i1)), setType(ctx, EitherT.pure(sum.i2)))
        .mapN { (first, second) => Typed.SUM(i1 = first.asInstanceOf[Typed.Block], i2 = second.asInstanceOf[Typed.Block]) }
        .subflatMap { sum =>
          val firstType = sum.i1.exprType
          val secondType = sum.i2.exprType

          if (firstType != INT) Left(s"The first operand is expected to be INT, but got $firstType")
          else if (secondType != INT) Left(s"The second operand is expected to be INT, but got $secondType")
          else Right(sum)
        }

    case and: AND =>
      (setType(ctx, EitherT.pure(and.t1)), setType(ctx, EitherT.pure(and.t2)))
        .mapN { (first, second) => Typed.AND(t1 = first.asInstanceOf[Typed.Block], t2 = second.asInstanceOf[Typed.Block]) }
        .subflatMap { and =>
          val firstType = and.t1.exprType
          val secondType = and.t2.exprType

          if (firstType != BOOLEAN) Left(s"The first operand is expected to be BOOLEAN, but got $firstType")
          else if (secondType != BOOLEAN) Left(s"The second operand is expected to be BOOLEAN, but got $secondType")
          else Right(and)
        }

    case or: OR =>
      (setType(ctx, EitherT.pure(or.t1)), setType(ctx, EitherT.pure(or.t2)))
        .mapN { (first, second) => Typed.OR(t1 = first.asInstanceOf[Typed.Block], t2 = second.asInstanceOf[Typed.Block]) }
        .subflatMap { or =>
          val firstType = or.t1.exprType
          val secondType = or.t2.exprType

          if (firstType != BOOLEAN) Left(s"The first operand is expected to be BOOLEAN, but got $firstType")
          else if (secondType != BOOLEAN) Left(s"The second operand is expected to be BOOLEAN, but got $secondType")
          else Right(or)
        }

    case eq: EQ =>
      (setType(ctx, EitherT.pure(eq.t1)), setType(ctx, EitherT.pure(eq.t2)))
        .mapN { (first, second) => Typed.EQ(t1 = first.asInstanceOf[Typed.Block], t2 = second.asInstanceOf[Typed.Block]) }
        .subflatMap { eq =>
          val firstType = eq.t1.exprType
          val secondType = eq.t2.exprType

          findCommonType(firstType, secondType)
            .map(_ => Right(eq))
            .getOrElse(Left(s"Can't find common type for $firstType and $secondType"))
        }

    case gt: GT =>
      (setType(ctx, EitherT.pure(gt.t1)), setType(ctx, EitherT.pure(gt.t2)))
        .mapN { (first, second) => Typed.GT(t1 = first.asInstanceOf[Typed.Block], t2 = second.asInstanceOf[Typed.Block]) }
        .subflatMap { gt =>
          val firstType = gt.t1.exprType
          val secondType = gt.t2.exprType

          if (firstType != INT) Left(s"The first operand is expected to be INT, but got $firstType")
          else if (secondType != INT) Left(s"The second operand is expected to be INT, but got $secondType")
          else Right(gt)
        }

    case ge: GE =>
      (setType(ctx, EitherT.pure(ge.t1)), setType(ctx, EitherT.pure(ge.t2)))
        .mapN { (first, second) => Typed.GE(t1 = first.asInstanceOf[Typed.Block], t2 = second.asInstanceOf[Typed.Block]) }
        .subflatMap { ge =>
          val firstType = ge.t1.exprType
          val secondType = ge.t2.exprType

          if (firstType != INT) Left(s"The first operand is expected to be INT, but got $firstType")
          else if (secondType != INT) Left(s"The second operand is expected to be INT, but got $secondType")
          else Right(ge)
        }

    case sigVerify: SIG_VERIFY =>
      (setType(ctx, EitherT.pure(sigVerify.message)), setType(ctx, EitherT.pure(sigVerify.signature)), setType(ctx, EitherT.pure(sigVerify.publicKey)))
        .mapN { (resolvedMessage, resolvedSignature, resolvedPublicKey) =>
          Typed.SIG_VERIFY(
            message = resolvedMessage.asInstanceOf[Typed.Block],
            signature = resolvedSignature.asInstanceOf[Typed.Block],
            publicKey = resolvedPublicKey.asInstanceOf[Typed.Block]
          )
        }

    case isDefined: IS_DEFINED =>
      setType(ctx, EitherT.pure(isDefined.t))
        .map(_.asInstanceOf[Typed.Block])
        .map(of => Typed.IS_DEFINED(of))

    case let: LET =>
      setType(ctx, EitherT.pure(let.value))
        .map(_.asInstanceOf[Typed.Block])
        .map(value => Typed.LET(name = let.name, value = value))

    case block: Block =>
      block.let match {
        case None =>
          setType(ctx, EitherT.pure(block.t)).map { resolvedT => Typed.Block(let = None, t = resolvedT, exprType = resolvedT.exprType) }

        case Some(let) =>
          setType(ctx, EitherT.pure(let))
            .flatMap {
              case letExpr: Typed.LET =>
                val updatedCtx = ctx.copy(varDefs = ctx.varDefs + (let.name -> letExpr.value.exprType))
                setType(updatedCtx, EitherT.pure(block.t))
                  .map { inExpr =>
                    Typed.Block(
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
        .tupled
        .subflatMap[String, Typed.Expr] { case (resolvedCond, resolvedTrueBranch, resolvedFalseBranch) =>
        val ifTrueType = resolvedTrueBranch.exprType
        val ifFalseType = resolvedFalseBranch.exprType
        findCommonType(ifTrueType, ifFalseType)
          .map { tpe =>
            Right(Typed.IF(
              cond = resolvedCond.asInstanceOf[Typed.Block],
              ifTrue = resolvedTrueBranch.asInstanceOf[Typed.Block],
              ifFalse = resolvedFalseBranch.asInstanceOf[Typed.Block],
              exprType = tpe
            ))
          }
          .getOrElse(Left(s"Can't find common type for $ifTrueType and $ifFalseType"))
      }

    case ref: REF => EitherT.fromEither {
      ctx.varDefs
        .get(ref.key)
        .map { tpe => Typed.REF(key = ref.key, exprType = tpe) }
        .toRight(s"A definition of '${ref.key}' is not found")
    }

    case get: GET =>
      setType(ctx, EitherT.pure(get.t))
        .map(_.asInstanceOf[Typed.Block])
        .subflatMap { expr =>
          expr.exprType match {
            case OPTION(in) => Right(Typed.GET(t = expr, exprType = in))
            case x => Left(s"GET called on $x, but only call on OPTION[_] is allowed")
          }
        }

    case some: SOME =>
      setType(ctx, EitherT.pure(some.t))
        .map(_.asInstanceOf[Typed.Block])
        .map { t => Typed.SOME(t = t, exprType = OPTION(t.exprType)) }
  }

  def apply(c: Context, expr: Expr): TypeCheckResult[Typed.Expr] = {
    val result = setType(c, EitherT.pure(expr)).value().left.map { e => s"Typecheck failed: $e" }
    Try(result) match {
      case Failure(ex) => Left(ex.toString)
      case Success(res) => res
    }
  }
}
