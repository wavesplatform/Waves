package com.wavesplatform.lang

import cats.data.OptionT
import cats.instances.either._
import com.wavesplatform.lang.Terms._

import scala.util.{Failure, Success, Try}

object TypeChecker {

  import scala.util.control.TailCalls.{TailRec, done, tailcall}

  type Defs = Map[String, Type]

  case class Context(predefTypes: Map[String, CUSTOMTYPE], varDefs: Defs)
  object Context {
    val empty = Context(Map.empty, Map.empty)
  }

  type TypeResolutionError = String
  type TypeCheckResult[T] = Either[TypeResolutionError, T]

  def setType(ctx: Context, t: Expr): TailRec[TypeCheckResult[Expr]] = t match {
    case expr@REF(key, None) => done {
      ctx.varDefs
        .get(key)
        .map { tpe => expr.copy(exprType = Some(tpe)) }
        .toRight(s"Typecheck failed: a definition of '$key' is not found")
    }

    case block: Block => tailcall {
      block.let match {
        case None =>
          setType(ctx, block.t)
            .map {
              case Left(e) => Left(e)
              case Right(resolvedT) => Right(block.copy(t = resolvedT, exprType = resolvedT.exprType))
            }

        case Some(let) =>
          setType(ctx, let)
            .flatMap {
              case Left(e) => done(Left(e))
              case Right(letExpr: LET) =>
                val updatedCtx = ctx.copy(varDefs = ctx.varDefs + (let.name -> letExpr.exprType.get))
                setType(updatedCtx, block.t)
                  .map {
                    case Left(e) => Left(e)
                    case Right(inExpr) =>
                      Right(block.copy(
                        let = Some(letExpr),
                        t = inExpr,
                        exprType = inExpr.exprType
                      ))
                  }
              case Right(x) => done(Left(s"Inferred '$x' during type check. Expected LET"))
            }
      }
    }

    case expr: IF => tailcall {
      for {
        resolvedTrueBranch <- setType(ctx, expr.ifTrue)
        resolvedFalseBranch <- setType(ctx, expr.ifFalse)
      } yield {
        val r = for {
          trueBranch <- OptionT.liftF(resolvedTrueBranch)
          falseBranch <- OptionT.liftF(resolvedFalseBranch)
          trueBranchType <- OptionT.fromOption[TypeCheckResult](trueBranch.exprType)
          falseBranchType <- OptionT.fromOption[TypeCheckResult](falseBranch.exprType)
          common <- OptionT.liftF[TypeCheckResult, Type] {
            findCommonType(trueBranchType, falseBranchType)
              .map(Right(_))
              .getOrElse(Left(s"Typecheck failed for IF: RType($resolvedFalseBranch) differs from LType($resolvedTrueBranch)"))
          }
        } yield expr.copy(exprType = Some(common))

        r.value.flatMap {
          case Some(x) => Right(x)
          case None => Left(s"Can't resolve types in $t")
        }
      }
    }

    case EQ(left, right) => tailcall {
      for {
        resolvedLeft <- setType(ctx, left)
        resolvedRight <- setType(ctx, right)
      } yield {
        val r = for {
          left <- OptionT.liftF(resolvedLeft)
          right <- OptionT.liftF(resolvedRight)
          leftType <- OptionT.fromOption[TypeCheckResult](left.exprType)
          rightType <- OptionT.fromOption[TypeCheckResult](right.exprType)
          _ <- if (leftType == rightType) OptionT.some[TypeCheckResult](()) else
            OptionT.liftF[TypeCheckResult, Unit](Left(s"Typecheck failed for EQ: RType($leftType) differs from LType($rightType)"))
        } yield t

        r.value.flatMap {
          case Some(x) => Right(x)
          case None => Left(s"Can't resolve types in $t")
        }
      }
    }

    case get: GET => tailcall {
      setType(ctx, get.t)
        .map {
          case Right(expr) =>
            expr.exprType match {
              case Some(OPTION(in)) => Right(get.copy(exprType = Some(in)))
              case Some(x) => Left(s"Typecheck failed: GET called on $x, but only call on OPTION[_] is allowed")
              case None => Left(s"Can't resolve $expr")
            }
          case Left(err) => Left(s"Typecheck failed: $err")
        }
        .flatMap(done)
    }

    case some: SOME => tailcall {
      setType(ctx, some.t)
        .map {
          case Right(resolvedT) => Right(some.copy(t = resolvedT, exprType = resolvedT.exprType.map(OPTION)))
          case Left(err) => Left(s"Typecheck failed: $err")
        }
        .flatMap(done)
    }

    case getter: GETTER => tailcall {
      setType(ctx, getter.i)
        .map {
          case Right(ref: REF) =>
            ref.exprType match {
              case Some(typeRef: TYPEREF) =>
                val fieldTpe = ctx.predefTypes.get(typeRef.name)
                  .flatMap { ct =>
                    ct.fields.collectFirst {
                      case (fieldName, tpe) if fieldName == getter.field => tpe
                    }
                  }

                fieldTpe match {
                  case None => Left(s"Can't resolve type of field ${getter.field}")
                  case Some(tpe) => Right(getter.copy(exprType = Some(tpe)))
                }
              case Some(x) => Left(s"Reference has a primitive type, can't access to '${getter.field}'")
              case None => Left("Unresolved")
            }

          case Right(_) => Left(s"Getter on non-object expresssion")
          case Left(err) => Left(s"Typecheck failed: $err")
        }
        .flatMap(done)
    }

    case let: LET => tailcall {
      setType(ctx, let.value)
        .map {
          case Right(value: Block) => Right(let.copy(value = value))
          case Right(x) => Left(s"Expected Block, but received '$x'")
          case Left(err) => Left(s"Typecheck failed: $err")
        }
        .flatMap(done)
    }

    case x => done(Right(x))
  }

  def apply(c: Context, expr: Expr): TypeCheckResult[Expr] = {
    val result = setType(c, expr).result
    Try(result) match {
      case Failure(ex) => Left(ex.toString)
      case Success(res) => res
    }
  }
}
