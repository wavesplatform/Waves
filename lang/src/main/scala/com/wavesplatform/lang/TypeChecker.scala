package com.wavesplatform.lang

import cats.data.OptionT
import cats.instances.either._
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

  private val pass: OptionT[TypeCheckResult, Unit] = OptionT.some[TypeCheckResult](())
  private def passWith[T](x: T): OptionT[TypeCheckResult, T] = OptionT.some[TypeCheckResult](x)
  private def fail(msg: String): OptionT[TypeCheckResult, Nothing] = OptionT.liftF[TypeCheckResult, Nothing](Left(msg))

  private def setType(ctx: Context, t: Expr): Coeval[TypeCheckResult[Expr]] = Coeval.defer(t match {
    case _: CONST_INT | _: CONST_BYTEVECTOR | TRUE | FALSE | NONE => Coeval(Right(t))

    case getter: GETTER =>
      setType(ctx, getter.ref)
        .map {
          case Right(ref@Block(_, _, exprType)) =>
            exprType match {
              case Some(typeRef: TYPEREF) =>
                val fieldTpe = ctx.predefTypes.get(typeRef.name)
                  .flatMap { ct =>
                    ct.fields.collectFirst {
                      case (fieldName, tpe) if fieldName == getter.field => tpe
                    }
                  }

                fieldTpe match {
                  case None => Left(s"Can't resolve type of field ${getter.field}")
                  case Some(tpe) => Right(getter.copy(ref = ref, exprType = Some(tpe)))
                }
              case Some(_) => Left(s"Reference has a primitive type, can't access to '${getter.field}'")
              case None => Left("Unresolved")
            }

          case Right(x) => Left(s"Getter on non-object expresssion: $x")
          case x => x
        }

    case sum: SUM =>
      for {
        resolvedFirst <- setType(ctx, sum.i1)
        resolvedSecond <- setType(ctx, sum.i2)
      } yield {
        val r = for {
          first <- OptionT.liftF(resolvedFirst)
          firstBlock <- first match {
            case x: Block => passWith(x)
            case _ => throw new IllegalStateException(s"Wrong processing of ${sum.i1}, got: $first")
          }

          second <- OptionT.liftF(resolvedSecond)
          secondBlock <- second match {
            case x: Block => passWith(x)
            case _ => throw new IllegalStateException(s"Wrong processing of ${sum.i2}, got: $second")
          }

          firstType <- OptionT.fromOption[TypeCheckResult](first.exprType)
          _ <- if (firstType == INT) pass else fail(s"The first operand is expected to be INT, but got $firstType, $second")

          secondType <- OptionT.fromOption[TypeCheckResult](second.exprType)
          _ <- if (secondType == INT) pass else fail(s"The second operand is expected to be INT, but got $secondType")
        } yield sum.copy(i1 = firstBlock, i2 = secondBlock)

        r.value.flatMap {
          case Some(x) => Right(x)
          case None => Left(s"Can't resolve types in $t")
        }
      }

    case and: AND =>
      for {
        resolvedFirst <- setType(ctx, and.t1)
        resolvedSecond <- setType(ctx, and.t2)
      } yield {
        val r = for {
          first <- OptionT.liftF(resolvedFirst)
          firstBlock <- first match {
            case x: Block => passWith(x)
            case _ => throw new IllegalStateException(s"Wrong processing of ${and.t1}, got: $first")
          }

          second <- OptionT.liftF(resolvedSecond)
          secondBlock <- second match {
            case x: Block => passWith(x)
            case _ => throw new IllegalStateException(s"Wrong processing of ${and.t2}, got: $second")
          }

          firstType <- OptionT.fromOption[TypeCheckResult](first.exprType)
          _ <- if (firstType == BOOLEAN) pass else fail(s"The first operand is expected to be BOOLEAN, but got $firstType")

          secondType <- OptionT.fromOption[TypeCheckResult](second.exprType)
          _ <- if (secondType == BOOLEAN) pass else fail(s"The second operand is expected to be BOOLEAN, but got $secondType")
        } yield and.copy(t1 = firstBlock, t2 = secondBlock)

        r.value.flatMap {
          case Some(x) => Right(x)
          case None => Left(s"Can't resolve types in $t")
        }
      }

    case or: OR =>
      for {
        resolvedFirst <- setType(ctx, or.t1)
        resolvedSecond <- setType(ctx, or.t2)
      } yield {
        val r = for {
          first <- OptionT.liftF(resolvedFirst)
          firstBlock <- first match {
            case x: Block => passWith(x)
            case _ => throw new IllegalStateException(s"Wrong processing of ${or.t1}, got: $first")
          }

          second <- OptionT.liftF(resolvedSecond)
          secondBlock <- second match {
            case x: Block => passWith(x)
            case _ => throw new IllegalStateException(s"Wrong processing of ${or.t2}, got: $second")
          }

          firstType <- OptionT.fromOption[TypeCheckResult](first.exprType)
          _ <- if (firstType == BOOLEAN) pass else fail(s"The first operand is expected to be BOOLEAN, but got $firstType")

          secondType <- OptionT.fromOption[TypeCheckResult](second.exprType)
          _ <- if (secondType == BOOLEAN) pass else fail(s"The second operand is expected to be BOOLEAN, but got $secondType")
        } yield or.copy(t1 = firstBlock, t2 = secondBlock)

        r.value.flatMap {
          case Some(x) => Right(x)
          case None => Left(s"Can't resolve types in $t")
        }
      }

    case eq: EQ =>
      for {
        resolvedFirst <- setType(ctx, eq.t1)
        resolvedSecond <- setType(ctx, eq.t2)
      } yield {
        val r = for {
          first <- OptionT.liftF(resolvedFirst)
          firstBlock <- first match {
            case x: Block => passWith(x)
            case _ => throw new IllegalStateException(s"Wrong processing of ${eq.t1}, got: $first")
          }

          second <- OptionT.liftF(resolvedSecond)
          secondBlock <- second match {
            case x: Block => passWith(x)
            case _ => throw new IllegalStateException(s"Wrong processing of ${eq.t2}, got: $second")
          }

          firstType <- OptionT.fromOption[TypeCheckResult](first.exprType)
          secondType <- OptionT.fromOption[TypeCheckResult](second.exprType)
          _ <- findCommonType(firstType, secondType) match {
            case None => fail(s"Can't find common type for $firstType and $secondType")
            case Some(x) => passWith(x)
          }
        } yield eq.copy(t1 = firstBlock, t2 = secondBlock)

        r.value.flatMap {
          case Some(x) => Right(x)
          case None => Left(s"Can't resolve types in $t")
        }
      }

    case gt: GT =>
      for {
        resolvedFirst <- setType(ctx, gt.t1)
        resolvedSecond <- setType(ctx, gt.t2)
      } yield {
        val r = for {
          first <- OptionT.liftF(resolvedFirst)
          firstBlock <- first match {
            case x: Block => passWith(x)
            case _ => throw new IllegalStateException(s"Wrong processing of ${gt.t1}, got: $first")
          }

          second <- OptionT.liftF(resolvedSecond)
          secondBlock <- second match {
            case x: Block => passWith(x)
            case _ => throw new IllegalStateException(s"Wrong processing of ${gt.t2}, got: $second")
          }

          firstType <- OptionT.fromOption[TypeCheckResult](first.exprType)
          secondType <- OptionT.fromOption[TypeCheckResult](second.exprType)
          _ <- if (firstType == secondType) pass else fail(s"Can't compare $firstType with $secondType")
        } yield gt.copy(t1 = firstBlock, t2 = secondBlock)

        r.value.flatMap {
          case Some(x) => Right(x)
          case None => Left(s"Can't resolve types in $t")
        }
      }

    case ge: GE =>
      for {
        resolvedFirst <- setType(ctx, ge.t1)
        resolvedSecond <- setType(ctx, ge.t2)
      } yield {
        val r = for {
          first <- OptionT.liftF(resolvedFirst)
          firstBlock <- first match {
            case x: Block => passWith(x)
            case _ => throw new IllegalStateException(s"Wrong processing of ${ge.t1}, got: $first")
          }

          second <- OptionT.liftF(resolvedSecond)
          secondBlock <- second match {
            case x: Block => passWith(x)
            case _ => throw new IllegalStateException(s"Wrong processing of ${ge.t2}, got: $second")
          }

          firstType <- OptionT.fromOption[TypeCheckResult](first.exprType)
          secondType <- OptionT.fromOption[TypeCheckResult](second.exprType)
          _ <- if (firstType == secondType) pass else fail(s"Can't compare $firstType with $secondType")
        } yield ge.copy(t1 = firstBlock, t2 = secondBlock)

        r.value.flatMap {
          case Some(x) => Right(x)
          case None => Left(s"Can't resolve types in $t")
        }
      }

    case sigVerify: SIG_VERIFY =>
      for {
        resolvedMessage <- setType(ctx, sigVerify.message)
        resolvedSignature <- setType(ctx, sigVerify.signature)
        resolvedPublicKey <- setType(ctx, sigVerify.publicKey)
      } yield {
        import cats.instances.either._
        import cats.syntax.all._
        (resolvedMessage, resolvedSignature, resolvedPublicKey).mapN {
          case (message: Block, signature: Block, publicKey: Block) =>
            sigVerify.copy(message = message, signature = signature, publicKey = publicKey)

          case _ => throw new IllegalStateException()
        }
      }

    case isDefined: IS_DEFINED =>
      setType(ctx, isDefined.t).map {
        case Right(of: Block) => Right(isDefined.copy(t = of))
        case Right(x) => throw new IllegalStateException(s"Wrong processing of ${isDefined.t}, got: $x")
        case x => x
      }

    case let: LET =>
      setType(ctx, let.value)
        .map {
          case Right(value: Block) => Right(let.copy(value = value))
          case Right(x) => throw new IllegalStateException(s"Wrong processing of ${let.value}, got: $x")
          case x => x
        }

    case block: Block =>
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
              case Left(e) => Coeval(Left(e))
              case Right(letExpr: LET) =>
                val updatedCtx = ctx.copy(varDefs = ctx.varDefs + (let.name -> letExpr.value.exprType.get))
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
              case Right(x) => Coeval(Left(s"Inferred '$x' during type check. Expected LET"))
            }
      }

    case ifExpr: IF =>
      for {
        resolvedCond <- setType(ctx, ifExpr.cond)
        resolvedTrueBranch <- setType(ctx, ifExpr.ifTrue)
        resolvedFalseBranch <- setType(ctx, ifExpr.ifFalse)
      } yield {
        val r = for {
          cond <- OptionT.liftF(resolvedCond)
          condBlock <- cond match {
            case x: Block => passWith(x)
            case _ => throw new IllegalStateException(s"Wrong processing of ${ifExpr.cond}, got: $resolvedCond")
          }

          trueBranch <- OptionT.liftF(resolvedTrueBranch)
          trueBranchBlock <- trueBranch match {
            case x: Block => passWith(x)
            case _ => throw new IllegalStateException(s"Wrong processing of ${ifExpr.ifTrue}, got: $trueBranch")
          }

          falseBranch <- OptionT.liftF(resolvedFalseBranch)
          falseBranchBlock <- falseBranch match {
            case x: Block => passWith(x)
            case _ => throw new IllegalStateException(s"Wrong processing of ${ifExpr.ifFalse}, got: $falseBranch")
          }

          trueBranchType <- OptionT.fromOption[TypeCheckResult](trueBranch.exprType)
          falseBranchType <- OptionT.fromOption[TypeCheckResult](falseBranch.exprType)
          common <- OptionT.liftF[TypeCheckResult, Type] {
            findCommonType(trueBranchType, falseBranchType)
              .map(Right(_))
              .getOrElse(Left(s"Can't find common type for $trueBranchType and $falseBranchType"))
          }
        } yield ifExpr.copy(
          cond = condBlock,
          ifTrue = trueBranchBlock,
          ifFalse = falseBranchBlock,
          exprType = Some(common)
        )

        r.value.flatMap {
          case Some(x) => Right(x)
          case None => Left(s"Can't resolve types in $t")
        }
      }

    case ref: REF => Coeval {
      ctx.varDefs
        .get(ref.key)
        .map { tpe => ref.copy(exprType = Some(tpe)) }
        .toRight(s"A definition of '${ref.key}' is not found")
    }

    case get: GET =>
      setType(ctx, get.t).map {
        case Right(expr: Block) =>
          expr.exprType match {
            case Some(OPTION(in)) => Right(get.copy(t = expr, exprType = Some(in)))
            case Some(x) => Left(s"GET called on $x, but only call on OPTION[_] is allowed")
            case None => Left(s"Can't resolve $expr")
          }
        case Right(expr) => throw new IllegalStateException(s"Wrong processing of ${get.t}, got: $expr")
        case x => x
      }

    case some: SOME =>
      setType(ctx, some.t)
        .map {
          case Right(resolvedT: Block) => Right(some.copy(t = resolvedT, exprType = resolvedT.exprType.map(OPTION)))
          case Right(x) => throw new IllegalStateException(s"Wrong processing of ${some.t}, got: $x")
          case x => x
        }
  })

  def apply(c: Context, expr: Expr): TypeCheckResult[Expr] = {
    val result = setType(c, expr)().left.map { e => s"Typecheck failed: $e" }
    Try(result) match {
      case Failure(ex) => Left(ex.toString)
      case Success(res) => res
    }
  }
}
