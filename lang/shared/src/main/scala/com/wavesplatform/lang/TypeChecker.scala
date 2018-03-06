package com.wavesplatform.lang

import cats.data._
import cats.syntax.all._
import com.wavesplatform.lang.Context.PredefType
import com.wavesplatform.lang.Terms._
import monix.eval.Coeval

import scala.util.{Failure, Success, Try}

object TypeChecker {

  type TypeDefs     = Map[String, TYPE]
  type FunctionSigs = Map[String, FUNCTION]
  case class TypeCheckerContext(predefTypes: Map[String, PredefType], varDefs: TypeDefs, functionDefs: FunctionSigs)

  object TypeCheckerContext {
    val empty = TypeCheckerContext(Map.empty, Map.empty, Map.empty)

    def fromContext(ctx: Context): TypeCheckerContext =
      TypeCheckerContext(
        predefTypes = ctx.typeDefs,
        varDefs = ctx.letDefs.mapValues(_.tpe),
        functionDefs = ctx.functions.mapValues(x => FUNCTION(x.args.map(y => y._2), x.resultType))
      )
  }

  type TypeResolutionError      = String
  type TypeCheckResult[T]       = Either[TypeResolutionError, T]
  private type SetTypeResult[T] = EitherT[Coeval, String, T]

  private def resolveTypes(typeWithParam: TYPE, map: Map[String, Either[String, TYPE]]): Either[String, TYPE] =
    typeWithParam match {
      case TYPEREF(name) =>
        map.getOrElse(name, Either.left[String, TYPE](s"Type param $name not found"))
      case OPTION(t) => resolveTypes(t, map).map(x => OPTION(x))
      case x         => Right(x)
    }

  private def setType(ctx: TypeCheckerContext, t: SetTypeResult[Untyped.EXPR]): SetTypeResult[Typed.EXPR] = t.flatMap {
    case x: Untyped.CONST_INT        => EitherT.pure(Typed.CONST_INT(x.t))
    case x: Untyped.CONST_BYTEVECTOR => EitherT.pure(Typed.CONST_BYTEVECTOR(x.bs))
    case Untyped.TRUE                => EitherT.pure(Typed.TRUE)
    case Untyped.FALSE               => EitherT.pure(Typed.FALSE)
    case Untyped.NONE                => EitherT.pure(Typed.NONE)

    case getter: Untyped.GETTER =>
      setType(ctx, EitherT.pure(getter.ref))
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

    case expr @ Untyped.FUNCTION_CALL(name, args) =>
      val value: EitherT[Coeval, String, Typed.EXPR] = ctx.functionDefs.get(name) match {
        case Some(FUNCTION(argTypes, resultType)) =>
          if (args.lengthCompare(argTypes.size) != 0)
            EitherT.fromEither[Coeval](Left(s"Function '$name' requires ${argTypes.size} arguments, but ${args.size} are provided"))
          else {
            import cats.instances.vector._
            val actualArgTypes: Vector[SetTypeResult[Typed.EXPR]] = args.map(arg => setType(ctx, EitherT.pure(arg))).toVector
            val sequencedActualArgTypes                           = actualArgTypes.sequence[SetTypeResult, Typed.EXPR].map(x => x.zip(argTypes))

            sequencedActualArgTypes.subflatMap { v: Seq[(Typed.EXPR, TYPE)] =>
              val typeParameters = v.flatMap(x => inferTypeParams(x._1.tpe, x._2))

              val resolvedTypes = typeParameters
                .groupBy { case (n, _) => n }
                .map(
                  g =>
                    (g._1,
                     g._2
                       .drop(1)
                       .foldLeft(Either.right[String, TYPE](g._2.head._2))((a, b) =>
                         a.flatMap(t1 =>
                           findCommonType(t1, b._2)
                             .toRight(s"There is no common type among (${g._2.map(_._2).mkString(", ")}) for ${g._1} type param")))))

              val matches = v.map(x => (x._1, resolveTypes(x._2, resolvedTypes)))
                .map {
                  case ((e, Right(tpe))) =>
                    matchType(tpe, e.tpe) match {
                      case Some(_) => Right(e)
                      case None =>
                        Left(
                          s"Types of arguments of function call '$name' do not match types required in signature. Expected: $tpe, Actual: ${e.tpe}")
                    }
                  case (_, Left(l)) => Left(l)
                }
              matches.find(_.isLeft) match {
                case Some(left) => left
                case None =>
                  resultType match {
                    case TYPEREF(n) =>
                      typeParameters.find(_._1 == n) match {
                        case Some(g) => Right(Typed.FUNCTION_CALL(name, v.map(_._1).toList, g._2))
                        case None    => Left(s"Type param $n not found")
                      }
                    case _ => Right(Typed.FUNCTION_CALL(name, v.map(_._1).toList, resultType))
                  }
              }
            }
          }
        case None => EitherT.fromEither[Coeval](Left(s"Function '$name' not found"))
      }
      value

    case expr @ Untyped.BINARY_OP(a, op, b) =>
      (setType(ctx, EitherT.pure(a)), setType(ctx, EitherT.pure(b))).tupled
        .subflatMap {
          case operands @ (a, b) =>
            val aTpe = a.tpe
            val bTpe = b.tpe

            op match {
              case SUM_OP =>
                if (aTpe != INT) Left(s"The first operand is expected to be INT, but got $aTpe: $a in $expr")
                else if (bTpe != INT) Left(s"The second operand is expected to be INT, but got $bTpe: $b in $expr")
                else Right(operands -> INT)

              case GT_OP | GE_OP =>
                if (aTpe != INT) Left(s"The first operand is expected to be INT, but got $aTpe: $a in $expr")
                else if (bTpe != INT) Left(s"The second operand is expected to be INT, but got $bTpe: $b in $expr")
                else Right(operands -> BOOLEAN)

              case AND_OP | OR_OP =>
                if (aTpe != BOOLEAN) Left(s"The first operand is expected to be BOOLEAN, but got $aTpe: $a in $expr")
                else if (bTpe != BOOLEAN) Left(s"The second operand is expected to be BOOLEAN, but got $bTpe: $b in $expr")
                else Right(operands -> BOOLEAN)

              case EQ_OP =>
                findCommonType(aTpe, bTpe) match {
                  case Some(_) => Right(operands -> BOOLEAN)
                  case None    => Left(s"Can't find common type for $aTpe and $bTpe: $a and $b in $expr")
                }
            }
        }
        .map { case (operands, tpe) => Typed.BINARY_OP(operands._1, op, operands._2, tpe) }

    case isDefined: Untyped.IS_DEFINED =>
      setType(ctx, EitherT.pure(isDefined.opt)).map(of => Typed.IS_DEFINED(of))

    case let: Untyped.LET =>
      setType(ctx, EitherT.pure(let.value)).map(value => Typed.LET(name = let.name, value = value))

    case block: Untyped.BLOCK =>
      block.let match {
        case None =>
          setType(ctx, EitherT.pure(block.body)).map { resolvedT =>
            Typed.BLOCK(let = None, body = resolvedT, tpe = resolvedT.tpe)
          }

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
      (setType(ctx, EitherT.pure(ifExpr.cond)), setType(ctx, EitherT.pure(ifExpr.ifTrue)), setType(ctx, EitherT.pure(ifExpr.ifFalse))).tupled
        .subflatMap[String, Typed.EXPR] {
          case (resolvedCond, resolvedIfTrue, resolvedIfFalse) =>
            val ifTrueTpe  = resolvedIfTrue.tpe
            val ifFalseTpe = resolvedIfFalse.tpe
            findCommonType(ifTrueTpe, ifFalseTpe) match {
              case Some(tpe) =>
                Right(
                  Typed.IF(
                    cond = resolvedCond,
                    ifTrue = resolvedIfTrue,
                    ifFalse = resolvedIfFalse,
                    tpe = tpe
                  ))
              case None => Left(s"Can't find common type for $ifTrueTpe and $ifFalseTpe")
            }
        }

    case ref: Untyped.REF =>
      EitherT.fromEither {
        ctx.varDefs
          .get(ref.key)
          .map { tpe =>
            Typed.REF(key = ref.key, tpe = tpe)
          }
          .toRight(s"A definition of '${ref.key}' is not found")
      }

    case get: Untyped.GET =>
      setType(ctx, EitherT.pure(get.opt)).subflatMap { expr =>
        expr.tpe match {
          case OPTION(in) => Right(Typed.GET(opt = expr, tpe = in))
          case x          => Left(s"GET called on $x, but only call on OPTION[_] is allowed")
        }
      }

    case some: Untyped.SOME => setType(ctx, EitherT.pure(some.t)).map(t => Typed.SOME(t = t, tpe = OPTION(t.tpe)))
  }

  def apply(c: TypeCheckerContext, expr: Untyped.EXPR): TypeCheckResult[Typed.EXPR] = {
    def result = setType(c, EitherT.pure(expr)).value().left.map { e =>
      s"Typecheck failed: $e"
    }
    Try(result) match {
      case Failure(ex)  => Left(ex.toString)
      case Success(res) => res
    }
  }
}
