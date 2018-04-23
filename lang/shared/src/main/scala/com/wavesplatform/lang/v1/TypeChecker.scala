package com.wavesplatform.lang.v1

import cats.data._
import cats.syntax.all._
import com.wavesplatform.lang.v1.FunctionHeader.FunctionHeaderType
import com.wavesplatform.lang.v1.Terms._
import com.wavesplatform.lang.v1.ctx.{Context, PredefType}
import monix.eval.Coeval

import scala.util.{Failure, Success, Try}

object TypeChecker {

  type TypeDefs     = Map[String, TYPE]
  type FunctionSigs = Map[String, Seq[FunctionTypeSignature]]
  case class TypeCheckerContext(predefTypes: Map[String, PredefType], varDefs: TypeDefs, functionDefs: FunctionSigs) {
    def functionTypeSignaturesByName(name: String): Seq[FunctionTypeSignature] = functionDefs.getOrElse(name, Seq.empty)
  }

  object TypeCheckerContext {
    val empty = TypeCheckerContext(Map.empty, Map.empty, Map.empty)

    def fromContext(ctx: Context): TypeCheckerContext = {
      val map = ctx.functions.values.groupBy(_.name).mapValues(_.map(_.signature).toSeq)
      TypeCheckerContext(predefTypes = ctx.typeDefs, varDefs = ctx.letDefs.mapValues(_.tpe), functionDefs = map)
    }
  }

  type TypeResolutionError      = String
  type TypeCheckResult[T]       = Either[TypeResolutionError, T]
  private type SetTypeResult[T] = EitherT[Coeval, String, T]

  private def setType(ctx: TypeCheckerContext, t: SetTypeResult[Untyped.EXPR]): SetTypeResult[Typed.EXPR] = t.flatMap {
    case x: Untyped.CONST_LONG       => EitherT.pure(Typed.CONST_LONG(x.value))
    case x: Untyped.CONST_BYTEVECTOR => EitherT.pure(Typed.CONST_BYTEVECTOR(x.value))
    case x: Untyped.CONST_STRING     => EitherT.pure(Typed.CONST_STRING(x.value))
    case Untyped.TRUE                => EitherT.pure(Typed.TRUE)
    case Untyped.FALSE               => EitherT.pure(Typed.FALSE)
    case Untyped.BINARY_OP(a, op, b) =>
      op match {
        case AND_OP => setType(ctx, EitherT.pure(Untyped.IF(a, b, Untyped.FALSE)))
        case OR_OP  => setType(ctx, EitherT.pure(Untyped.IF(a, Untyped.TRUE, b)))
        case _      => setType(ctx, EitherT.pure(Untyped.FUNCTION_CALL(opsToFunctions(op), List(a, b))))
      }

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

    case Untyped.FUNCTION_CALL(name, args) =>
      type ResolvedArgsResult = EitherT[Coeval, String, List[Typed.EXPR]]

      def resolvedArguments(args: List[Terms.Untyped.EXPR]): ResolvedArgsResult = {
        import cats.instances.list._
        val r: List[SetTypeResult[Typed.EXPR]] = args.map(arg => setType(ctx, EitherT.pure(arg)))(collection.breakOut)
        r.sequence[SetTypeResult, Typed.EXPR]
      }

      def matchOverload(resolvedArgs: List[Typed.EXPR], f: FunctionTypeSignature): Either[String, Typed.EXPR] = {
        val argTypes   = f.args
        val resultType = f.result
        if (args.lengthCompare(argTypes.size) != 0)
          Left(s"Function '$name' requires ${argTypes.size} arguments, but ${args.size} are provided")
        else {
          val typedExpressionArgumentsAndTypedPlaceholders: List[(Typed.EXPR, TYPEPLACEHOLDER)] = resolvedArgs.zip(argTypes)

          val typePairs = typedExpressionArgumentsAndTypedPlaceholders.map { case ((typedExpr, tph)) => (typedExpr.tpe, tph) }
          for {
            resolvedTypeParams <- TypeInferrer(typePairs)
            resolvedResultType <- TypeInferrer.inferResultType(resultType, resolvedTypeParams)
          } yield
            Typed.FUNCTION_CALL(
              FunctionHeader(name, f.args.map(FunctionHeaderType.fromTypePlaceholder)),
              typedExpressionArgumentsAndTypedPlaceholders.map(_._1),
              resolvedResultType
            )
        }
      }

      ctx.functionTypeSignaturesByName(name) match {
        case Nil                   => EitherT.fromEither[Coeval](Left(s"Function '$name' not found"))
        case singleOverload :: Nil => resolvedArguments(args).subflatMap(matchOverload(_, singleOverload))
        case many =>
          resolvedArguments(args).subflatMap { resolvedArgs =>
            val matchedSignatures = many
              .zip(many.map(matchOverload(resolvedArgs, _)))
              .collect {
                case (sig, result) if result.isRight => (sig, result)
              }

            matchedSignatures match {
              case Nil                       => Left(s"Can't find a function '$name'(${resolvedArgs.map(_.tpe.typeInfo).mkString(", ")})")
              case (_, oneFuncResult) :: Nil => oneFuncResult
              case manyPairs =>
                val candidates = manyPairs.map { case (sig, _) => s"'$name'(${sig.args.mkString(", ")})" }
                Left(s"Can't choose an overloaded function. Candidates: ${candidates.mkString("; ")}")
            }
          }
      }

    case block: Untyped.BLOCK =>
      import block.let
      (ctx.varDefs.get(let.name), ctx.functionDefs.get(let.name)) match {
        case (Some(_), _) => EitherT.leftT[Coeval, Typed.EXPR](s"Value '${let.name}' already defined in the scope")
        case (_, Some(_)) =>
          EitherT.leftT[Coeval, Typed.EXPR](s"Value '${let.name}' can't be defined because function with such name is predefined")
        case (None, None) =>
          setType(ctx, EitherT.pure(let.value)).flatMap { exprTpe =>
            val updatedCtx = ctx.copy(varDefs = ctx.varDefs + (let.name -> exprTpe.tpe))
            setType(updatedCtx, EitherT.pure(block.body))
              .map { inExpr =>
                Typed.BLOCK(
                  let = Typed.LET(let.name, exprTpe),
                  body = inExpr,
                  tpe = inExpr.tpe
                )
              }
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

  }

  def apply(c: TypeCheckerContext, expr: Untyped.EXPR): TypeCheckResult[Typed.EXPR] = {
    def result = setType(c, EitherT.pure(expr)).value().left.map { e =>
      s"Typecheck failed: $e"
    }
    Try(result) match {
      case Failure(ex)  => Left(ex.toString)
      case Success(res) => res
    }
    result

  }
}
