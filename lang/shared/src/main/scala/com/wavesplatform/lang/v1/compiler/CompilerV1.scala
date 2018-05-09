package com.wavesplatform.lang.v1.compiler

import cats.data._
import cats.syntax.all._
import com.wavesplatform.lang.ExprCompiler
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.directives.Directive
import com.wavesplatform.lang.v1.FunctionHeader.FunctionHeaderType
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.PredefFunction.FunctionTypeSignature
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.FunctionHeader
import monix.eval.Coeval

import scala.util.Try

class CompilerV1(ctx: CompilerContext) extends ExprCompiler {
  override type V = V1.type
  override val version: V = V1

  override def compile(input: String, directives: List[Directive]): Either[String, version.ExprT] = {
    Parser(input) match {
      case fastparse.core.Parsed.Success(value, _) =>
        CompilerV1(ctx, value) match {
          case Left(err)   => Left(err.toString)
          case Right(expr) => Right(expr)
        }
      case f @ fastparse.core.Parsed.Failure(_, _, _) => Left(f.toString)
    }
  }
}

object CompilerV1 {

  type TypeResolutionError      = String
  type CompilationResult[T]     = Either[TypeResolutionError, T]
  private type SetTypeResult[T] = EitherT[Coeval, String, T]

  private def setType(ctx: CompilerContext, t: SetTypeResult[Expressions.EXPR]): SetTypeResult[EXPR] = t.flatMap {
    case x: Expressions.CONST_LONG       => EitherT.pure(CONST_LONG(x.value))
    case x: Expressions.CONST_BYTEVECTOR => EitherT.pure(CONST_BYTEVECTOR(x.value))
    case x: Expressions.CONST_STRING     => EitherT.pure(CONST_STRING(x.value))
    case Expressions.TRUE                => EitherT.pure(TRUE)
    case Expressions.FALSE               => EitherT.pure(FALSE)
    case Expressions.BINARY_OP(a, op, b) =>
      op match {
        case AND_OP => setType(ctx, EitherT.pure(Expressions.IF(a, b, Expressions.FALSE)))
        case OR_OP  => setType(ctx, EitherT.pure(Expressions.IF(a, Expressions.TRUE, b)))
        case _      => setType(ctx, EitherT.pure(Expressions.FUNCTION_CALL(opsToFunctions(op), List(a, b))))
      }

    case getter: Expressions.GETTER =>
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

              fieldTpe.right.map(tpe => GETTER(ref = ref, field = getter.field, tpe = tpe))
            case x => Left(s"Can't access to '${getter.field}' of a primitive type $x")
          }
        }

    case Expressions.FUNCTION_CALL(name, args) =>
      type ResolvedArgsResult = EitherT[Coeval, String, List[EXPR]]

      def resolvedArguments(args: List[Expressions.EXPR]): ResolvedArgsResult = {
        import cats.instances.list._
        val r: List[SetTypeResult[EXPR]] = args.map(arg => setType(ctx, EitherT.pure(arg)))(collection.breakOut)
        r.sequence[SetTypeResult, EXPR]
      }

      def matchOverload(resolvedArgs: List[EXPR], f: FunctionTypeSignature): Either[String, EXPR] = {
        val argTypes   = f.args
        val resultType = f.result
        if (args.lengthCompare(argTypes.size) != 0)
          Left(s"Function '$name' requires ${argTypes.size} arguments, but ${args.size} are provided")
        else {
          val typedExpressionArgumentsAndTypedPlaceholders: List[(EXPR, TYPEPLACEHOLDER)] = resolvedArgs.zip(argTypes)

          val typePairs = typedExpressionArgumentsAndTypedPlaceholders.map { case ((typedExpr, tph)) => (typedExpr.tpe, tph) }
          for {
            resolvedTypeParams <- TypeInferrer(typePairs)
            resolvedResultType <- TypeInferrer.inferResultType(resultType, resolvedTypeParams)
          } yield
            FUNCTION_CALL(
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

    case block: Expressions.BLOCK =>
      import block.let
      (ctx.varDefs.get(let.name), ctx.functionDefs.get(let.name)) match {
        case (Some(_), _) => EitherT.leftT[Coeval, EXPR](s"Value '${let.name}' already defined in the scope")
        case (_, Some(_)) =>
          EitherT.leftT[Coeval, EXPR](s"Value '${let.name}' can't be defined because function with such name is predefined")
        case (None, None) =>
          setType(ctx, EitherT.pure(let.value)).flatMap { exprTpe =>
            val updatedCtx = ctx.copy(varDefs = ctx.varDefs + (let.name -> exprTpe.tpe))
            setType(updatedCtx, EitherT.pure(block.body))
              .map { inExpr =>
                BLOCK(
                  let = LET(let.name, exprTpe),
                  body = inExpr,
                  tpe = inExpr.tpe
                )
              }
          }
      }

    case ifExpr: Expressions.IF =>
      (setType(ctx, EitherT.pure(ifExpr.cond)), setType(ctx, EitherT.pure(ifExpr.ifTrue)), setType(ctx, EitherT.pure(ifExpr.ifFalse))).tupled
        .subflatMap[String, EXPR] {
          case (resolvedCond: EXPR, resolvedIfTrue, resolvedIfFalse) =>
            if (resolvedCond.tpe != BOOLEAN)
              Left(s"IF clause is expected to be BOOLEAN, acutal: ${resolvedCond.tpe}")
            else {
              val ifTrueTpe  = resolvedIfTrue.tpe
              val ifFalseTpe = resolvedIfFalse.tpe
              TypeInferrer.findCommonType(ifTrueTpe, ifFalseTpe) match {
                case Some(tpe) =>
                  Right(
                    IF(
                      cond = resolvedCond,
                      ifTrue = resolvedIfTrue,
                      ifFalse = resolvedIfFalse,
                      tpe = tpe
                    ))
                case None => Left(s"Can't find common type for $ifTrueTpe and $ifFalseTpe")
              }
            }
        }

    case ref: Expressions.REF =>
      EitherT.fromEither {
        ctx.varDefs
          .get(ref.key)
          .map { tpe =>
            REF(key = ref.key, tpe = tpe)
          }
          .toRight(s"A definition of '${ref.key}' is not found")
      }

  }

  def apply(c: CompilerContext, expr: Expressions.EXPR): CompilationResult[EXPR] = {
    def result = setType(c, EitherT.pure(expr)).value().left.map { e =>
      s"Typecheck failed: $e"
    }
    Try(result) match {
      case scala.util.Failure(ex)  => Left(ex.toString)
      case scala.util.Success(res) => res
    }
  }
}
