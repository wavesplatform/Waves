package com.wavesplatform.lang.v1.compiler

import cats.data._
import cats.syntax.all._
import com.wavesplatform.lang.ExprCompiler
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.directives.Directive
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.FunctionHeaderType
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.PredefFunction.FunctionTypeSignature
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import com.wavesplatform.lang.v1.parser.Expressions.{BINARY_OP, MATCH_CASE}
import com.wavesplatform.lang.v1.parser.{BinaryOperation, Expressions, Parser}
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

  private def compile(ctx: CompilerContext, t: SetTypeResult[Expressions.EXPR]): SetTypeResult[EXPR] = t.flatMap {
    case x: Expressions.CONST_LONG       => EitherT.pure(CONST_LONG(x.value))
    case x: Expressions.CONST_BYTEVECTOR => EitherT.pure(CONST_BYTEVECTOR(x.value))
    case x: Expressions.CONST_STRING     => EitherT.pure(CONST_STRING(x.value))
    case Expressions.TRUE                => EitherT.pure(TRUE)
    case Expressions.FALSE               => EitherT.pure(FALSE)
    case getter: Expressions.GETTER      => compileGetter(ctx, getter)
    case fc: Expressions.FUNCTION_CALL   => compileFunctionCall(ctx, fc)
    case block: Expressions.BLOCK        => compileBlock(ctx, block)
    case ifExpr: Expressions.IF          => compileIf(ctx, ifExpr)
    case ref: Expressions.REF            => compileRef(ctx, ref)
    case m: Expressions.MATCH            => compileMatch(ctx, m)
    case Expressions.BINARY_OP(a, op, b) =>
      op match {
        case AND_OP => compileIf(ctx, Expressions.IF(a, b, Expressions.FALSE))
        case OR_OP  => compileIf(ctx, Expressions.IF(a, Expressions.TRUE, b))
        case _      => compileFunctionCall(ctx, Expressions.FUNCTION_CALL(opsToFunctions(op), List(a, b)))
      }
  }

  private def compileGetter(ctx: CompilerContext, getter: Expressions.GETTER): SetTypeResult[EXPR] =
    compile(ctx, EitherT.pure(getter.ref))
      .subflatMap { subExpr =>
        def getField(name: String): Either[String, GETTER] = {
          val refTpe = ctx.predefTypes.get(name).map(Right(_)).getOrElse(Left(s"Undefined type: $name"))
          val fieldTpe = refTpe.flatMap { ct =>
            val fieldTpe = ct.fields.collectFirst { case (fieldName, tpe) if fieldName == getter.field => tpe }
            fieldTpe.map(Right(_)).getOrElse(Left(s"Undefined field $name.${getter.field}"))
          }
          fieldTpe.right.map(tpe => GETTER(expr = subExpr, field = getter.field, tpe = tpe))
        }

        subExpr.tpe match {
          case typeRef: TYPEREF     => getField(typeRef.name)
          case typeRef: CASETYPEREF => getField(typeRef.name)
          case union: UNION =>
            val x1 = union.l
              .map(k => ctx.predefTypes(k.name))
              .map(predefType => predefType.fields.find(_._1 == getter.field))
            if (x1.contains(None)) Left(s"Undefined field ${getter.field} on $union")
            else
              TypeInferrer.findCommonType(x1.map(_.get._2)) match {
                case Some(cT) => Right(GETTER(expr = subExpr, field = getter.field, tpe = cT))
                case None     => Left(s"Undefined common type for field ${getter.field} on $union")
              }

          case x => Left(s"Can't access to '${getter.field}' of a primitive type $x")
        }
      }

  private def compileIf(ctx: CompilerContext, ifExpr: Expressions.IF): SetTypeResult[EXPR] =
    (compile(ctx, EitherT.pure(ifExpr.cond)), compile(ctx, EitherT.pure(ifExpr.ifTrue)), compile(ctx, EitherT.pure(ifExpr.ifFalse))).tupled
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

  private def compileFunctionCall(ctx: CompilerContext, fc: Expressions.FUNCTION_CALL): SetTypeResult[EXPR] = {
    val Expressions.FUNCTION_CALL(name, args) = fc
    type ResolvedArgsResult = EitherT[Coeval, String, List[EXPR]]

    def resolvedArguments(args: List[Expressions.EXPR]): ResolvedArgsResult = {
      import cats.instances.list._
      val r: List[SetTypeResult[EXPR]] = args.map(arg => compile(ctx, EitherT.pure(arg)))(collection.breakOut)
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
  }

  private def compileBlock(ctx: CompilerContext, block: Expressions.BLOCK): SetTypeResult[EXPR] = {
    import block.let
    (ctx.varDefs.get(let.name), ctx.functionDefs.get(let.name)) match {
      case (Some(_), _) => EitherT.leftT[Coeval, EXPR](s"Value '${let.name}' already defined in the scope")
      case (_, Some(_)) =>
        EitherT.leftT[Coeval, EXPR](s"Value '${let.name}' can't be defined because function with such name is predefined")
      case (None, None) =>
        for {
          exprTpe <- compile(ctx, EitherT.pure(let.value))
          _ <- EitherT
            .cond[Coeval](let.types.forall(ctx.predefTypes.contains), (), s"Value '${let.name}' declared as non-existing type")
          desiredUnion = if (let.types.isEmpty) exprTpe.tpe else UNION(let.types.toList.map(CASETYPEREF))
          updatedCtx   = ctx.copy(varDefs = ctx.varDefs + (let.name -> desiredUnion))
          inExpr <- compile(updatedCtx, EitherT.pure(block.body))
        } yield
          BLOCK(
            let = LET(let.name, exprTpe),
            body = inExpr,
            tpe = inExpr.tpe
          )
    }
  }

  private def compileRef(ctx: CompilerContext, ref: Expressions.REF): SetTypeResult[EXPR] = EitherT.fromEither {
    ctx.varDefs
      .get(ref.key)
      .map { tpe =>
        REF(key = ref.key, tpe = tpe)
      }
      .toRight(s"A definition of '${ref.key}' is not found")
  }

  private def compileMatch(ctx: CompilerContext, m: Expressions.MATCH): SetTypeResult[EXPR] = {
    val Expressions.MATCH(expr, cases) = m
    val rootMatchTmpArg                = "$match" + ctx.tmpArgsIdx
    val updatedCtx                     = ctx.copy(tmpArgsIdx = ctx.tmpArgsIdx + 1)
    def liftedCommonType(l: List[TYPE]): EitherT[Coeval, TypeResolutionError, TYPE] = TypeInferrer.findCommonType(l) match {
      case None     => EitherT.fromEither[Coeval](Left("No common type inferred for branches"))
      case Some(cT) => EitherT.fromEither[Coeval](Right(cT))
    }
    for {
      typedExpr <- compile(ctx, EitherT.pure(expr))
      u <- typedExpr.tpe match {
        case u: UNION => EitherT.fromEither[Coeval](Right(u))
        case _        => EitherT.fromEither[Coeval](Left("Only union type can be matched"))
      }
      matchingTypes = cases.flatMap(_.types)
      _ <- EitherT.cond[Coeval](UNION.eq(u, UNION(matchingTypes.toList.map(CASETYPEREF))),
                                (),
                                s"Matching not exhaustive: possibleTypes are ${u.l}, while matched are $matchingTypes")
      ifBasedCases: Expressions.EXPR = cases.foldRight(Expressions.REF("???"): Expressions.EXPR) {
        case (mc, further) =>
          val swarma = mc.types.foldLeft(Expressions.FALSE: Expressions.EXPR) {
            case (other, matchType) =>
              BINARY_OP(Expressions.FUNCTION_CALL("_isInstanceOf", List(Expressions.REF(rootMatchTmpArg), Expressions.CONST_STRING(matchType))),
                        BinaryOperation.OR_OP,
                        other)
          }

          Expressions.IF(swarma, mc.expr, further)

      }

      compiled <- compileBlock(updatedCtx, Expressions.BLOCK(Expressions.LET(rootMatchTmpArg, expr), ifBasedCases))

    } yield compiled
  }

  private def compileMatchCase(ctx: CompilerContext, mc: MATCH_CASE, matchRootRef: Expressions.REF): SetTypeResult[(MATCH_CASE, EXPR)] = {
    val tmpExpr: Expressions.EXPR = mc.newVarName match {
      case Some(nweVal) => Expressions.BLOCK(Expressions.LET(nweVal, matchRootRef, mc.types), mc.expr)
      case None         => mc.expr
    }
    compile(ctx, EitherT.pure(tmpExpr)).map((mc, _))
  }

  def apply(c: CompilerContext, expr: Expressions.EXPR): CompilationResult[EXPR] = {
    def result = compile(c, EitherT.pure(expr)).value().left.map { e =>
      s"Typecheck failed: $e"
    }
    Try(result) match {
      case scala.util.Failure(ex)  => Left(ex.toString)
      case scala.util.Success(res) => res
    }
  }
}
