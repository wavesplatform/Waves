package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.task.imports._
import cats.implicits._
import com.wavesplatform.lang.ExprCompiler
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.directives.Directive
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.parser.{BinaryOperation, Expressions, Parser}
import com.wavesplatform.lang.v1.parser.Expressions.{BINARY_OP, MATCH_CASE, PART}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.FunctionHeaderType
import com.wavesplatform.lang.v1.evaluator.ctx.{PredefBase, PredefCaseType, UnionType}
import com.wavesplatform.lang.v1.evaluator.ctx.PredefFunction.FunctionTypeSignature
import com.wavesplatform.lang.v1.parser.BinaryOperation.{AND_OP, OR_OP}
import UNION._
import com.wavesplatform.lang.v1.compiler.Errors.CompilationError
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import Errors._

class CompilerV1_1(ctx: CompilerContext) extends ExprCompiler {

  override type V = V1.type
  override val version: V = V1

  override def compile(input: String, directives: List[Directive]): Either[String, Terms.EXPR] = {
    Parser(input) match {
      case fastparse.core.Parsed.Success(xs, _) =>
        if (xs.size > 1) Left("Too many expressions")
        else if (xs.isEmpty) Left("No expression")
        else
          CompilerV1_1.compileExpr(xs.head).run(ctx).attempt.value match {
            case Left(err)               => Left(err.toString)
            case Right((_, Right(expr))) => Right(expr)
            case Right((_, Left(err)))   => Left(err)
          }
      case f @ fastparse.core.Parsed.Failure(_, _, _) => Left(f.toString)
    }
  }
}

object CompilerV1_1 {

  private def compileExpr(expr: Expressions.EXPR): CompileM[Terms.EXPR] = {
    expr match {
      case x: Expressions.CONST_LONG                   => (CONST_LONG(x.value): EXPR).pure[CompileM]
      case x: Expressions.CONST_BYTEVECTOR             => handlePart(x.value).map(CONST_BYTEVECTOR)
      case x: Expressions.CONST_STRING                 => handlePart(x.value).map(CONST_STRING)
      case _: Expressions.TRUE                         => (TRUE: EXPR).pure[CompileM]
      case _: Expressions.FALSE                        => (FALSE: EXPR).pure[CompileM]
      case Expressions.GETTER(_, _, ref, field)        => compileGetter(field, ref)
      case Expressions.BLOCK(_, _, let, body)          => compileBlock(let, body)
      case Expressions.IF(_, _, cond, ifTrue, ifFalse) => compileIf(cond, ifTrue, ifFalse)
      case Expressions.REF(_, _, key)                  => compileRef(key)
      case Expressions.FUNCTION_CALL(_, _, name, args) => compileFunctionCall(name, args)
      case Expressions.MATCH(_, _, expr, cases)        => compileMatch(expr, cases.toList)
      case Expressions.INVALID(start, end, message, _) => raiseError(Generic(s"$message in $start - $end"))
      case Expressions.BINARY_OP(start, end, a, op, b) =>
        op match {
          case AND_OP => compileIf(a, b, Expressions.FALSE(start, end))
          case OR_OP  => compileIf(a, Expressions.TRUE(start, end), b)
          case _      => compileFunctionCall(PART.VALID(start, end, BinaryOperation.opsToFunctions(op)), List(a, b))
        }
    }
  }

  private def compileIf(condExpr: Expressions.EXPR, ifTrueExpr: Expressions.EXPR, ifFalseExpr: Expressions.EXPR): CompileM[Terms.EXPR] = {
    (compileExpr(condExpr), compileExpr(ifTrueExpr), compileExpr(ifFalseExpr))
      .mapN(mkIf) >>= (_.fold(raiseError, _.pure[CompileM]))
  }

  def flat(typeDefs: Map[String, PredefBase], tl: List[String]): List[String] = {
    tl.flatMap { t =>
      typeDefs.get(t).fold(List(t)) {
        case UnionType(_, tl) => tl.map(_.name)
        case t                => List(t.name)
      }
    }
  }

  private def compileMatch(expr: Expressions.EXPR, cases: List[Expressions.MATCH_CASE]): CompileM[Terms.EXPR] = {
    for {
      ctx <- get[CompilerContext, CompilationError]
      typedExpr <- compileExpr(expr)
      exprTypes <- typedExpr.tpe match {
        case u: UNION => u.pure[CompileM]
        case _        => raiseError[CompilerContext, CompilationError, UNION](MatchOnlyUnion)
      }
      _ <- cases
        .flatMap(_.types)
        .traverse[CompileM, String](handlePart)
        .flatMap(tl => {
          val matchedTypes = UNION(flat(ctx.predefTypes, tl).map(CASETYPEREF))
          val exhausted    = (cases.last.types.isEmpty && (exprTypes >= matchedTypes)) || (exprTypes equivalent matchedTypes)

          if (exhausted) ().pure[CompileM]
          else raiseError[CompilerContext, CompilationError, Unit](MatchNotExhaustive(exprTypes.l, matchedTypes.l))
        })
      refTmpKey = "$match" + ctx.tmpArgsIdx
      refTmp    = Expressions.REF(1, 1, PART.VALID(1, 1, refTmpKey))
      _          <- modify[CompilerContext, CompilationError](c => c.copy(tmpArgsIdx = c.tmpArgsIdx + 1))
      updatedCtx <- get[CompilerContext, CompilationError]
      ifCases = mkIfCases(updatedCtx, cases, refTmp)
      compiledMatch <- compileBlock(Expressions.LET(1, 1, PART.VALID(1, 1, refTmpKey), expr, Seq.empty), ifCases)
    } yield compiledMatch
  }

  private def compileBlock(let: Expressions.LET, body: Expressions.EXPR): CompileM[Terms.EXPR] = {
    for {
      ctx <- get[CompilerContext, CompilationError]
      letName <- handlePart(let.name)
        .ensureOr(n => AlreadyDefined(n, false))(n => !ctx.varDefs.contains(n))
        .ensureOr(n => AlreadyDefined(n, true))(n => !ctx.functionDefs.contains(n))
      compiledLet <- compileExpr(let.value)
      letTypes <- let.types.toList
        .traverse[CompileM, String]((p: PART[String]) => handlePart[String](p))
        .ensureOr(lt => NonExistingType(letName, ctx.predefTypes.keys.toList))(tl =>
          tl.forall(ctx.predefTypes.contains))
      typeUnion = if (letTypes.isEmpty) compiledLet.tpe else UNION(letTypes.map(CASETYPEREF))
      _            <- modify[CompilerContext, CompilationError](c => c.copy(varDefs = c.varDefs + (letName -> typeUnion)))
      compiledBody <- compileExpr(body)
    } yield BLOCK(LET(letName, compiledLet), compiledBody, compiledBody.tpe)
  }

  private def compileGetter(fieldPart: PART[String], refExpr: Expressions.EXPR): CompileM[Terms.EXPR] = {
    for {
      ctx         <- get[CompilerContext, CompilationError]
      field       <- handlePart(fieldPart)
      compiledRef <- compileExpr(refExpr)
      result <- (compiledRef.tpe match {
        case CASETYPEREF(name) => mkGetter(ctx, name, field, compiledRef)
        case UNION(types)      => mkGetter(ctx, types, field, compiledRef)
      }).fold(raiseError[CompilerContext, CompilationError, EXPR], _.pure[CompileM])
    } yield result
  }

  private def compileFunctionCall(namePart: PART[String], args: List[Expressions.EXPR]): CompileM[EXPR] = {
    for {
      name         <- handlePart(namePart)
      signatures   <- get[CompilerContext, CompilationError].map(_.functionTypeSignaturesByName(name))
      compiledArgs <- args.traverse(compileExpr)
      compiledCalls = signatures
        .map(matchFuncOverload(name, args, compiledArgs, _))
        .collect({ case Right(expr) => expr })
      result <- compiledCalls match {
        case Nil         => raiseError[CompilerContext, CompilationError, EXPR](FunctionNotFound(name, compiledArgs.map(_.tpe.toString)))
        case call :: Nil => call.pure[CompileM]
        case _ => raiseError[CompilerContext, CompilationError, EXPR](AmbiguousOverloading(name, signatures.toList))
      }
    } yield result
  }

  private def compileRef(keyPart: PART[String]): CompileM[EXPR] = {
    for {
      key <- handlePart(keyPart)
      ctx <- get[CompilerContext, CompilationError]
      result <- ctx.varDefs
        .get(key)
        .fold(raiseError[CompilerContext, CompilationError, EXPR](DefNotFound(key)))(t => (REF(key, t): EXPR).pure[CompileM])
    } yield result
  }

  private def matchFuncOverload(funcName: String,
                                funcArgs: List[Expressions.EXPR],
                                resolvedArgs: List[EXPR],
                                f: FunctionTypeSignature): Either[CompilationError, EXPR] = {
    val argTypes = f.args
    if (funcArgs.lengthCompare(argTypes.size) != 0)
      Left(WrongArgumentsNumber(funcName, argTypes.size, funcArgs.size))
    else {
      val typedExpressionArgumentsAndTypedPlaceholders: List[(EXPR, TYPEPLACEHOLDER)] = resolvedArgs.zip(argTypes)

      val typePairs = typedExpressionArgumentsAndTypedPlaceholders.map { case (typedExpr, tph) => (typedExpr.tpe, tph) }
      for {
        resolvedTypeParams <- TypeInferrer(typePairs)
        resolvedResultType <- TypeInferrer.inferResultType(f.result, resolvedTypeParams)
      } yield
        FUNCTION_CALL(
          FunctionHeader(funcName, f.args.map(FunctionHeaderType.fromTypePlaceholder)),
          typedExpressionArgumentsAndTypedPlaceholders.map(_._1),
          resolvedResultType
        )
    }
  }.leftMap(Generic)

  def resolveFields(ctx: CompilerContext, tpe: PredefBase): List[(String, TYPE)] = tpe match {
    case PredefCaseType(_, fields) => fields
    case UnionType(_, types)       => types.map(n => resolveFields(ctx, ctx.predefTypes(n.name)).toSet).reduce(_ intersect _).toList
  }

  def mkIf(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR): Either[CompilationError, EXPR] = {
    if (cond.tpe != BOOLEAN)
      Left(UnexpectedType("BOOLEAN", cond.tpe.toString))
    else {
      TypeInferrer
        .findCommonType(ifTrue.tpe, ifFalse.tpe)
        .fold(UnexpectedType(ifTrue.tpe.toString, ifFalse.tpe.toString).asLeft[IF])(IF(cond, ifTrue, ifFalse, _).asRight)
    }
  }

  def mkIfCases(ctx: CompilerContext, cases: List[MATCH_CASE], refTmp: Expressions.REF): Expressions.EXPR = {
    cases.foldRight(Expressions.REF(1, 1, PART.VALID(1, 1, PureContext.errRef)): Expressions.EXPR) {
      case (mc, further) =>
        val blockWithNewVar = mc.newVarName match {
          case Some(newVal) => Expressions.BLOCK(1, 1, Expressions.LET(1, 1, newVal, refTmp, mc.types), mc.expr)
          case None         => mc.expr
        }
        mc.types.toList match {
          case Nil => blockWithNewVar
          case types =>
            def isInst(matchType: String): Expressions.EXPR =
              Expressions.FUNCTION_CALL(1,
                                        1,
                                        PART.VALID(1, 1, PureContext._isInstanceOf.name),
                                        List(refTmp, Expressions.CONST_STRING(1, 1, PART.VALID(1, 1, matchType))))
            val hType :: tTypes = flat(ctx.predefTypes, types.map(_.asInstanceOf[PART.VALID[String]].v))
            val typeIf = tTypes.foldLeft(isInst(hType)) {
              case (other, matchType) => BINARY_OP(1, 1, isInst(matchType), BinaryOperation.OR_OP, other)
            }

            val blockWithNewVar = mc.newVarName match {
              case Some(newVal) => Expressions.BLOCK(1, 1, Expressions.LET(1, 1, newVal, refTmp, mc.types), mc.expr)
              case None         => mc.expr
            }
            Expressions.IF(1, 1, typeIf, blockWithNewVar, further)
        }
    }
  }

  def mkGetter(ctx: CompilerContext, typeName: String, fieldName: String, expr: EXPR): Either[CompilationError, GETTER] = {
    for {
      refTpe <- ctx.predefTypes
        .get(typeName)
        .fold(TypeNotFound(typeName).asLeft[PredefBase])(_.asRight)
      fieldTpe <- resolveFields(ctx, refTpe)
        .collectFirst({ case (field, tpe) if fieldName == field => tpe })
        .fold(FieldNotFound(fieldName, typeName).asLeft[TYPE])(_.asRight)
    } yield GETTER(expr, fieldName, fieldTpe)
  }

  def mkGetter(ctx: CompilerContext, types: List[CASETYPEREF], fieldName: String, expr: EXPR): Either[CompilationError, GETTER] = {
    types
      .traverse[Option, Terms.TYPE](ctr => {
        ctx.predefTypes
          .get(ctr.name)
          .flatMap(resolveFields(ctx, _).find(_._1 == fieldName).map(_._2))
      })
      .fold(FieldNotFound(fieldName, s"Union($types)").asLeft[GETTER])(ts => {
        TypeInferrer.findCommonType(ts) match {
          case Some(ct) => GETTER(expr, fieldName, ct).asRight[CompilationError]
          case None     => UnexpectedType(s"UNION($types)", s"UNION($ts)").asLeft[GETTER]
        }
      })
  }

  private def handlePart[T](part: PART[T]): CompileM[T] = part match {
    case PART.VALID(_, _, x)               => x.pure[CompileM]
    case PART.INVALID(start, end, message) => raiseError(Generic(s"$message in $start-$end"))
  }
}
