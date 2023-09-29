package com.wavesplatform.lang.v1.compiler

import cats.implicits.*
import cats.{Id, Show}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.CommonError
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.compiler.CompilationError.*
import com.wavesplatform.lang.v1.compiler.CompilerContext.*
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1.*
import com.wavesplatform.lang.v1.evaluator.ctx.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.GlobalValNames
import com.wavesplatform.lang.v1.parser.BinaryOperation.*
import com.wavesplatform.lang.v1.parser.Expressions.{
  BINARY_OP,
  CompositePattern,
  ConstsPat,
  MATCH_CASE,
  ObjPat,
  PART,
  Pos,
  Single,
  TuplePat,
  Type,
  TypedVar
}
import com.wavesplatform.lang.v1.parser.Parser.LibrariesOffset
import com.wavesplatform.lang.v1.parser.{BinaryOperation, Expressions, Parser}
import com.wavesplatform.lang.v1.task.imports.*
import com.wavesplatform.lang.v1.{BaseGlobal, ContractLimits, FunctionHeader}

import java.nio.charset.StandardCharsets
import scala.util.Try

object ExpressionCompiler {
  private val global: BaseGlobal = com.wavesplatform.lang.Global

  case class CompilationStepResultExpr(
      ctx: CompilerContext,
      expr: Terms.EXPR,
      t: FINAL,
      parseNodeExpr: Expressions.EXPR,
      errors: Iterable[CompilationError] = Iterable.empty
  )

  case class CompilationStepResultDec(
      ctx: CompilerContext,
      dec: Terms.DECLARATION,
      t: FINAL,
      parseNodeExpr: Expressions.Declaration,
      errors: Iterable[CompilationError] = Iterable.empty
  )

  def compile(
      input: String,
      offset: LibrariesOffset,
      ctx: CompilerContext,
      version: StdLibVersion,
      allowIllFormedStrings: Boolean = false
  ): Either[String, (EXPR, FINAL)] = {
    val parser = new Parser(version)(offset)
    parser.parseExpr(input) match {
      case fastparse.Parsed.Success(xs, _) => ExpressionCompiler(ctx, xs, allowIllFormedStrings)
      case f: fastparse.Parsed.Failure     => Left(parser.toString(input, f))
    }
  }

  def compileBoolean(input: String, offset: LibrariesOffset, ctx: CompilerContext, version: StdLibVersion): Either[String, EXPR] = {
    compile(input, offset, ctx, version).flatMap {
      case (expr, BOOLEAN) => Right(expr)
      case _               => Left("Script should return boolean")
    }
  }

  def compileUntyped(input: String, offset: LibrariesOffset, ctx: CompilerContext, version: StdLibVersion): Either[String, EXPR] = {
    compile(input, offset, ctx, version)
      .map { case (expr, _) => expr }
  }

  def compileWithParseResult(
      input: String,
      offset: LibrariesOffset,
      ctx: CompilerContext,
      version: StdLibVersion,
      saveExprContext: Boolean = true
  ): Either[(String, Int, Int), (EXPR, Expressions.SCRIPT, Iterable[CompilationError])] =
    new Parser(version)(offset)
      .parseExpressionWithErrorRecovery(input)
      .flatMap { case (parseResult, removedCharPosOpt) =>
        compileExprWithCtx(parseResult.expr, saveExprContext, allowIllFormedStrings = false)
          .run(ctx)
          .value
          ._2
          .map { compRes =>
            val errorList =
              compRes.errors ++
                (if (compRes.t equivalent BOOLEAN) Nil else List(Generic(0, 0, "Script should return boolean"))) ++
                (if (removedCharPosOpt.isEmpty)
                   Nil
                 else
                   List(
                     Generic(
                       removedCharPosOpt.get.start,
                       removedCharPosOpt.get.end,
                       "Parsing failed. Some chars was removed as result of recovery process."
                     )
                   ))
            (compRes.expr, parseResult.copy(expr = compRes.parseNodeExpr), errorList)
          }
          .leftMap(e => (s"Compilation failed: ${Show[CompilationError].show(e)}", e.start, e.end))
      }

  def compileDecls(input: String, offset: LibrariesOffset, ctx: CompilerContext, version: StdLibVersion): Either[String, EXPR] = {
    val adjustedDecls = s"$input\n${GlobalValNames.Unit}"
    compileUntyped(adjustedDecls, offset, ctx, version)
  }

  private def compileExpr(expr: Expressions.EXPR): CompileM[(Terms.EXPR, FINAL, Expressions.EXPR, Iterable[CompilationError])] =
    compileExprWithCtx(expr, allowIllFormedStrings = false).map(r => (r.expr, r.t, r.parseNodeExpr, r.errors))

  private def compileExprWithCtx(
      expr: Expressions.EXPR,
      saveExprContext: Boolean = false,
      allowIllFormedStrings: Boolean
  ): CompileM[CompilationStepResultExpr] = {
    get[Id, CompilerContext, CompilationError].flatMap { ctx =>
      def adjustByteStr(expr: Expressions.CONST_BYTESTR, b: ByteStr) =
        CONST_BYTESTR(b)
          .leftMap(e => CompilationError.Generic(expr.position.start, expr.position.end, e.message))
          .map(CompilationStepResultExpr(ctx, _, BYTESTR, expr))
          .recover { case err => CompilationStepResultExpr(ctx, FAILED_EXPR(), NOTHING, expr, List(err)) }

      def adjustStr(expr: Expressions.CONST_STRING, str: String): Either[CompilationError, CompilationStepResultExpr] =
        CONST_STRING(str)
          .filterOrElse(_ => allowIllFormedStrings || !global.isIllFormed(str), CommonError(s"String '$str' contains ill-formed characters"))
          .leftMap(e => CompilationError.Generic(expr.position.start, expr.position.end, e.message))
          .map(CompilationStepResultExpr(ctx, _, STRING, expr))
          .recover { case err => CompilationStepResultExpr(ctx, FAILED_EXPR(), NOTHING, expr, List(err)) }

      expr match {
        case x: Expressions.CONST_LONG    => CompilationStepResultExpr(ctx, CONST_LONG(x.value), LONG, x).pure[CompileM]
        case x: Expressions.CONST_BYTESTR => handlePart(x.value).flatMap(b => liftEither(adjustByteStr(x, b)))
        case x: Expressions.CONST_STRING  => handlePart(x.value).flatMap(s => liftEither(adjustStr(x, s)))
        case x: Expressions.TRUE          => CompilationStepResultExpr(ctx, TRUE, BOOLEAN, x).pure[CompileM]
        case x: Expressions.FALSE         => CompilationStepResultExpr(ctx, FALSE, BOOLEAN, x).pure[CompileM]

        case x: Expressions.INVALID =>
          CompilationStepResultExpr(
            ctx,
            FAILED_EXPR(),
            NOTHING,
            x: Expressions.EXPR,
            List(Generic(x.position.start, x.position.end, x.message))
          ).pure[CompileM]

        case Expressions.GETTER(p, ref, field, _, _, c)     => compileGetter(p, field, ref, saveExprContext, allowIllFormedStrings, c)
        case Expressions.BLOCK(p, dec, body, _, _)          => compileBlock(p, dec, body, saveExprContext, allowIllFormedStrings)
        case Expressions.IF(p, cond, ifTrue, ifFalse, _, _) => compileIf(p, cond, ifTrue, ifFalse, saveExprContext, allowIllFormedStrings)
        case Expressions.REF(p, key, _, _)                  => compileRef(p, key, saveExprContext)
        case Expressions.FUNCTION_CALL(p, name, args, _, _) => compileFunctionCall(p, name, args, saveExprContext, allowIllFormedStrings)
        case Expressions.MATCH(p, ex, cases, _, _)          => compileMatch(p, ex, cases.toList, saveExprContext, allowIllFormedStrings)
        case f: Expressions.FOLD                            => compileFold(f)
        case Expressions.GENERIC_FUNCTION_CALL(p, e, name, t, _, _) =>
          compileGenericFunctionCall(p, e, name, t, saveExprContext, allowIllFormedStrings)
        case Expressions.BINARY_OP(p, a, op, b, _, _) =>
          op match {
            case AND_OP => compileIf(p, a, b, Expressions.FALSE(p), saveExprContext, allowIllFormedStrings)
            case OR_OP  => compileIf(p, a, Expressions.TRUE(p), b, saveExprContext, allowIllFormedStrings)
            case _ => compileFunctionCall(p, PART.VALID(p, BinaryOperation.opsToFunctions(op)), List(a, b), saveExprContext, allowIllFormedStrings)
          }
      }
    }
  }

  private def compileIf(
      p: Pos,
      condExpr: Expressions.EXPR,
      ifTrueExpr: Expressions.EXPR,
      ifFalseExpr: Expressions.EXPR,
      saveExprContext: Boolean,
      allowIllFormedStrings: Boolean
  ): CompileM[CompilationStepResultExpr] =
    for {
      condWithErr <- local {
        compileExprWithCtx(condExpr, saveExprContext, allowIllFormedStrings).map { condCompRes =>
          val error = Some(UnexpectedType(p.start, p.end, BOOLEAN.toString, condCompRes.t.toString)).filter(_ => !(condCompRes.t equivalent BOOLEAN))
          (condCompRes, error)
        }
      }
      ifTrue  <- local(compileExprWithCtx(ifTrueExpr, saveExprContext, allowIllFormedStrings))
      ifFalse <- local(compileExprWithCtx(ifFalseExpr, saveExprContext, allowIllFormedStrings))

      ctx = ifFalse.ctx
      t   = TypeInferrer.findCommonType(ifTrue.t, ifFalse.t, mergeTuples = false)
      parseNodeExpr = Expressions.IF(
        p,
        condWithErr._1.parseNodeExpr,
        ifTrue.parseNodeExpr,
        ifFalse.parseNodeExpr,
        Some(t),
        saveExprContext.toOption(ctx)
      )
      errorList = condWithErr._1.errors ++ ifTrue.errors ++ ifFalse.errors

      result =
        if (condWithErr._2.isEmpty) {
          CompilationStepResultExpr(
            ctx,
            IF(condWithErr._1.expr, ifTrue.expr, ifFalse.expr),
            t,
            parseNodeExpr,
            errorList
          )
        } else {
          CompilationStepResultExpr(ctx, FAILED_EXPR(), NOTHING, parseNodeExpr, errorList ++ condWithErr._2.map(List(_)).get)
        }
    } yield result

  private def flatSingle(
      pos: Pos,
      typeDefs: Map[String, FINAL],
      expectedTypes: List[String],
      varName: Option[String],
      typeName: String
  ): Either[CompilationError, List[FINAL]] =
    typeDefs.get(typeName) match {
      case Some(UNION(unionTypes, _)) => Right(unionTypes)
      case Some(realType)             => Right(List(realType))
      case None =>
        Left {
          TypeNotFound(pos.start, pos.end, typeName, expectedTypes, varName)
        }
    }

  private def findGenericType(p: Pos, t: String): Either[CompilationError, FINAL => FINAL] =
    t match {
      case Type.ListTypeName => Right(LIST)
      case _                 => Left(GenericTypeNotFound(p.start, p.end, t))
    }

  private def compileMatch(
      p: Pos,
      expr: Expressions.EXPR,
      cases: List[Expressions.MATCH_CASE],
      saveExprContext: Boolean,
      allowIllFormedStrings: Boolean
  ): CompileM[CompilationStepResultExpr] =
    for {
      ctx <- get[Id, CompilerContext, CompilationError]
      _ <- {
        val types = ctx.predefTypes.keySet
        val typeNamedCases =
          cases.collect {
            case MATCH_CASE(_, TypedVar(Some(PART.VALID(_, name)), _), _, _, _) if types.contains(name) => name
          }

        Either
          .cond(
            typeNamedCases.isEmpty,
            (),
            TypeNamedCases(p.start, p.end, typeNamedCases)
          )
          .toCompileM
      }
      _ <- {
        val defaultCasesCount = cases.count {
          case MATCH_CASE(_, TypedVar(_, t), _, _, _) => t.isEmpty
          case _                                      => false
        }
        Either
          .cond(
            defaultCasesCount < 2,
            (),
            MultipleDefaultCases(p.start, p.end, defaultCasesCount)
          )
          .toCompileM
      }
      typedExpr <- compileExprWithCtx(expr, saveExprContext, allowIllFormedStrings)
      exprTypes = typedExpr.t
      tmpArgId  = ctx.tmpArgsIdx
      refTmpKey = "$match" + tmpArgId
      _ <- set[Id, CompilerContext, CompilationError](ctx.copy(tmpArgsIdx = tmpArgId + 1))
      allowShadowVarName = typedExpr.expr match {
        case REF(k) => Some(k)
        case _      => None
      }
      matchTypes <- cases.traverse {
        case MATCH_CASE(_, TypedVar(_, t), _, _, _)  => handleCompositeType(p, t, Some(exprTypes), allowShadowVarName)
        case MATCH_CASE(_, ObjPat(_, t, _), _, _, _) => handleCompositeType(p, t, Some(exprTypes), allowShadowVarName)
        case _                                       => (NOTHING: FINAL).pure[CompileM]
      }
      defaultType = exprTypes match {
        case ANY          => ANY
        case UNION(tl, _) => UNION(tl.filter(t => !matchTypes.contains(t)), None)
        case _            => NOTHING
      }
      ifCasesWithErr <- inspectFlat[Id, CompilerContext, CompilationError, Expressions.EXPR](updatedCtx => {
        val ref = Expressions.REF(p, PART.VALID(p, refTmpKey), ctxOpt = saveExprContext.toOption(updatedCtx))
        mkIfCases(cases, matchTypes, ref, defaultType, allowShadowVarName, updatedCtx).toCompileM
      }).handleError()
      compiledMatch <- compileLetBlock(
        p,
        Expressions.LET(p, PART.VALID(p, refTmpKey), expr),
        ifCasesWithErr._1.getOrElse(
          Expressions.INVALID(
            p,
            ifCasesWithErr._2.map(e => Show[CompilationError].show(e)).mkString("\n"),
            ctxOpt = saveExprContext.toOption(ctx)
          )
        ),
        saveExprContext,
        allowIllFormedStrings
      )
      checktypes =
        if (matchTypes.contains(LIST(ANY))) {
          (
            matchTypes.filter(_ != LIST(ANY)),
            UNION.create((exprTypes match {
              case ANY => List(ANY)
              case t   => t.typeList
            }).filter {
              case LIST(_) => false
              case _       => true
            })
          )
        } else {
          (matchTypes, exprTypes)
        }
      matchedTypesUnion = UNION.create(checktypes._1)
      checkWithErr <- Either
        .cond(
          (cases.last.pattern.isRest && (checktypes._2 >= matchedTypesUnion)) || (checktypes._2 equivalent matchedTypesUnion),
          (),
          MatchNotExhaustive(p.start, p.end, exprTypes.typeList, matchTypes)
        )
        .toCompileM
        .handleError()
      _ <- set[Id, CompilerContext, CompilationError](ctx.copy(tmpArgsIdx = tmpArgId))

      errorList = ifCasesWithErr._2 ++ compiledMatch.errors ++ checkWithErr._2

      result =
        if (errorList.isEmpty) {
          compiledMatch.copy(errors = compiledMatch.errors ++ typedExpr.errors)
        } else {
          CompilationStepResultExpr(
            ctx,
            FAILED_EXPR(),
            NOTHING,
            Expressions.MATCH(p, typedExpr.parseNodeExpr, cases, ctxOpt = saveExprContext.toOption(ctx)),
            errorList ++ typedExpr.errors
          )
        }
    } yield result

  private def exprContainsRef(expr: Expressions.EXPR, ref: String): Boolean =
    expr match {
      case Expressions.GETTER(_, expr, _, _, _, _) =>
        exprContainsRef(expr, ref)

      case Expressions.BLOCK(_, decl, body, _, _) =>
        val refIsOverlappedByDecl =
          decl.name match {
            case PART.VALID(_, name) if name == ref => true
            case _                                  => false
          }
        if (refIsOverlappedByDecl) false
        else {
          val declContainsRef =
            decl match {
              case Expressions.LET(_, _, value, _, _) =>
                exprContainsRef(value, ref)
              case Expressions.FUNC(_, expr, _, args) =>
                val refIsOverlappedByArg =
                  args.exists {
                    case (PART.VALID(_, name), _) if name == ref => true
                    case _                                       => false
                  }
                if (!refIsOverlappedByArg) exprContainsRef(expr, ref)
                else false
            }
          declContainsRef || exprContainsRef(body, ref)
        }

      case Expressions.IF(_, cond, ifTrue, ifFalse, _, _) =>
        exprContainsRef(cond, ref) ||
          exprContainsRef(ifTrue, ref) ||
          exprContainsRef(ifFalse, ref)

      case Expressions.FUNCTION_CALL(_, _, args, _, _) =>
        args.exists(exprContainsRef(_, ref))

      case Expressions.REF(_, PART.VALID(_, name), _, _) if name == ref =>
        true

      case BINARY_OP(_, a, _, b, _, _) =>
        exprContainsRef(a, ref) || exprContainsRef(b, ref)

      case Expressions.MATCH(_, matchingExpr, cases, _, _) =>
        exprContainsRef(matchingExpr, ref) ||
          cases.exists {
            case MATCH_CASE(_, TypedVar(Some(PART.VALID(_, varName)), _), caseExpr, _, _) if varName != ref =>
              exprContainsRef(caseExpr, ref)
            case MATCH_CASE(_, TypedVar(None, _), caseExpr, _, _) =>
              exprContainsRef(caseExpr, ref)
            case _ => false
          }

      case _ => false
    }

  def compileBlock(
      pos: Expressions.Pos,
      declaration: Expressions.Declaration,
      expr: Expressions.EXPR,
      saveExprContext: Boolean,
      allowIllFormedStrings: Boolean
  ): CompileM[CompilationStepResultExpr] =
    declaration match {
      case l: Expressions.LET  => compileLetBlock(pos, l, expr, saveExprContext, allowIllFormedStrings)
      case f: Expressions.FUNC => compileFuncBlock(pos, f, expr, saveExprContext, allowIllFormedStrings)
    }

  private def validateShadowing(p: Pos, dec: Expressions.Declaration, allowedExceptions: List[String] = List.empty): CompileM[String] = {
    for {
      ctx <- get[Id, CompilerContext, CompilationError]
      letName <- handlePart(dec.name)
        .ensureOr(n => AlreadyDefined(p.start, p.end, n, isFunction = false))(n =>
          !ctx.varDefs.contains(n) || dec.allowShadowing || allowedExceptions.contains(n)
        )
        .ensureOr(n => AlreadyDefined(p.start, p.end, n, isFunction = true))(n => !ctx.functionDefs.contains(n))
    } yield letName
  }

  private def checkDeclarationNameSize(p: Pos, decl: Expressions.Declaration): CompileM[String] =
    handlePart(decl.name).ensure(
      TooLongDeclarationName(p.start, p.end, decl)
    )(
      _.getBytes(StandardCharsets.UTF_8).length <= ContractLimits.MaxDeclarationNameInBytes
    )

  def compileLet(p: Pos, let: Expressions.LET, saveExprContext: Boolean, allowIllFormedStrings: Boolean): CompileM[CompilationStepResultDec] =
    for {
      _              <- checkDeclarationNameSize(p, let)
      letNameWithErr <- validateShadowing(p, let).handleError()
      compiledLet    <- compileExprWithCtx(let.value, saveExprContext, allowIllFormedStrings)
      ctx            <- get[Id, CompilerContext, CompilationError]

      letType       = let.types.getOrElse(compiledLet.t)
      errorList     = letNameWithErr._2
      parseNodeDecl = let.copy(value = compiledLet.parseNodeExpr)

      result =
        if (errorList.isEmpty) {
          CompilationStepResultDec(ctx, LET(letNameWithErr._1.get, compiledLet.expr), letType, parseNodeDecl, compiledLet.errors)
        } else {
          CompilationStepResultDec(ctx, FAILED_DEC(), letType, parseNodeDecl, errorList ++ compiledLet.errors)
        }
    } yield result

  def compileFunc(
      p: Pos,
      func: Expressions.FUNC,
      saveExprContext: Boolean,
      annListVars: List[String] = List.empty,
      allowIllFormedStrings: Boolean
  ): CompileM[(CompilationStepResultDec, List[(String, FINAL)])] = {
    for {
      _               <- checkDeclarationNameSize(p, func)
      funcNameWithErr <- validateShadowing(p, func, annListVars).handleError()
      argsWithErr <- func.args.toList
        .pure[CompileM]
        .ensure(BadFunctionSignatureSameArgNames(p.start, p.end, funcNameWithErr._1.getOrElse("NO_NAME"))) { l =>
          val names = l.map(_._1)
          names.toSet.size == names.size
        }
        .handleError()
      ctx <- get[Id, CompilerContext, CompilationError]
      argTypesWithErr <- func.args.toList
        .traverse { case (argName, argType) =>
          for {
            name        <- handlePart(argName)
            handledType <- handleCompositeType(p, argType, None, Some(name))
          } yield (name, VariableInfo(argName.position, handledType))
        }
        .handleError()
      compiledFuncBody <- local {
        val newArgs: VariableTypes = argTypesWithErr._1.getOrElse(List.empty).toMap
        modify[Id, CompilerContext, CompilationError](vars.modify(_)(_ ++ newArgs))
          .flatMap(_ => compileExprWithCtx(func.expr, saveExprContext, allowIllFormedStrings))
      }

      errorList     = funcNameWithErr._2 ++ argsWithErr._2 ++ argTypesWithErr._2
      parseNodeDecl = func.copy(expr = compiledFuncBody.parseNodeExpr)

      result =
        if (errorList.isEmpty) {
          CompilationStepResultDec(
            ctx,
            FUNC(funcNameWithErr._1.get, argTypesWithErr._1.get.map(_._1), compiledFuncBody.expr),
            compiledFuncBody.t,
            parseNodeDecl,
            compiledFuncBody.errors
          )
        } else {
          CompilationStepResultDec(ctx, FAILED_DEC(), compiledFuncBody.t, parseNodeDecl, errorList ++ compiledFuncBody.errors)
        }
    } yield (result, argTypesWithErr._1.map(_.map(nameAnfInfo => (nameAnfInfo._1, nameAnfInfo._2.vType))).getOrElse(List.empty))
  }

  def updateCtx(letName: String, letType: Types.FINAL, p: Pos): CompileM[Unit] =
    modify[Id, CompilerContext, CompilationError](vars.modify(_)(_ + (letName -> VariableInfo(p, letType))))

  def updateCtx(funcName: String, typeSig: FunctionTypeSignature, p: Pos): CompileM[Unit] =
    modify[Id, CompilerContext, CompilationError](functions.modify(_)(_ + (funcName -> FunctionInfo(p, List(typeSig)))))

  private def compileLetBlock(
      p: Pos,
      let: Expressions.LET,
      body: Expressions.EXPR,
      saveExprContext: Boolean,
      allowIllFormedStrings: Boolean
  ): CompileM[CompilationStepResultExpr] =
    for {
      compLetResult <- compileLet(p, let, saveExprContext, allowIllFormedStrings)
      letName = compLetResult.dec.name
      compiledBody <- local {
        updateCtx(letName, compLetResult.t, let.position)
          .flatMap(_ => compileExprWithCtx(body, saveExprContext, allowIllFormedStrings))
      }

      parseNodeExpr = Expressions.BLOCK(
        p,
        compLetResult.parseNodeExpr,
        compiledBody.parseNodeExpr,
        compiledBody.parseNodeExpr.resultType,
        saveExprContext.toOption(compiledBody.ctx)
      )
      result =
        if (!compLetResult.dec.isItFailed) {
          LET_BLOCK(compLetResult.dec.asInstanceOf[LET], compiledBody.expr)
        } else {
          FAILED_EXPR()
        }
    } yield CompilationStepResultExpr(compiledBody.ctx, result, compiledBody.t, parseNodeExpr, compLetResult.errors ++ compiledBody.errors)

  private def compileFuncBlock(
      p: Pos,
      func: Expressions.FUNC,
      body: Expressions.EXPR,
      saveExprContext: Boolean,
      allowIllFormedStrings: Boolean
  ): CompileM[CompilationStepResultExpr] = {
    for {
      compFuncRes <- compileFunc(p, func, saveExprContext, allowIllFormedStrings = allowIllFormedStrings)
      (compFuncStepRes, argTypes) = compFuncRes
      funcname                    = compFuncStepRes.dec.name
      typeSig                     = FunctionTypeSignature(compFuncStepRes.t, argTypes, FunctionHeader.User(funcname))
      compiledBody <- local {
        updateCtx(funcname, typeSig, func.position)
          .flatMap(_ => compileExprWithCtx(body, saveExprContext, allowIllFormedStrings))
      }

      expr = BLOCK(compFuncStepRes.dec, compiledBody.expr)
      parseNodeExpr = Expressions.BLOCK(
        p,
        compFuncStepRes.parseNodeExpr,
        compiledBody.parseNodeExpr,
        compiledBody.parseNodeExpr.resultType,
        ctxOpt = saveExprContext.toOption(compFuncStepRes.ctx)
      )
    } yield CompilationStepResultExpr(compiledBody.ctx, expr, compiledBody.t, parseNodeExpr, compFuncStepRes.errors ++ compiledBody.errors)
  }

  private def compileGetter(
      p: Pos,
      fieldPart: PART[String],
      refExpr: Expressions.EXPR,
      saveExprContext: Boolean,
      allowIllFormedStrings: Boolean,
      checkObjectType: Boolean
  ): CompileM[CompilationStepResultExpr] =
    for {
      ctx          <- get[Id, CompilerContext, CompilationError]
      fieldWithErr <- handlePart(fieldPart).handleError()
      compiledRef  <- compileExprWithCtx(refExpr, saveExprContext, allowIllFormedStrings)
      getterWithErr <- mkGetter(p, ctx, compiledRef.t, fieldWithErr._1.getOrElse("NO_NAME"), compiledRef.expr, checkObjectType).toCompileM
        .handleError()

      errorList     = fieldWithErr._2 ++ getterWithErr._2
      parseNodeExpr = Expressions.GETTER(p, compiledRef.parseNodeExpr, fieldPart, ctxOpt = saveExprContext.toOption(ctx))

      result =
        if (errorList.isEmpty) {
          val (ctx, expr, t) = getterWithErr._1.get
          CompilationStepResultExpr(ctx, expr, t, parseNodeExpr.copy(resultType = Some(t)), compiledRef.errors)
        } else {
          CompilationStepResultExpr(ctx, FAILED_EXPR(), NOTHING, parseNodeExpr, errorList ++ compiledRef.errors)
        }
    } yield result

  private def compileFunctionCall(
      p: Pos,
      namePart: PART[String],
      args: List[Expressions.EXPR],
      saveExprContext: Boolean,
      allowIllFormedStrings: Boolean
  ): CompileM[CompilationStepResultExpr] =
    for {
      ctx         <- get[Id, CompilerContext, CompilationError]
      nameWithErr <- handlePart(namePart).handleError()
      name = nameWithErr._1.getOrElse("NO_NAME")
      signatures   <- get[Id, CompilerContext, CompilationError].map(_.functionTypeSignaturesByName(name, args.size))
      compiledArgs <- args.traverse(arg => compileExprWithCtx(arg, saveExprContext, allowIllFormedStrings))
      funcCallWithErr <- (signatures match {
        case Nil           => FunctionNotFound(p.start, p.end, name, compiledArgs.map(_.t.toString)).asLeft[(EXPR, FINAL)]
        case single :: Nil => matchFuncOverload(p, name, args, compiledArgs, ctx.predefTypes, single)
        case many =>
          val matchedSigs = many
            .map(matchFuncOverload(p, name, args, compiledArgs, ctx.predefTypes, _))
            .collect({ case Right(ex) => ex })

          matchedSigs match {
            case Nil         => OverloadNotFound(p.start, p.end, name, compiledArgs.map(_.t.toString)).asLeft[(EXPR, FINAL)]
            case call :: Nil => call.asRight[CompilationError]
            case _           => AmbiguousOverloading(p.start, p.end, name, signatures).asLeft[(EXPR, FINAL)]
          }
      }).toCompileM.handleError()

      errorList    = nameWithErr._2 ++ funcCallWithErr._2
      argErrorList = compiledArgs.flatMap(_.errors)
      parseNodeExpr = Expressions.FUNCTION_CALL(
        p,
        namePart,
        compiledArgs.map(_.parseNodeExpr),
        funcCallWithErr._1.map(_._2),
        saveExprContext.toOption(ctx)
      )

      result =
        if (errorList.isEmpty) {
          val (expr, t) = funcCallWithErr._1.get
          CompilationStepResultExpr(ctx, expr, t, parseNodeExpr, argErrorList)
        } else {
          CompilationStepResultExpr(ctx, FAILED_EXPR(), NOTHING, parseNodeExpr, errorList ++ argErrorList)
        }
    } yield result

  private def compileGenericFunctionCall(
      p: Pos,
      e: Expressions.EXPR,
      namePart: PART[String],
      rawType: Expressions.Type,
      saveExprContext: Boolean,
      allowIllFormedStrings: Boolean
  ): CompileM[CompilationStepResultExpr] =
    for {
      ctx         <- get[Id, CompilerContext, CompilationError]
      name        <- handlePart(namePart)
      handledType <- handleCompositeType(p, rawType, None, None)
      handledExpr <- compileExprWithCtx(e, saveExprContext, allowIllFormedStrings)
    } yield TypeCast(p, name, handledExpr, handledType, ctx.provideRuntimeTypeOnCastError)

  private def compileRef(p: Pos, keyPart: PART[String], saveExprContext: Boolean): CompileM[CompilationStepResultExpr] =
    for {
      keyWithErr <- handlePart(keyPart).handleError()
      ctx        <- get[Id, CompilerContext, CompilationError]
      typeWithErr = ctx
        .resolveVar(keyWithErr._1.getOrElse(""))
        .fold[(Option[FINAL], Iterable[CompilationError])]((None, List(DefNotFound(p.start, p.end, keyWithErr._1.getOrElse("")))))(info =>
          (Some(info.vType), List.empty)
        )

      errorList = keyWithErr._2 ++ typeWithErr._2

      result =
        if (errorList.isEmpty) {
          CompilationStepResultExpr(
            ctx,
            REF(keyWithErr._1.get),
            typeWithErr._1.get,
            Expressions.REF(p, keyPart, typeWithErr._1, saveExprContext.toOption(ctx))
          )
        } else {
          CompilationStepResultExpr(
            ctx,
            FAILED_EXPR(),
            NOTHING,
            Expressions.REF(p, keyPart, ctxOpt = saveExprContext.toOption(ctx)),
            errorList
          )
        }
    } yield result

  private def compileFold(fold: Expressions.FOLD): CompileM[CompilationStepResultExpr] =
    for {
      (compiledList, listType, _, compileListErrors) <- compileExpr(fold.list)
      name = s"FOLD<${fold.limit}>"
      listInnerType <- (listType match {
        case list: LIST => Right(list.innerType)
        case other      => Left(Generic(fold.position.start, fold.position.end, s"First $name argument should be List[A], but $other found"))
      }).toCompileM
      (compiledAcc, accType, _, compileAccErrors) <- compileExpr(fold.acc)
      funcName                                    <- handlePart(fold.func.key)
      ctx                                         <- get[Id, CompilerContext, CompilationError]
      compiledFunc <- ctx
        .functionTypeSignaturesByName(funcName, args = 2)
        .collectFirst {
          case s @ FunctionTypeSignature(_, Seq((_, type1: FINAL), (_, type2: FINAL)), _) if type1 >= accType && type2 >= listInnerType =>
            Right(s)
        }
        .getOrElse {
          val accTypeStr       = if (accType == NOTHING) ANY else accType
          val listInnerTypeStr = if (listInnerType == NOTHING) ANY else listInnerType
          Left(
            Generic(fold.position.start, fold.position.end, s"Can't find suitable function $funcName(a: $accTypeStr, b: $listInnerTypeStr) for $name")
          )
        }
        .toCompileM
      _ <- set[Id, CompilerContext, CompilationError](ctx.copy(foldIdx = ctx.foldIdx + 1))
      resultType = compiledFunc.args.head._2.asInstanceOf[FINAL]
      compiledFold <- {
        val unwrapped = CompilerMacro.unwrapFold(ctx.foldIdx, fold.limit, compiledList, compiledAcc, compiledFunc.header)
        CompilationStepResultExpr(ctx, unwrapped, resultType, fold.copy(resultType = Some(resultType)), compileListErrors ++ compileAccErrors)
          .asRight[CompilationError]
          .toCompileM
      }
    } yield compiledFold

  private def matchFuncOverload(
      p: Pos,
      funcName: String,
      funcArgs: List[Expressions.EXPR],
      resolvedArgs: List[CompilationStepResultExpr],
      predefTypes: Map[String, FINAL],
      f: FunctionTypeSignature
  ): Either[CompilationError, (EXPR, FINAL)] = {
    val argTypes = f.args
    if (funcArgs.lengthCompare(argTypes.size) != 0)
      Left(WrongArgumentsNumber(p.start, p.end, funcName, argTypes.size, funcArgs.size))
    else {
      val typedExpressionArgumentsAndTypedPlaceholders = resolvedArgs.zip(argTypes)

      val typePairs = typedExpressionArgumentsAndTypedPlaceholders.map { case (typedExpr, tph) => (typedExpr.t, tph._2) }
      for {
        resolvedTypeParams <- TypeInferrer(typePairs, predefTypes).leftMap(Generic(p.start, p.end, _))
        args = typedExpressionArgumentsAndTypedPlaceholders.map(_._1.expr)
        resultType <- Try(toFinal(f.result, resolvedTypeParams)).toEither
          .leftMap(e => CompilationError.Generic(0, 0, s"Base error: ${e.getMessage}"))
      } yield {
        (FUNCTION_CALL(f.header, args): EXPR, resultType)
      }
    }
  }

  def mkIf(p: Pos, cond: EXPR, ifTrue: (EXPR, FINAL), ifFalse: (EXPR, FINAL)): Either[CompilationError, (EXPR, FINAL)] = {
    val t = TypeInferrer.findCommonType(ifTrue._2, ifFalse._2)
    (IF(cond, ifTrue._1, ifFalse._1), t).asRight
  }

  def mkIfCases(
      cases: List[MATCH_CASE],
      caseTypes: List[FINAL],
      refTmp: Expressions.REF,
      defaultType: FINAL,
      allowShadowVarName: Option[String],
      ctx: CompilerContext
  ): Either[CompilationError, Expressions.EXPR] = {

    def resolveFieldType(p: Pos, field: String, t: Single): Either[CompilationError, Option[FINAL]] =
      handleCompositeType(p, t, None, allowShadowVarName)
        .run(ctx)
        .value
        ._2
        .map(_.asInstanceOf[CASETYPEREF].fields.find(_._1 == field).map(_._2))

    def f(mc: MATCH_CASE, caseType: FINAL, further: Expressions.EXPR): Either[CompilationError, Expressions.EXPR] = {
      val blockWithNewVarE = mc.pattern match {
        case TypedVar(None, _) | ConstsPat(_, _) => mc.expr.asRight[CompilationError]
        case TypedVar(Some(nv), _) =>
          val allowShadowing = nv match {
            case PART.VALID(_, x) => allowShadowVarName.contains(x)
            case _                => false
          }
          val t = caseType match {
            case UNION(Seq(), _) =>
              defaultType match {
                case UNION(Seq(t), _) => t
                case _                => defaultType
              }
            case _ => caseType
          }
          Expressions.BLOCK(mc.position, Expressions.LET(mc.position, nv, refTmp, Some(t), allowShadowing), mc.expr).asRight[CompilationError]
        case p: CompositePattern =>
          val newRef = p.caseType.fold(refTmp)(t => refTmp.copy(resultType = Some(caseType)))
          val exprE = p.subpatterns.foldRight(mc.expr.asRight[CompilationError]) { (pa, nextExprE) =>
            (nextExprE, pa) match {
              case (Right(nextExpr), (TypedVar(Some(nv), t), path)) =>
                val (field, objType) = path.head
                for {
                  resolvedField     <- handlePart(field).run(ctx).value._2
                  resolvedFieldType <- objType.flatTraverse(resolveFieldType(nv.position, resolvedField, _))
                  typeFromContext   <- handleCompositeType(nv.position, t, None, allowShadowVarName).run(ctx).value._2
                } yield {
                  val resolvedType = resolvedFieldType.fold(typeFromContext)(t => UNION.reduce(UNION(t, typeFromContext)))
                  val accs         = mkGet(path, newRef, nv.position)
                  val allowShadowing = nv match {
                    case PART.VALID(_, x) => allowShadowVarName.contains(x)
                    case _                => false
                  }
                  Expressions.BLOCK(mc.position, Expressions.LET(nv.position, nv, accs, Some(resolvedType), allowShadowing), nextExpr)
                }
              case _ => nextExprE
            }
          }
          exprE.map(expr =>
            p.caseType.fold(expr) { _ =>
              val let = Expressions.LET(p.position, newRef.key, newRef, Some(caseType), allowShadowing = true)
              Expressions.BLOCK(p.position, let, expr)
            }
          )
      }

      def isInst(matchType: String): Expressions.EXPR =
        Expressions
          .FUNCTION_CALL(
            mc.position,
            PART.VALID(mc.position, IsInstanceOf),
            List(refTmp, Expressions.CONST_STRING(mc.position, PART.VALID(mc.position, matchType)))
          )

      blockWithNewVarE.flatMap { blockWithNewVar =>
        (mc.pattern, caseType.unfold) match {
          case (_: TypedVar, ANY)           => Right(blockWithNewVar)
          case (_: TypedVar, UNION(Nil, _)) => Right(blockWithNewVar)
          case (_: TypedVar, UNION(types, _)) =>
            for {
              cases <- types.map(_.name) match {
                case hType :: tTypes =>
                  val typeIf =
                    tTypes.foldLeft(isInst(hType))((other, matchType) => BINARY_OP(mc.position, isInst(matchType), BinaryOperation.OR_OP, other))
                  Right(makeIfCase(typeIf, blockWithNewVar, further))
                case Nil => ???
              }
            } yield cases
          case (_: TypedVar, t) =>
            Right(makeIfCase(isInst(t.name), blockWithNewVar, further))

          case (ConstsPat(consts, _), _) =>
            val cond = consts
              .map(c => BINARY_OP(mc.position, c, BinaryOperation.EQ_OP, refTmp))
              .reduceRight((c, r) => BINARY_OP(mc.position, c, BinaryOperation.OR_OP, r))
            Right(makeIfCase(cond, blockWithNewVar, further))

          case (p: CompositePattern, _) =>
            val pos        = p.position
            val newRef     = p.caseType.fold(refTmp)(t => refTmp.copy(resultType = Some(caseType)))
            val conditions = makeConditionsFromCompositePattern(p, newRef)
            val cond = if (conditions.isEmpty) {
              Expressions.TRUE(pos): Expressions.EXPR
            } else {
              conditions.reduceRight { (c, r) =>
                BINARY_OP(pos, c, BinaryOperation.AND_OP, r): Expressions.EXPR
              }
            }
            val checkingCond =
              if (p.isInstanceOf[TuplePat]) {
                val (resolvedTypes, size) = resolveTypesFromCompositePattern(p)
                if (p.patternsWithFields.size == size) {
                  val typeChecks =
                    resolvedTypes
                      .map(t => Expressions.FUNCTION_CALL(pos, PART.VALID(pos, IsInstanceOf), List(refTmp, Expressions.CONST_STRING(pos, t))))
                      .reduceLeft[Expressions.EXPR] { case (c, r) => BINARY_OP(pos, c, BinaryOperation.OR_OP, r) }
                  BINARY_OP(pos, cond, BinaryOperation.AND_OP, typeChecks)
                } else {
                  val size        = Expressions.CONST_LONG(pos, p.patternsWithFields.size)
                  val getSize     = Expressions.FUNCTION_CALL(pos, PART.VALID(pos, "size"), List(refTmp))
                  val compareSize = BINARY_OP(pos, getSize, BinaryOperation.EQ_OP, size)
                  BINARY_OP(pos, cond, BinaryOperation.AND_OP, compareSize)
                }
              } else
                cond
            Right(
              makeIfCase(
                p.caseType.fold(checkingCond)(t =>
                  BINARY_OP(
                    pos,
                    Expressions.FUNCTION_CALL(pos, PART.VALID(pos, IsInstanceOf), List(refTmp, Expressions.CONST_STRING(pos, t.name))),
                    BinaryOperation.AND_OP,
                    Expressions.BLOCK(pos, Expressions.LET(pos, newRef.key, newRef, Some(caseType), true), checkingCond)
                  )
                ),
                blockWithNewVar,
                further
              )
            )
        }
      }
    }

    val default: Either[CompilationError, Expressions.EXPR] = Right {
      val pos = cases.head.position
      Expressions.FUNCTION_CALL(
        pos,
        PART.VALID(pos, "throw"),
        List(Expressions.CONST_STRING(pos, PART.VALID(pos, "Match error")))
      )
    }

    (cases zip caseTypes).foldRight(default) { case ((mc, caseType), furtherEi) =>
      furtherEi match {
        case Right(further) => f(mc, caseType, further)
        case Left(e)        => Left(e)
      }
    }
  }

  private def makeIfCase(cond: Expressions.EXPR, ifTrue: Expressions.EXPR, ifFalse: Expressions.EXPR): Expressions.IF =
    Expressions.IF(Pos(cond.position), cond, ifTrue, ifFalse)

  private def mkGet(path: Seq[(PART[String], Option[Single])], ref: Expressions.EXPR, pos: Pos): Expressions.EXPR =
    path.map(_._1).foldRight(ref) { (field, exp) =>
      Expressions.GETTER(pos, exp, field, checkObjectType = false)
    }

  private def makeConditionsFromCompositePattern(p: CompositePattern, newRef: Expressions.REF): Seq[Expressions.EXPR] =
    p.subpatterns collect {
      case (pat @ TypedVar(_, Expressions.Union(types)), path) if types.nonEmpty =>
        val pos = pat.position
        val v   = mkGet(path, newRef, pos)
        types
          .map {
            case Expressions.Single(t, None) =>
              Expressions.FUNCTION_CALL(pos, PART.VALID(pos, IsInstanceOf), List(v, Expressions.CONST_STRING(pos, t))): Expressions.EXPR
            case Expressions.Single(PART.VALID(pos, Type.ListTypeName), Some(PART.VALID(_, Expressions.AnyType(_)))) =>
              val t = PART.VALID(pos, "List[Any]")
              Expressions.FUNCTION_CALL(
                pos,
                PART.VALID(pos, IsInstanceOf),
                List(v, Expressions.CONST_STRING(pos, t))
              ): Expressions.EXPR
            case _ => ???
          }
          .reduceRight[Expressions.EXPR] { (c, r) =>
            BINARY_OP(pos, c, BinaryOperation.OR_OP, r)
          }
      case (pat @ TypedVar(_, Expressions.Single(PART.VALID(_, Type.ListTypeName), Some(PART.VALID(_, Expressions.AnyType(_))))), path) =>
        val pos = pat.position
        val v   = mkGet(path, newRef, pos)
        val t   = PART.VALID(pos, "List[Any]")
        Expressions.FUNCTION_CALL(pos, PART.VALID(pos, IsInstanceOf), List(v, Expressions.CONST_STRING(pos, t))): Expressions.EXPR
      case (pat @ TypedVar(_, Expressions.Single(t, None)), path) =>
        val pos = pat.position
        val v   = mkGet(path, newRef, pos)
        Expressions.FUNCTION_CALL(pos, PART.VALID(pos, IsInstanceOf), List(v, Expressions.CONST_STRING(pos, t))): Expressions.EXPR
      case (TypedVar(_, Expressions.Single(_, _)), _) => ???
      case (pat @ ConstsPat(consts, _), path) =>
        val pos = pat.position
        val v   = mkGet(path, newRef, pos)
        consts
          .map { c =>
            BINARY_OP(pos, c, BinaryOperation.EQ_OP, v)
          }
          .reduceRight[BINARY_OP] { (c, r) =>
            BINARY_OP(pos, c, BinaryOperation.OR_OP, r)
          }
    }

  private def resolveTypesFromCompositePattern(p: CompositePattern): (Seq[PART[String]], Int) = {
    val rawTypes = p.patternsWithFields.collect {
      case (_, TypedVar(_, Expressions.Union(types))) if types.nonEmpty =>
        types
          .map {
            case Expressions.Single(t, None) =>
              List(t)
            case Expressions.Single(PART.VALID(pos, Type.ListTypeName), Some(PART.VALID(_, Expressions.AnyType(_)))) =>
              val t = PART.VALID(pos, "List[Any]")
              List(t)
            case _ => ???
          }
          .reduceRight[Seq[PART[String]]] { (ct, rt) =>
            rt ++ ct
          }
      case (_, pat @ TypedVar(_, Expressions.Single(PART.VALID(_, Type.ListTypeName), Some(PART.VALID(_, Expressions.AnyType(_)))))) =>
        val pos = pat.position
        val t   = PART.VALID(pos, "List[Any]")
        List(t)
      case (_, TypedVar(_, Expressions.Single(t, None))) =>
        List(t)
      case (_, TypedVar(_, Expressions.Single(_, _))) => ???
      case (_, pat @ ConstsPat(consts, _)) =>
        val pos = pat.position
        consts
          .map { c =>
            val t = c match {
              case e: Expressions.INVALID => PART.INVALID(pos, e.message)
              case a                      => a.resultType.map(t => PART.VALID(pos, t.name)).getOrElse(PART.VALID(pos, "Any"))
            }
            List(t)
          }
          .reduceRight[Seq[PART[String]]] { (ct, rt) =>
            rt ++ ct
          }
      case (_, c: TuplePat) =>
        resolveTypesFromCompositePattern(c)._1
      case (_, c: ObjPat) =>
        Seq(c.objType.name)
    }
    val resolvedType =
      regroup(rawTypes).map {
        _.toList
          .traverse {
            case PART.VALID(_, v) => Right(v)
            case invalid          => Left(invalid)
          }
          .fold(identity, types => PART.VALID(p.position, if (types.size == 1) types.head else types.mkString("(", ", ", ")")))
      }.distinct
    (resolvedType, rawTypes.size)
  }

  private def mkGetter(
      p: Pos,
      ctx: CompilerContext,
      objectType: FINAL,
      fieldName: String,
      expr: EXPR,
      checkObjectType: Boolean
  ): Either[CompilationError, (CompilerContext, GETTER, FINAL)] = {
    lazy val err =
      FieldNotFound(p.start, p.end, fieldName, objectType.name)
        .asLeft[(CompilerContext, GETTER, FINAL)]

    val getter = GETTER(expr, fieldName)
    objectType.typeList
      .traverse(_.fields.find(_._1 == fieldName).map(_._2))
      .map(TypeInferrer.findCommonType)
      .fold(err)(t => if (t == NOTHING && checkObjectType) err else Right((ctx, getter, t)))
  }

  private def handleCompositeType(
      pos: Pos,
      t: Expressions.Type,
      expectedType: Option[FINAL],
      varName: Option[String]
  ): CompileM[FINAL] =
    t match {
      case Expressions.Single(name, parameter) =>
        for {
          ctx              <- get[Id, CompilerContext, CompilationError]
          handledName      <- handlePart(name)
          handledParameter <- parameter.traverse(handlePart)
          expectedTypes = expectedType.fold(ctx.predefTypes.keys.toList)(_.typeList.map(_.name))
          parameter <- handledParameter.traverse(handleCompositeType(pos, _, expectedType, varName))
          t <- liftEither[Id, CompilerContext, CompilationError, FINAL](
            parameter.fold(flatSingle(pos, ctx.predefTypes, expectedTypes, varName, handledName).map(v => UNION.reduce(UNION.create(v, None)))) { p =>
              for {
                typeConstr <- findGenericType(pos, handledName)
              } yield typeConstr(p)
            }
          )
        } yield t
      case Expressions.Union(types) =>
        types.toList
          .traverse(handleCompositeType(pos, _, expectedType, varName))
          .map { types =>
            val union = UNION.create(types)
            if (union.typeList.isEmpty) union else UNION.reduce(union)
          }
      case Expressions.Tuple(types) =>
        types.toList
          .traverse(handleCompositeType(pos, _, expectedType, varName))
          .map(types => TUPLE(types))
      case Expressions.AnyType(pos) => (ANY: FINAL).pure[CompileM]
    }

  def handlePart[T](part: PART[T]): CompileM[T] = part match {
    case PART.VALID(_, x)         => x.pure[CompileM]
    case PART.INVALID(p, message) => raiseError(Generic(p.start, p.end, message))
  }

  implicit class RichBoolean(val b: Boolean) extends AnyVal {
    final def toOption[A](a: => A): Option[A] = if (b) Some(a) else None
  }

  def apply(c: CompilerContext, expr: Expressions.EXPR, allowIllFormedStrings: Boolean = false): Either[String, (EXPR, FINAL)] =
    applyWithCtx(c, expr, allowIllFormedStrings).map(r => (r._2, r._3))

  def applyWithCtx(
      c: CompilerContext,
      expr: Expressions.EXPR,
      allowIllFormedStrings: Boolean = false
  ): Either[String, (CompilerContext, EXPR, FINAL)] =
    compileExprWithCtx(expr, allowIllFormedStrings = allowIllFormedStrings)
      .run(c)
      .value
      ._2
      .leftMap(e => s"Compilation failed: ${Show[CompilationError].show(e)}")
      .flatMap(res =>
        Either.cond(
          res.errors.isEmpty,
          (res.ctx, res.expr, res.t),
          s"Compilation failed: [${res.errors.map(e => Show[CompilationError].show(e)).mkString("; ")}]"
        )
      )
}
