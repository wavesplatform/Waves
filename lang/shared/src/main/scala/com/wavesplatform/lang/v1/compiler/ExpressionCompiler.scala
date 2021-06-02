package com.wavesplatform.lang.v1.compiler

import cats.implicits._
import cats.{Id, Show}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.compiler.CompilationError._
import com.wavesplatform.lang.v1.compiler.CompilerContext._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import com.wavesplatform.lang.v1.parser.Expressions.{BINARY_OP, CompositePattern, ConstsPat, MATCH_CASE, ObjPat, PART, Pos, TypedVar}
import com.wavesplatform.lang.v1.parser.{BinaryOperation, Expressions, Parser}
import com.wavesplatform.lang.v1.task.imports._
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

  def compile(input: String, ctx: CompilerContext, allowIllFormedStrings: Boolean = false): Either[String, (EXPR, FINAL)] = {
    Parser.parseExpr(input) match {
      case fastparse.Parsed.Success(xs, _)       => ExpressionCompiler(ctx, xs, allowIllFormedStrings)
      case f @ fastparse.Parsed.Failure(_, _, _) => Left(f.toString)
    }
  }

  def compileBoolean(input: String, ctx: CompilerContext): Either[String, EXPR] = {
    compile(input, ctx).flatMap {
      case (expr, BOOLEAN) => Right(expr)
      case (_, _)          => Left("Script should return boolean")
    }
  }

  def compileUntyped(input: String, ctx: CompilerContext): Either[String, EXPR] = {
    compile(input, ctx)
      .map { case (expr, _) => expr }
  }

  def compileWithParseResult(
      input: String,
      ctx: CompilerContext,
      saveExprContext: Boolean = true
  ): Either[String, (EXPR, Expressions.SCRIPT, Iterable[CompilationError])] = {
    val res = Parser.parseExpressionWithErrorRecovery(input)
    res match {
      case Right((parseResult, removedCharPosOpt)) =>
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
          .leftMap(e => s"Compilation failed: ${Show[CompilationError].show(e)}")

      case Left(error) => Left(error.toString)
    }
  }

  def compileDecls(input: String, ctx: CompilerContext): Either[String, EXPR] = {
    val adjustedDecls = s"$input\n${PureContext.unitVarName}"
    compileUntyped(adjustedDecls, ctx)
  }

  private def compileExpr(expr: Expressions.EXPR): CompileM[(Terms.EXPR, FINAL, Expressions.EXPR)] =
    compileExprWithCtx(expr, allowIllFormedStrings = false).map(r => (r.expr, r.t, r.parseNodeExpr))

  private def compileExprWithCtx(expr: Expressions.EXPR, saveExprContext: Boolean = false, allowIllFormedStrings: Boolean): CompileM[CompilationStepResultExpr] = {
    get[Id, CompilerContext, CompilationError].flatMap { ctx =>
      def adjustByteStr(expr: Expressions.CONST_BYTESTR, b: ByteStr) =
        CONST_BYTESTR(b)
          .leftMap(CompilationError.Generic(expr.position.start, expr.position.end, _))
          .map(CompilationStepResultExpr(ctx, _, BYTESTR, expr))
          .recover { case err => CompilationStepResultExpr(ctx, FAILED_EXPR(), NOTHING, expr, List(err)) }

      def adjustStr(expr: Expressions.CONST_STRING, str: String): Either[CompilationError, CompilationStepResultExpr] =
        CONST_STRING(str)
          .filterOrElse(_ => allowIllFormedStrings || !global.isIllFormed(str), s"String '$str' contains ill-formed characters")
          .leftMap(CompilationError.Generic(expr.position.start, expr.position.end, _))
          .map(CompilationStepResultExpr(ctx, _, STRING, expr))
          .recover { case err => CompilationStepResultExpr(ctx, FAILED_EXPR(), NOTHING, expr, List(err)) }

      expr match {
        case x: Expressions.CONST_LONG    => CompilationStepResultExpr(ctx, CONST_LONG(x.value): EXPR, LONG: FINAL, x: Expressions.EXPR).pure[CompileM]
        case x: Expressions.CONST_BYTESTR => handlePart(x.value).flatMap(b => liftEither(adjustByteStr(x, b)))
        case x: Expressions.CONST_STRING  => handlePart(x.value).flatMap(s => liftEither(adjustStr(x, s)))
        case x: Expressions.TRUE          => CompilationStepResultExpr(ctx, TRUE: EXPR, BOOLEAN: FINAL, x: Expressions.EXPR).pure[CompileM]
        case x: Expressions.FALSE         => CompilationStepResultExpr(ctx, FALSE: EXPR, BOOLEAN: FINAL, x: Expressions.EXPR).pure[CompileM]

        case x: Expressions.INVALID =>
          CompilationStepResultExpr(
            ctx,
            FAILED_EXPR(): EXPR,
            NOTHING,
            x: Expressions.EXPR,
            List(Generic(x.position.start, x.position.end, x.message))
          ).pure[CompileM]

        case Expressions.GETTER(p, ref, field, _, _)        => compileGetter(p, field, ref, saveExprContext, allowIllFormedStrings)
        case Expressions.BLOCK(p, dec, body, _, _)          => compileBlock(p, dec, body, saveExprContext, allowIllFormedStrings)
        case Expressions.IF(p, cond, ifTrue, ifFalse, _, _) => compileIf(p, cond, ifTrue, ifFalse, saveExprContext, allowIllFormedStrings)
        case Expressions.REF(p, key, _, _)                  => compileRef(p, key, saveExprContext)
        case Expressions.FUNCTION_CALL(p, name, args, _, _) => compileFunctionCall(p, name, args, saveExprContext, allowIllFormedStrings)
        case Expressions.MATCH(p, ex, cases, _, _)          => compileMatch(p, ex, cases.toList, saveExprContext, allowIllFormedStrings)
        case Expressions.FOLD(p, limit, list, acc, f, _, _) => compileFold(p, limit, list, acc, f.key)
        case Expressions.BINARY_OP(p, a, op, b, _, _) =>
          op match {
            case AND_OP => compileIf(p, a, b, Expressions.FALSE(p), saveExprContext, allowIllFormedStrings)
            case OR_OP  => compileIf(p, a, Expressions.TRUE(p), b, saveExprContext, allowIllFormedStrings)
            case _      => compileFunctionCall(p, PART.VALID(p, BinaryOperation.opsToFunctions(op)), List(a, b), saveExprContext, allowIllFormedStrings)
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
        ctxOpt = saveExprContext.toOption(ctx.getSimpleContext())
      )
      errorList = condWithErr._1.errors ++ ifTrue.errors ++ ifFalse.errors

      result = if (condWithErr._2.isEmpty) {
        CompilationStepResultExpr(
          ctx,
          IF(condWithErr._1.expr, ifTrue.expr, ifFalse.expr),
          t,
          parseNodeExpr.copy(resultType = Some(t)),
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
      case "List" => Right(LIST)
      case _      => Left(GenericTypeNotFound(p.start, p.end, t))
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
          case _ => false
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
        case MATCH_CASE(_, TypedVar(_, t), _, _, _) => handleCompositeType(p, t, Some(exprTypes), allowShadowVarName)
        case MATCH_CASE(_, ObjPat(_, t, _), _, _, _) => handleCompositeType(p, t, Some(exprTypes), allowShadowVarName)
        case _ => (NOTHING: FINAL).pure[CompileM]
      }
      defaultType = exprTypes match {
        case ANY => ANY
        case UNION(tl, _) => UNION(tl.filter(t => !matchTypes.contains(t)), None)
        case _ => NOTHING
      }
      ifCasesWithErr <- inspectFlat[Id, CompilerContext, CompilationError, Expressions.EXPR](
        updatedCtx => {
          val ref = Expressions.REF(p, PART.VALID(p, refTmpKey), ctxOpt = saveExprContext.toOption(updatedCtx.getSimpleContext()))
          mkIfCases(cases, matchTypes, ref, defaultType, allowShadowVarName).toCompileM
        }
      ).handleError()
      compiledMatch <- compileLetBlock(
        p,
        Expressions.LET(p, PART.VALID(p, refTmpKey), expr),
        ifCasesWithErr._1.getOrElse(
          Expressions.INVALID(
            p,
            ifCasesWithErr._2.map(e => Show[CompilationError].show(e)).mkString_("\n"),
            ctxOpt = saveExprContext.toOption(ctx.getSimpleContext())
          )
        ),
        saveExprContext,
        allowIllFormedStrings
      )
      checktypes = if(matchTypes.contains(LIST(ANY))) {
        (matchTypes.filter(_ != LIST(ANY)), UNION.create((exprTypes match {
          case ANY => List(ANY)
          case t => t.typeList
        }).filter {
          case LIST(_) => false
          case _ => true
        }))
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

      result = if (errorList.isEmpty) {
        compiledMatch.copy(errors = compiledMatch.errors ++ typedExpr.errors)
      } else {
        CompilationStepResultExpr(
          ctx,
          FAILED_EXPR(),
          NOTHING,
          Expressions.MATCH(p, typedExpr.parseNodeExpr, cases, ctxOpt = saveExprContext.toOption(ctx.getSimpleContext())),
          errorList ++ typedExpr.errors
        )
      }
    } yield result

  private def exprContainsRef(expr: Expressions.EXPR, ref: String): Boolean =
    expr match {
      case Expressions.GETTER(_, expr, _, _, _) =>
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
        .ensureOr(n => AlreadyDefined(p.start, p.end, n, isFunction = false))(
          n => !ctx.varDefs.contains(n) || dec.allowShadowing || allowedExceptions.contains(n)
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

      result = if (errorList.isEmpty) {
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
        .traverse {
          case (argName, argType) =>
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

      result = if (errorList.isEmpty) {
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
        ctxOpt = saveExprContext.toOption(compiledBody.ctx.getSimpleContext())
      )
      result = if (!compLetResult.dec.isItFailed) {
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
        ctxOpt = saveExprContext.toOption(compFuncStepRes.ctx.getSimpleContext())
      )
    } yield CompilationStepResultExpr(compiledBody.ctx, expr, compiledBody.t, parseNodeExpr, compFuncStepRes.errors ++ compiledBody.errors)
  }

  private def compileGetter(
      p: Pos,
      fieldPart: PART[String],
      refExpr: Expressions.EXPR,
      saveExprContext: Boolean,
      allowIllFormedStrings: Boolean
  ): CompileM[CompilationStepResultExpr] =
    for {
      ctx           <- get[Id, CompilerContext, CompilationError]
      fieldWithErr  <- handlePart(fieldPart).handleError()
      compiledRef   <- compileExprWithCtx(refExpr, saveExprContext, allowIllFormedStrings)
      getterWithErr <- mkGetter(p, ctx, compiledRef.t.typeList, fieldWithErr._1.getOrElse("NO_NAME"), compiledRef.expr).toCompileM.handleError()

      errorList     = fieldWithErr._2 ++ getterWithErr._2
      parseNodeExpr = Expressions.GETTER(p, compiledRef.parseNodeExpr, fieldPart, ctxOpt = saveExprContext.toOption(ctx.getSimpleContext()))

      result = if (errorList.isEmpty) {
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
      signatures   <- get[Id, CompilerContext, CompilationError].map(_.functionTypeSignaturesByName(name))
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
        ctxOpt = saveExprContext.toOption(ctx.getSimpleContext())
      )

      result = if (errorList.isEmpty) {
        val (expr, t) = funcCallWithErr._1.get
        CompilationStepResultExpr(ctx, expr, t, parseNodeExpr, argErrorList)
      } else {
        CompilationStepResultExpr(ctx, FAILED_EXPR(), NOTHING, parseNodeExpr, errorList ++ argErrorList)
      }
    } yield result

  private def compileRef(p: Pos, keyPart: PART[String], saveExprContext: Boolean): CompileM[CompilationStepResultExpr] =
    for {
      keyWithErr <- handlePart(keyPart).handleError()
      ctx        <- get[Id, CompilerContext, CompilationError]
      typeWithErr = ctx.resolveVar(keyWithErr._1.getOrElse(""))
        .fold[(Option[FINAL], Iterable[CompilationError])]((None, List(DefNotFound(p.start, p.end, keyWithErr._1.getOrElse("")))))(
          info => (Some(info.vType), List.empty)
        )

      errorList = keyWithErr._2 ++ typeWithErr._2

      result = if (errorList.isEmpty) {
        CompilationStepResultExpr(
          ctx,
          REF(keyWithErr._1.get),
          typeWithErr._1.get,
          Expressions.REF(p, keyPart, None, ctxOpt = saveExprContext.toOption(ctx.getSimpleContext()))
        )
      } else {
        CompilationStepResultExpr(
          ctx,
          FAILED_EXPR(),
          NOTHING,
          Expressions.REF(p, keyPart, ctxOpt = saveExprContext.toOption(ctx.getSimpleContext())),
          errorList
        )
      }
    } yield result

  private def compileFold(
      p: Pos,
      limit: Int,
      list: Expressions.EXPR,
      acc: Expressions.EXPR,
      func: PART[String]
  ): CompileM[CompilationStepResultExpr] =
    for {
      (list, listType, _) <- compileExpr(list)
      listInnerType <- (listType match {
        case list: LIST => Right(list.innerType)
        case other      => Left(Generic(p.start, p.end, s"FOLD first argument should be List[T], but $other found"))
      }).toCompileM
      (acc, accType, accRaw) <- compileExpr(acc)
      funcName               <- handlePart(func)
      ctx                    <- get[Id, CompilerContext, CompilationError]
      function <- ctx
        .functionTypeSignaturesByName(funcName)
        .collectFirst {
          case s @ FunctionTypeSignature(_, Seq((_, type1: FINAL), (_, type2: FINAL)), _) if type1 >= accType && type2 >= listInnerType =>
            Right(s)
        }
        .getOrElse {
          val accTypeStr       = if (accType == NOTHING) ANY else accType
          val listInnerTypeStr = if (listInnerType == NOTHING) ANY else listInnerType
          Left(Generic(p.start, p.end, s"Can't find suitable function $funcName(a: $accTypeStr, b: $listInnerTypeStr) for FOLD"))
        }
        .toCompileM
      r <- Right(CompilerMacro.unwrapFold(limit, list, acc, function.header)).toCompileM
    } yield CompilationStepResultExpr(ctx, r, function.args.head._2.asInstanceOf[FINAL], accRaw)

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
      allowShadowVarName: Option[String]
  ): Either[CompilationError, Expressions.EXPR] = {

    def mkGet(path: Seq[PART[String]], ref: Expressions.EXPR, pos: Pos): Expressions.EXPR = {
      path.foldRight(ref) {
        (field, exp) => Expressions.GETTER(pos, exp, field)
      }
    }

    def f(mc: MATCH_CASE, caseType: FINAL, further: Expressions.EXPR): Either[CompilationError, Expressions.EXPR] = {
      val blockWithNewVar = mc.pattern match {
        case TypedVar(None, _) | ConstsPat(_, _) => mc.expr
        case TypedVar(Some(nv), _) =>
          val allowShadowing = nv match {
            case PART.VALID(_, x) => allowShadowVarName.contains(x)
            case _                => false
          }
          val t = caseType match {
            case UNION(Seq(),_) => defaultType match {
                                     case UNION(Seq(t), _) => t
                                     case _ => defaultType
                                   }
            case _ => caseType
          }
          Expressions.BLOCK(mc.position, Expressions.LET(mc.position, nv, refTmp, Some(t), allowShadowing), mc.expr)
        case (p: CompositePattern) =>
          val newRef = p.caseType.fold(refTmp)(t => refTmp.copy(resultType = Some(caseType)))
          val expr = p.subpatterns.foldRight(mc.expr) { (pa, exp) =>
            pa match {
              case (TypedVar(Some(nv), t), path) =>
                val accs = mkGet(path, newRef, nv.position)
                val allowShadowing = nv match {
                  case PART.VALID(_, x) => allowShadowVarName.contains(x)
                  case _                => false
                }
                Expressions.BLOCK(mc.position, Expressions.LET(nv.position, nv, accs, None, allowShadowing), exp)
              case _ => exp
            }
          }
          p.caseType.fold(expr)(_ => Expressions.BLOCK(p.position, Expressions.LET(p.position, newRef.key, newRef, Some(caseType), true), expr))
      }

      def isInst(matchType: String): Expressions.EXPR =
        Expressions
          .FUNCTION_CALL(
            mc.position,
            PART.VALID(mc.position, "_isInstanceOf"),
            List(refTmp, Expressions.CONST_STRING(mc.position, PART.VALID(mc.position, matchType)))
          )

      (mc.pattern, caseType.unfold) match {
        case (_:TypedVar, ANY) => Right(blockWithNewVar)
        case (_:TypedVar, UNION(Nil, _)) => Right(blockWithNewVar)
        case (x:TypedVar, UNION(types, _)) =>
          for {
            cases <- types.map(_.name) match {
              case hType :: tTypes =>
                val typeIf =
                  tTypes.foldLeft(isInst(hType))((other, matchType) => BINARY_OP(mc.position, isInst(matchType), BinaryOperation.OR_OP, other))
                Right(Expressions.IF(mc.position, typeIf, blockWithNewVar, further))
              case Nil => ???
            }
          } yield cases
        case (_:TypedVar, t) =>
          Right(Expressions.IF(mc.position, isInst(t.name), blockWithNewVar, further))

        case (ConstsPat(consts, _), _) =>
          val cond = consts.map(c => BINARY_OP(mc.position, c, BinaryOperation.EQ_OP, refTmp)).reduceRight((c, r) => BINARY_OP(mc.position, c, BinaryOperation.OR_OP, r))
          Right(Expressions.IF(mc.position, cond, blockWithNewVar, further))

        case (p:CompositePattern, _) =>
          val pos = p.position
          val newRef = p.caseType.fold(refTmp)(t => refTmp.copy(resultType = Some(caseType)))
          val conds = p.subpatterns collect {
            case (pat @ TypedVar(_, Expressions.Union(types)), path) if types.nonEmpty =>
              val pos = pat.position
              val v = mkGet(path, newRef, pos)
              types map {
                case Expressions.Single(t, None) =>
                   Expressions.FUNCTION_CALL(pos, PART.VALID(pos, "_isInstanceOf"), List(v, Expressions.CONST_STRING(pos, t))): Expressions.EXPR
                case Expressions.Single(PART.VALID(pos, "List"), Some(PART.VALID(_, Expressions.AnyType(_)))) =>
                   Expressions.FUNCTION_CALL(pos, PART.VALID(pos, "_isInstanceOf"), List(v, Expressions.CONST_STRING(pos, PART.VALID(pos, "List[Any]")))): Expressions.EXPR
                case _ => ???
              } reduceRight { (c,r) =>
                BINARY_OP(pos, c, BinaryOperation.OR_OP, r): Expressions.EXPR
              }
            case (pat @ TypedVar(_, Expressions.Single(PART.VALID(pos, "List"), Some(PART.VALID(_, Expressions.AnyType(_))))), path) =>
              val pos = pat.position
              val v = mkGet(path, newRef, pos)
              Expressions.FUNCTION_CALL(pos, PART.VALID(pos, "_isInstanceOf"), List(v, Expressions.CONST_STRING(pos, PART.VALID(pos, "List[Any]")))): Expressions.EXPR
            case (pat @ TypedVar(_, Expressions.Single(t, None)), path) =>
              val pos = pat.position
              val v = mkGet(path, newRef, pos)
              Expressions.FUNCTION_CALL(pos, PART.VALID(pos, "_isInstanceOf"), List(v, Expressions.CONST_STRING(pos, t))): Expressions.EXPR
            case (pat @ TypedVar(_, Expressions.Single(_, _)), _) => ???
            case (pat @ ConstsPat(consts, pos), path) =>
              val pos = pat.position
              val v = mkGet(path, newRef, pos)
              consts map { c =>
                BINARY_OP(pos, c, BinaryOperation.EQ_OP, v)
              } reduceRight { (c,r) =>
                BINARY_OP(pos, c, BinaryOperation.OR_OP, r)
              }
          }
          val cond = if(conds.isEmpty) {
              Expressions.TRUE(pos): Expressions.EXPR
            } else {
              conds.reduceRight { (c, r) => BINARY_OP(pos, c, BinaryOperation.AND_OP, r): Expressions.EXPR }
            }
          Right(Expressions.IF(mc.position, p.caseType.fold(cond)(t =>
              BINARY_OP(pos,
                Expressions.FUNCTION_CALL(pos, PART.VALID(pos, "_isInstanceOf"), List(refTmp, Expressions.CONST_STRING(pos, t.name))),
                BinaryOperation.AND_OP,
                Expressions.BLOCK(pos, Expressions.LET(pos, newRef.key, newRef, Some(caseType), true), cond))), blockWithNewVar, further))
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

    (cases zip caseTypes).foldRight(default) {
      case ((mc, caseType), furtherEi) =>
        furtherEi match {
          case Right(further) => f(mc, caseType, further)
          case Left(e)        => Left(e)
        }
    }
  }

  private def mkGetter(
      p: Pos,
      ctx: CompilerContext,
      types: List[FINAL],
      fieldName: String,
      expr: EXPR
  ): Either[CompilationError, (CompilerContext, GETTER, FINAL)] = {

    lazy val errMsg =
      if (types.length == 1) types.head.toString
      else s"""Union(${types.mkString("|")})"""

    lazy val err =
      FieldNotFound(p.start, p.end, fieldName, errMsg)
        .asLeft[(CompilerContext, GETTER, FINAL)]

    val getter = GETTER(expr, fieldName)

    types
      .traverse(_.fields.find(_._1 == fieldName).map(_._2))
      .map(TypeInferrer.findCommonType)
      .fold(err)(t => Right((ctx, getter, t)))
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
      case Expressions.AnyType(pos) => (ANY:FINAL).pure[CompileM]
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
      .flatMap(
        res =>
          Either.cond(
            res.errors.isEmpty,
            (res.ctx, res.expr, res.t),
            s"Compilation failed: [${res.errors.map(e => Show[CompilationError].show(e)).mkString("; ")}]"
          )
      )
}
