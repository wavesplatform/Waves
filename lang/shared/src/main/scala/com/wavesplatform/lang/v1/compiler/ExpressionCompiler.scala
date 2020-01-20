package com.wavesplatform.lang.v1.compiler

import cats.{Id, Show}
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.CompilationError._
import com.wavesplatform.lang.v1.compiler.CompilerContext._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.{FINAL, _}
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import com.wavesplatform.lang.v1.parser.Expressions.{BINARY_OP, MATCH_CASE, PART, Pos, TypeParam}
import com.wavesplatform.lang.v1.parser.{BinaryOperation, Expressions, Parser, ParserV2}
import com.wavesplatform.lang.v1.task.TaskM
import com.wavesplatform.lang.v1.task.imports._

object ExpressionCompiler {

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

  def compile(input: String, ctx: CompilerContext): Either[String, EXPR] = {
    Parser.parseExpr(input) match {
      case fastparse.core.Parsed.Success(xs, _) =>
        ExpressionCompiler(ctx, xs) match {
          case Left(err)              => Left(err.toString)
          case Right((expr, BOOLEAN)) => Right(expr)
          case Right((_, _))          => Left("Script should return boolean")
        }
      case f @ fastparse.core.Parsed.Failure(_, _, _) => Left(f.toString)
    }
  }

  def compileWithParseResult(
      input: String,
      ctx: CompilerContext,
      saveExprContext: Boolean = true
  ): Either[String, (EXPR, Expressions.SCRIPT, Iterable[CompilationError])] = {
    val res = ParserV2.parseExpression(input)
    res match {
      case Right((parseResult, removedCharPosOpt)) =>
        compileExprWithCtx(parseResult.expr, saveExprContext)
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
    Parser.parseExpr(adjustedDecls) match {
      case fastparse.core.Parsed.Success(xs, _)       => ExpressionCompiler(ctx, xs).map(_._1)
      case f @ fastparse.core.Parsed.Failure(_, _, _) => Left(f.toString)
    }
  }

  def compileExpr(expr: Expressions.EXPR): CompileM[(Terms.EXPR, FINAL, Expressions.EXPR)] =
    compileExprWithCtx(expr).map(r => (r.expr, r.t, r.parseNodeExpr))

  def compileExprWithCtx(expr: Expressions.EXPR, saveExprContext: Boolean = false): CompileM[CompilationStepResultExpr] = {
    get[Id, CompilerContext, CompilationError].flatMap { ctx =>
      // TODO MAP ERROR TO RESULT
      def adjustByteStr(expr: Expressions.CONST_BYTESTR, b: ByteStr) =
        CONST_BYTESTR(b)
          .leftMap(CompilationError.Generic(expr.position.start, expr.position.end, _))
          .map(CompilationStepResultExpr(ctx, _, BYTESTR, expr))

      // TODO MAP ERROR TO RESULT
      def adjustStr(expr: Expressions.CONST_STRING, str: String) =
        CONST_STRING(str)
          .leftMap(CompilationError.Generic(expr.position.start, expr.position.end, _))
          .map(CompilationStepResultExpr(ctx, _, STRING, expr))

      expr match {
        case x: Expressions.CONST_LONG                      => CompilationStepResultExpr(ctx, CONST_LONG(x.value): EXPR, LONG: FINAL, x: Expressions.EXPR).pure[CompileM]
        case x: Expressions.CONST_BYTESTR                   => handlePart(x.value).flatMap(b => liftEither(adjustByteStr(x, b)))
        case x: Expressions.CONST_STRING                    => handlePart(x.value).flatMap(s => liftEither(adjustStr(x, s)))
        case x: Expressions.TRUE                            => CompilationStepResultExpr(ctx, TRUE: EXPR, BOOLEAN: FINAL, x: Expressions.EXPR).pure[CompileM]
        case x: Expressions.FALSE                           => CompilationStepResultExpr(ctx, FALSE: EXPR, BOOLEAN: FINAL, x: Expressions.EXPR).pure[CompileM]
        case Expressions.GETTER(p, ref, field, _, _)        => compileGetter(p, field, ref, saveExprContext)
        case Expressions.BLOCK(p, dec, body, _, _)          => compileBlock(p, dec, body, saveExprContext)
        case Expressions.IF(p, cond, ifTrue, ifFalse, _, _) => compileIf(p, cond, ifTrue, ifFalse, saveExprContext)
        case Expressions.REF(p, key, _, _)                  => compileRef(p, key, saveExprContext)
        case Expressions.FUNCTION_CALL(p, name, args, _, _) => compileFunctionCall(p, name, args, saveExprContext)
        case Expressions.MATCH(p, ex, cases, _, _)          => compileMatch(p, ex, cases.toList, saveExprContext)
        case Expressions.INVALID(p, message, _, _)          => raiseError(Generic(p.start, p.end, message))
        case Expressions.BINARY_OP(p, a, op, b, _, _) =>
          op match {
            case AND_OP => compileIf(p, a, b, Expressions.FALSE(p), saveExprContext)
            case OR_OP  => compileIf(p, a, Expressions.TRUE(p), b, saveExprContext)
            case _      => compileFunctionCall(p, PART.VALID(p, BinaryOperation.opsToFunctions(op)), List(a, b), saveExprContext)
          }
      }
    }
  }

  // TODO check context
  private def compileIf(
      p: Pos,
      condExpr: Expressions.EXPR,
      ifTrueExpr: Expressions.EXPR,
      ifFalseExpr: Expressions.EXPR,
      saveExprContext: Boolean
  ): CompileM[CompilationStepResultExpr] =
    for {
      condWithErr <- local {
        compileExprWithCtx(condExpr, saveExprContext).map { condCompRes =>
          val error = Some(UnexpectedType(p.start, p.end, BOOLEAN.toString, condCompRes.t.toString)).filter(_ => !(condCompRes.t equivalent BOOLEAN))
          (condCompRes, error)
        }
      }
      ifTrue  <- local(compileExprWithCtx(ifTrueExpr, saveExprContext))
      ifFalse <- local(compileExprWithCtx(ifFalseExpr, saveExprContext))

      ctx = ifFalse.ctx
      t   = TypeInferrer.findCommonType(ifTrue.t, ifFalse.t)
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

  private def flat(
      pos: Pos,
      typeDefs: Map[String, FINAL],
      definedTypes: List[String],
      expectedTypes: List[String] = List(),
      varName: Option[String] = None
  ): Either[CompilationError, List[FINAL]] =
    definedTypes.flatTraverse(flatSingle(pos, typeDefs, definedTypes, expectedTypes, varName, _))

  private def flatSingle(
      pos: Pos,
      typeDefs: Map[String, FINAL],
      definedTypesStr: List[String],
      expectedTypes: List[String],
      varName: Option[String],
      typeName: String
  ): Either[CompilationError, List[FINAL]] =
    typeDefs.get(typeName) match {
      case Some(UNION(unionTypes, _)) => Right(unionTypes)
      case Some(realType)             => Right(List(realType))
      case None =>
        Left {
          val messageTypes =
            if (expectedTypes.nonEmpty) expectedTypes
            else definedTypesStr
          TypeNotFound(pos.start, pos.end, typeName, messageTypes, varName)
        }
    }

  private def genericFlat(
      pos: Pos,
      typeDefs: Map[String, FINAL],
      definedTypes: List[(String, Option[String])],
      expectedTypes: List[String] = List(),
      varName: Option[String] = None
  ): Either[CompilationError, List[FINAL]] = {
    def f(t: String) = flatSingle(pos, typeDefs, definedTypes.map(_.toString), expectedTypes, varName, t)
    definedTypes.flatTraverse {
      case (typeName, typeParamO) =>
        typeParamO.fold(f(typeName))(
          paramName =>
            for {
              typeConstr <- findGenericType(pos, typeName)
              typeParam  <- f(paramName)
            } yield List(typeConstr(UNION.reduce(UNION.create(typeParam))))
        )
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
      saveExprContext: Boolean
  ): CompileM[CompilationStepResultExpr] =
    for {
      ctx       <- get[Id, CompilerContext, CompilationError]
      typedExpr <- compileExprWithCtx(expr, saveExprContext)
      exprTypesWithErr <- (typedExpr.t match {
        case u: UNION => u.pure[CompileM]
        case _        => raiseError[Id, CompilerContext, CompilationError, UNION](MatchOnlyUnion(p.start, p.end))
      }).handleError()
      exprTypes = exprTypesWithErr._1.getOrElse(NOTHING)
      tmpArgId  = ctx.tmpArgsIdx
      refTmpKey = "$match" + tmpArgId
      _ <- set[Id, CompilerContext, CompilationError](ctx.copy(tmpArgsIdx = tmpArgId + 1))
      allowShadowVarName = typedExpr.expr match {
        case REF(k) => Some(k)
        case _      => None
      }
      ifCasesWithErr <- inspectFlat[Id, CompilerContext, CompilationError, Expressions.EXPR](
        updatedCtx =>
          mkIfCases(
            updatedCtx,
            cases,
            Expressions.REF(p, PART.VALID(p, refTmpKey), ctxOpt = saveExprContext.toOption(updatedCtx.getSimpleContext())),
            allowShadowVarName,
            exprTypes
          ).toCompileM
      ).handleError()
      compiledMatch <- compileLetBlock(
        p,
        Expressions.LET(p, PART.VALID(p, refTmpKey), expr, Seq.empty),
        ifCasesWithErr._1.getOrElse(
          Expressions.INVALID(p, ifCasesWithErr._2.map(e => Show[CompilationError].show(e)).mkString_("\n"), ctxOpt = saveExprContext.toOption(ctx.getSimpleContext()))
        ),
        saveExprContext
      )
      checkWithErr <- cases
        .flatMap(_.types)
        .traverse[CompileM, String](handlePart)
        .flatMap(tl => liftEither(flat(p, ctx.predefTypes, tl)))
        .map(t => UNION.create(t))
        .flatMap(matchedTypes => {
          Either
            .cond(
              (cases.last.types.isEmpty && (exprTypes >= matchedTypes)) || (exprTypes equivalent matchedTypes),
              (),
              MatchNotExhaustive(p.start, p.end, exprTypes.typeList, matchedTypes.typeList)
            )
            .toCompileM
        })
        .handleError()
      _ <- set[Id, CompilerContext, CompilationError](ctx.copy(tmpArgsIdx = tmpArgId))

      errorList = exprTypesWithErr._2 ++ ifCasesWithErr._2 ++ checkWithErr._2

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

  def compileBlock(
      pos: Expressions.Pos,
      declaration: Expressions.Declaration,
      expr: Expressions.EXPR,
      saveExprContext: Boolean
  ): CompileM[CompilationStepResultExpr] =
    declaration match {
      case l: Expressions.LET  => compileLetBlock(pos, l, expr, saveExprContext)
      case f: Expressions.FUNC => compileFuncBlock(pos, f, expr, saveExprContext)
    }

  private def handleTypeUnion(types: List[String], f: FINAL, ctx: CompilerContext) =
    if (types.isEmpty) f else UNION.create(types.map(ctx.predefTypes))

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

  def compileLet(p: Pos, let: Expressions.LET, saveExprContext: Boolean): CompileM[CompilationStepResultDec] =
    for {
      letNameWithErr <- validateShadowing(p, let).handleError()
      compiledLet    <- compileExprWithCtx(let.value, saveExprContext)
      ctx            <- get[Id, CompilerContext, CompilationError]
      letTypesWithErr <- let.types.toList
        .traverse[CompileM, String](handlePart)
        .ensure(NonExistingType(p.start, p.end, letNameWithErr._1.getOrElse("NO_NAME"), ctx.predefTypes.keys.toList))(
          _.forall(ctx.predefTypes.contains)
        )
        .handleError()

      typeUnion     = handleTypeUnion(letTypesWithErr._1.getOrElse(List.empty), compiledLet.t, ctx)
      errorList     = letNameWithErr._2 ++ letTypesWithErr._2
      parseNodeDecl = let.copy(value = compiledLet.parseNodeExpr)

      result = if (errorList.isEmpty) {
        CompilationStepResultDec(ctx, LET(letNameWithErr._1.get, compiledLet.expr), typeUnion, parseNodeDecl, compiledLet.errors)
      } else {
        CompilationStepResultDec(ctx, FAILED_DEC(), typeUnion, parseNodeDecl, errorList ++ compiledLet.errors)
      }
    } yield result

  def compileFunc(
      p: Pos,
      func: Expressions.FUNC,
      saveExprContext: Boolean,
      annListVars: List[String] = List.empty
  ): CompileM[(CompilationStepResultDec, List[(String, FINAL)])] = {
    for {
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
              name <- handlePart(argName)
              typeDecls <- argType.toList
                .traverse(handleGenericPart)
                .ensure(NonExistingType(p.start, p.end, funcNameWithErr._1.getOrElse("NO_NAME"), ctx.predefTypes.keys.toList))(_.forall {
                  case (t, param) => param.fold(ctx.predefTypes.contains(t))(ctx.predefTypes.contains)
                })
              types <- liftEither[Id, CompilerContext, CompilationError, List[FINAL]](genericFlat(p, ctx.predefTypes, typeDecls))
              union = UNION.reduce(UNION.create(types))
            } yield (name, VariableInfo(argName.position, union))
        }
        .handleError()
      compiledFuncBody <- local {
        val newArgs: VariableTypes = argTypesWithErr._1.getOrElse(List.empty).toMap
        modify[Id, CompilerContext, CompilationError](vars.modify(_)(_ ++ newArgs))
          .flatMap(_ => compileExprWithCtx(func.expr, saveExprContext))
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
      saveExprContext: Boolean
  ): CompileM[CompilationStepResultExpr] =
    for {
      compLetResult <- compileLet(p, let, saveExprContext)
      letName = compLetResult.dec.name
      compiledBody <- local {
        updateCtx(letName, compLetResult.t, p)
          .flatMap(_ => compileExprWithCtx(body, saveExprContext))
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
      saveExprContext: Boolean
  ): CompileM[CompilationStepResultExpr] = {
    for {
      compFuncRes <- compileFunc(p, func, saveExprContext)
      (compFuncStepRes, argTypes) = compFuncRes
      funcname                    = compFuncStepRes.dec.name
      typeSig                     = FunctionTypeSignature(compFuncStepRes.t, argTypes, FunctionHeader.User(funcname))
      compiledBody <- local {
        updateCtx(funcname, typeSig, p)
          .flatMap(_ => compileExprWithCtx(body, saveExprContext))
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
      saveExprContext: Boolean
  ): CompileM[CompilationStepResultExpr] =
    for {
      ctx           <- get[Id, CompilerContext, CompilationError]
      fieldWithErr  <- handlePart(fieldPart).handleError()
      compiledRef   <- compileExprWithCtx(refExpr, saveExprContext)
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
      saveExprContext: Boolean
  ): CompileM[CompilationStepResultExpr] =
    for {
      ctx         <- get[Id, CompilerContext, CompilationError]
      nameWithErr <- handlePart(namePart).handleError()
      name = nameWithErr._1.getOrElse("NO_NAME")
      signatures   <- get[Id, CompilerContext, CompilationError].map(_.functionTypeSignaturesByName(name))
      compiledArgs <- args.traverse(arg => compileExprWithCtx(arg, saveExprContext))
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

      errorList     = nameWithErr._2 ++ funcCallWithErr._2
      argErrorList  = compiledArgs.flatMap(_.errors)
      parseNodeExpr = Expressions.FUNCTION_CALL(p, namePart, compiledArgs.map(_.parseNodeExpr), ctxOpt = saveExprContext.toOption(ctx.getSimpleContext()))

      result = if (errorList.isEmpty) {
        val (expr, t) = funcCallWithErr._1.get
        CompilationStepResultExpr(ctx, expr, t, parseNodeExpr.copy(resultType = Some(t)), argErrorList)
      } else {
        CompilationStepResultExpr(ctx, FAILED_EXPR(), NOTHING, parseNodeExpr, errorList ++ argErrorList)
      }
    } yield result

  private def compileRef(p: Pos, keyPart: PART[String], saveExprContext: Boolean): CompileM[CompilationStepResultExpr] =
    for {
      keyWithErr <- handlePart(keyPart).handleError()
      ctx        <- get[Id, CompilerContext, CompilationError]
      typeWithErr = ctx.varDefs
        .get(keyWithErr._1.getOrElse(""))
        .fold[(Option[FINAL], Iterable[CompilationError])]((None, List(DefNotFound(p.start, p.end, keyWithErr._1.getOrElse("")))))(
          info => (Some(info.vType), List.empty)
        )

      errorList = keyWithErr._2 ++ typeWithErr._2

      result = if (errorList.isEmpty) {
        CompilationStepResultExpr(
          ctx,
          REF(keyWithErr._1.get),
          typeWithErr._1.get,
          Expressions.REF(p, keyPart, Some(typeWithErr._1.get), ctxOpt = saveExprContext.toOption(ctx.getSimpleContext()))
        )
      } else {
        CompilationStepResultExpr(ctx, FAILED_EXPR(), NOTHING, Expressions.REF(p, keyPart, ctxOpt = saveExprContext.toOption(ctx.getSimpleContext())), errorList)
      }
    } yield result

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
      } yield {
        val resultType = toFinal(f.result, resolvedTypeParams)
        (FUNCTION_CALL(f.header, args): EXPR, resultType)
      }
    }
  }

  def mkIf(p: Pos, cond: EXPR, ifTrue: (EXPR, FINAL), ifFalse: (EXPR, FINAL)): Either[CompilationError, (EXPR, FINAL)] = {
    val t = TypeInferrer.findCommonType(ifTrue._2, ifFalse._2)
    (IF(cond, ifTrue._1, ifFalse._1), t).asRight
  }

  def mkIfCases(
      ctx: CompilerContext,
      cases: List[MATCH_CASE],
      refTmp: Expressions.REF,
      allowShadowVarName: Option[String],
      exprTypes: FINAL
  ): Either[CompilationError, Expressions.EXPR] = {

    def f(mc: MATCH_CASE, further: Expressions.EXPR): Either[CompilationError, Expressions.EXPR] = {
      val blockWithNewVar = mc.newVarName.fold(mc.expr) { nv =>
        val allowShadowing = nv match {
          case PART.VALID(_, x) => allowShadowVarName.contains(x)
          case _                => false
        }
        Expressions.BLOCK(mc.position, Expressions.LET(mc.position, nv, refTmp, mc.types, allowShadowing), mc.expr)
      }
      mc.types.toList match {
        case Nil => Right(blockWithNewVar)
        case types =>
          def isInst(matchType: String): Expressions.EXPR =
            Expressions
              .FUNCTION_CALL(
                mc.position,
                PART.VALID(mc.position, PureContext._isInstanceOf.name),
                List(refTmp, Expressions.CONST_STRING(mc.position, PART.VALID(mc.position, matchType)))
              )

          for {
            types <- flat(
              pos = mc.position,
              typeDefs = ctx.predefTypes,
              definedTypes = types.map(_.asInstanceOf[PART.VALID[String]].v),
              expectedTypes = exprTypes.typeList.map(_.name),
              allowShadowVarName
            )
            cases <- types.map(_.name) match {
              case hType :: tTypes =>
                val typeIf =
                  tTypes.foldLeft(isInst(hType))((other, matchType) => BINARY_OP(mc.position, isInst(matchType), BinaryOperation.OR_OP, other))
                Right(Expressions.IF(mc.position, typeIf, blockWithNewVar, further))
              case Nil => ???
            }
          } yield cases
      }
    }

    val default: Either[CompilationError, Expressions.EXPR] = Right(
      Expressions.FUNCTION_CALL(cases.head.position, PART.VALID(cases.head.position, "throw"), List.empty)
    )

    cases.foldRight(default) {
      case (mc, furtherEi) =>
        furtherEi match {
          case Right(further) => f(mc, further)
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

  private def handleGenericPart(
      decl: (PART[String], TypeParam)
  ): TaskM[CompilerContext, CompilationError, (String, Option[String])] =
    for {
      t1 <- handlePart(decl._1)
      t2 <- decl._2.traverse(handlePart)
    } yield (t1, t2)

  def handlePart[T](part: PART[T]): CompileM[T] = part match {
    case PART.VALID(_, x)         => x.pure[CompileM]
    case PART.INVALID(p, message) => raiseError(Generic(p.start, p.end, message))
  }

  implicit class RichBoolean(val b: Boolean) extends AnyVal {
    final def toOption[A](a: => A): Option[A] = if (b) Some(a) else None
  }

  def apply(c: CompilerContext, expr: Expressions.EXPR): Either[String, (EXPR, FINAL)] =
    applyWithCtx(c, expr).map(r => (r._2, r._3))

  def applyWithCtx(
      c: CompilerContext,
      expr: Expressions.EXPR
  ): Either[String, (CompilerContext, EXPR, FINAL)] =
    compileExprWithCtx(expr)
      .run(c)
      .value
      ._2
      .leftMap(e => s"Compilation failed. ${Show[CompilationError].show(e)}")
      .flatMap(
        res =>
          Either.cond(res.errors.isEmpty, (res.ctx, res.expr, res.t), s"Compilation failed: ${res.errors.map(e => Show[CompilationError].show(e))}")
      )
}
