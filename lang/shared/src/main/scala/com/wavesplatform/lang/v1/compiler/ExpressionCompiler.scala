package com.wavesplatform.lang.v1.compiler

import cats.Show
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
import com.wavesplatform.lang.v1.parser.{BinaryOperation, Expressions, Parser, Parser2}
import com.wavesplatform.lang.v1.task.TaskM
import com.wavesplatform.lang.v1.task.imports._

import scala.util.{Failure, Success}

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

  def compileWithParseResult(input: String, ctx: CompilerContext): Either[String, (EXPR, Expressions.EXPR, Iterable[CompilationError])] = {
    Parser2.parseExpression(input) match {
      case Success(parseResult) =>
        compileExprWithCtx(parseResult)
          .run(ctx)
          .value
          ._2
          .map(compRes => (compRes.expr, compRes.parseNodeExpr, compRes.errors))
          .leftMap(e => s"Compilation failed: ${Show[CompilationError].show(e)}")

      case Failure(error) => Left(error.toString)
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

  def compileExprWithCtx(expr: Expressions.EXPR): CompileM[CompilationStepResultExpr] = {
    get[CompilerContext, CompilationError].flatMap { ctx =>
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
        case x: Expressions.CONST_LONG                   => CompilationStepResultExpr(ctx, CONST_LONG(x.value): EXPR, LONG: FINAL, x: Expressions.EXPR).pure[CompileM]
        case x: Expressions.CONST_BYTESTR                => handlePart(x.value).flatMap(b => liftEither(adjustByteStr(x, b)))
        case x: Expressions.CONST_STRING                 => handlePart(x.value).flatMap(s => liftEither(adjustStr(x, s)))
        case x: Expressions.TRUE                         => CompilationStepResultExpr(ctx, TRUE: EXPR, BOOLEAN: FINAL, x: Expressions.EXPR).pure[CompileM]
        case x: Expressions.FALSE                        => CompilationStepResultExpr(ctx, FALSE: EXPR, BOOLEAN: FINAL, x: Expressions.EXPR).pure[CompileM]
        case Expressions.GETTER(p, ref, field, _)        => compileGetter(p, field, ref)
        case Expressions.BLOCK(p, dec, body, _)          => compileBlock(p, dec, body)
        case Expressions.IF(p, cond, ifTrue, ifFalse, _) => compileIf(p, cond, ifTrue, ifFalse)
        case Expressions.REF(p, key, _)                  => compileRef(p, key)
        case Expressions.FUNCTION_CALL(p, name, args, _) => compileFunctionCall(p, name, args)
        case Expressions.MATCH(p, ex, cases, _)          => compileMatch(p, ex, cases.toList)
        case Expressions.INVALID(p, message, _)          => raiseError(Generic(p.start, p.end, message))
        case Expressions.BINARY_OP(p, a, op, b, _) =>
          op match {
            case AND_OP => compileIf(p, a, b, Expressions.FALSE(p))
            case OR_OP  => compileIf(p, a, Expressions.TRUE(p), b)
            case _      => compileFunctionCall(p, PART.VALID(p, BinaryOperation.opsToFunctions(op)), List(a, b))
          }
      }
    }
  }

  // TODO check context
  private def compileIf(
      p: Pos,
      condExpr: Expressions.EXPR,
      ifTrueExpr: Expressions.EXPR,
      ifFalseExpr: Expressions.EXPR
  ): CompileM[CompilationStepResultExpr] =
    for {
      condWithErr <- local {
        compileExprWithCtx(condExpr).map { condCompRes =>
          val error = Some(UnexpectedType(p.start, p.end, BOOLEAN.toString, condCompRes.t.toString)).filter(_ => !(condCompRes.t equivalent BOOLEAN))
          (condCompRes, error)
        }
      }
      ifTrue  <- local(compileExprWithCtx(ifTrueExpr))
      ifFalse <- local(compileExprWithCtx(ifFalseExpr))

      t             = TypeInferrer.findCommonType(ifTrue.t, ifFalse.t)
      parseNodeExpr = Expressions.IF(p, condWithErr._1.parseNodeExpr, ifTrue.parseNodeExpr, ifFalse.parseNodeExpr)
      errorList     = condWithErr._1.errors ++ ifTrue.errors ++ ifFalse.errors

      result = if (condWithErr._2.isEmpty) {
        CompilationStepResultExpr(
          ifFalse.ctx,
          IF(condWithErr._1.expr, ifTrue.expr, ifFalse.expr),
          t,
          parseNodeExpr.copy(resultType = Some(t)),
          errorList
        )
      } else {
        CompilationStepResultExpr(ifFalse.ctx, FAILED_EXPR(), NOTHING, parseNodeExpr, errorList ++ condWithErr._2.map(List(_)).get)
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
      cases: List[Expressions.MATCH_CASE]
  ): CompileM[CompilationStepResultExpr] =
    for {
      ctx       <- get[CompilerContext, CompilationError]
      typedExpr <- compileExprWithCtx(expr)
      exprTypesWithErr <- (typedExpr.t match {
        case u: UNION => u.pure[CompileM]
        case _        => raiseError[CompilerContext, CompilationError, UNION](MatchOnlyUnion(p.start, p.end))
      }).handleError()
      exprTypes = exprTypesWithErr._1.getOrElse(NOTHING)
      tmpArgId  = ctx.tmpArgsIdx
      refTmpKey = "$match" + tmpArgId
      _ <- set[CompilerContext, CompilationError](ctx.copy(tmpArgsIdx = tmpArgId + 1))
      allowShadowVarName = typedExpr.expr match {
        case REF(k) => Some(k)
        case _      => None
      }
      ifCasesWithErr <- inspectFlat[CompilerContext, CompilationError, Expressions.EXPR](
        updatedCtx =>
          mkIfCases(
            updatedCtx,
            cases,
            Expressions.REF(p, PART.VALID(p, refTmpKey)),
            allowShadowVarName,
            exprTypes
          ).toCompileM
      ).handleError()
      compiledMatch <- compileLetBlock(
        p,
        Expressions.LET(p, PART.VALID(p, refTmpKey), expr, Seq.empty),
        ifCasesWithErr._1.getOrElse(Expressions.INVALID(p, ifCasesWithErr._2.map(e => Show[CompilationError].show(e)).mkString_("\n")))
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
      _ <- set[CompilerContext, CompilationError](ctx.copy(tmpArgsIdx = tmpArgId))

      errorList = exprTypesWithErr._2 ++ ifCasesWithErr._2 ++ checkWithErr._2

      result = if (errorList.isEmpty) {
        compiledMatch.copy(errors = compiledMatch.errors ++ typedExpr.errors)
      } else {
        CompilationStepResultExpr(ctx, FAILED_EXPR(), NOTHING, Expressions.MATCH(p, typedExpr.parseNodeExpr, cases), errorList ++ typedExpr.errors)
      }

    } yield result

  def compileBlock(
      pos: Expressions.Pos,
      declaration: Expressions.Declaration,
      expr: Expressions.EXPR
  ): CompileM[CompilationStepResultExpr] =
    declaration match {
      case l: Expressions.LET  => compileLetBlock(pos, l, expr)
      case f: Expressions.FUNC => compileFuncBlock(pos, f, expr)
    }

  private def handleTypeUnion(types: List[String], f: FINAL, ctx: CompilerContext) =
    if (types.isEmpty) f else UNION.create(types.map(ctx.predefTypes))

  private def validateShadowing(p: Pos, dec: Expressions.Declaration, allowedExceptions: List[String] = List.empty): CompileM[String] = {
    for {
      ctx <- get[CompilerContext, CompilationError]
      letName <- handlePart(dec.name)
        .ensureOr(n => AlreadyDefined(p.start, p.end, n, isFunction = false))(
          n => !ctx.varDefs.contains(n) || dec.allowShadowing || allowedExceptions.contains(n)
        )
        .ensureOr(n => AlreadyDefined(p.start, p.end, n, isFunction = true))(n => !ctx.functionDefs.contains(n))
    } yield letName
  }

  def compileLet(p: Pos, let: Expressions.LET): CompileM[CompilationStepResultDec] =
    for {
      letNameWithErr <- validateShadowing(p, let).handleError()
      compiledLet    <- compileExprWithCtx(let.value)
      ctx            <- get[CompilerContext, CompilationError]
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
      ctx <- get[CompilerContext, CompilationError]
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
              types <- liftEither(genericFlat(p, ctx.predefTypes, typeDecls))
              union = UNION.reduce(UNION.create(types))
            } yield (name, union)
        }
        .handleError()
      compiledFuncBody <- local {
        val newArgs: VariableTypes = argTypesWithErr._1.getOrElse(List.empty).toMap
        modify[CompilerContext, CompilationError](vars.modify(_)(_ ++ newArgs))
          .flatMap(_ => compileExprWithCtx(func.expr))
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
    } yield (result, argTypesWithErr._1.getOrElse(List.empty))
  }

  def updateCtx(letName: String, letType: Types.FINAL, p: Pos): CompileM[Unit] =
    modify[CompilerContext, CompilationError](vars.modify(_)(_ + (letName -> letType)))

  def updateCtx(funcName: String, typeSig: FunctionTypeSignature): CompileM[Unit] =
    modify[CompilerContext, CompilationError](functions.modify(_)(_ + (funcName -> List(typeSig))))

  private def compileLetBlock(
      p: Pos,
      let: Expressions.LET,
      body: Expressions.EXPR
  ): CompileM[CompilationStepResultExpr] =
    for {
      compLetResult <- compileLet(p, let)
      letName = compLetResult.dec.name
      compiledBody <- local {
        updateCtx(letName, compLetResult.t, p)
          .flatMap(_ => compileExprWithCtx(body))
      }

      parseNodeExpr = Expressions.BLOCK(p, compLetResult.parseNodeExpr, compiledBody.parseNodeExpr, compiledBody.parseNodeExpr.resultType)
      result = if (!compLetResult.dec.isItFailed) {
        LET_BLOCK(compLetResult.dec.asInstanceOf[LET], compiledBody.expr)
      } else {
        FAILED_EXPR()
      }
    } yield CompilationStepResultExpr(compiledBody.ctx, result, compiledBody.t, parseNodeExpr, compLetResult.errors ++ compiledBody.errors)

  private def compileFuncBlock(
      p: Pos,
      func: Expressions.FUNC,
      body: Expressions.EXPR
  ): CompileM[CompilationStepResultExpr] = {
    for {
      compFuncRes <- compileFunc(p, func)
      (compFuncStepRes, argTypes) = compFuncRes
      funcname                    = compFuncStepRes.dec.name
      typeSig                     = FunctionTypeSignature(compFuncStepRes.t, argTypes, FunctionHeader.User(funcname))
      compiledBody <- local {
        updateCtx(funcname, typeSig)
          .flatMap(_ => compileExprWithCtx(body))
      }

      expr          = BLOCK(compFuncStepRes.dec, compiledBody.expr)
      parseNodeExpr = Expressions.BLOCK(p, compFuncStepRes.parseNodeExpr, compiledBody.parseNodeExpr, compiledBody.parseNodeExpr.resultType)
    } yield CompilationStepResultExpr(compiledBody.ctx, expr, compiledBody.t, parseNodeExpr, compFuncStepRes.errors ++ compiledBody.errors)
  }

  private def compileGetter(
      p: Pos,
      fieldPart: PART[String],
      refExpr: Expressions.EXPR
  ): CompileM[CompilationStepResultExpr] =
    for {
      ctx           <- get[CompilerContext, CompilationError]
      fieldWithErr  <- handlePart(fieldPart).handleError()
      compiledRef   <- compileExprWithCtx(refExpr)
      getterWithErr <- mkGetter(p, ctx, compiledRef.t.typeList, fieldWithErr._1.getOrElse("NO_NAME"), compiledRef.expr).toCompileM.handleError()

      errorList     = fieldWithErr._2 ++ getterWithErr._2
      parseNodeExpr = Expressions.GETTER(p, compiledRef.parseNodeExpr, fieldPart)

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
      args: List[Expressions.EXPR]
  ): CompileM[CompilationStepResultExpr] =
    for {
      ctx         <- get[CompilerContext, CompilationError]
      nameWithErr <- handlePart(namePart).handleError()
      name = nameWithErr._1.getOrElse("NO_NAME")
      signatures   <- get[CompilerContext, CompilationError].map(_.functionTypeSignaturesByName(name))
      compiledArgs <- args.traverse(compileExprWithCtx)
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
      parseNodeExpr = Expressions.FUNCTION_CALL(p, namePart, compiledArgs.map(_.parseNodeExpr))

      result = if (errorList.isEmpty) {
        val (expr, t) = funcCallWithErr._1.get
        CompilationStepResultExpr(ctx, expr, t, parseNodeExpr.copy(resultType = Some(t)), argErrorList)
      } else {
        CompilationStepResultExpr(ctx, FAILED_EXPR(), NOTHING, parseNodeExpr, errorList ++ argErrorList)
      }
    } yield result

  private def compileRef(p: Pos, keyPart: PART[String]): CompileM[CompilationStepResultExpr] =
    for {
      keyWithErr <- handlePart(keyPart).handleError()
      ctx        <- get[CompilerContext, CompilationError]
      typeWithErr = ctx.varDefs
        .get(keyWithErr._1.getOrElse(""))
        .fold[(Option[FINAL], Iterable[CompilationError])]((None, List(DefNotFound(p.start, p.end, keyWithErr._1.getOrElse("")))))(
          t => (Some(t), List.empty)
        )

      errorList = keyWithErr._2 ++ typeWithErr._2

      result = if (errorList.isEmpty) {
        CompilationStepResultExpr(ctx, REF(keyWithErr._1.get), typeWithErr._1.get, Expressions.REF(p, keyPart, Some(typeWithErr._1.get)))
      } else {
        CompilationStepResultExpr(ctx, FAILED_EXPR(), NOTHING, Expressions.REF(p, keyPart), errorList)
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
