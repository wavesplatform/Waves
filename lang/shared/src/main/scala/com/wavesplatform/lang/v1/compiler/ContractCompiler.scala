package com.wavesplatform.lang.v1.compiler

import cats.implicits._
import cats.{Id, Show}
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp._
import com.wavesplatform.lang.contract.meta.{MetaMapper, V1, V2}
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3}
import com.wavesplatform.lang.v1.compiler.CompilationError.{AlreadyDefined, Generic, WrongArgumentType}
import com.wavesplatform.lang.v1.compiler.CompilerContext.{VariableInfo, vars}
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler._
import com.wavesplatform.lang.v1.compiler.Types.{BOOLEAN, BYTESTR, LONG, STRING}
import com.wavesplatform.lang.v1.evaluator.ctx.FunctionTypeSignature
import com.wavesplatform.lang.v1.evaluator.ctx.impl._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types._
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.lang.v1.parser.Expressions.{FUNC, PART, Type}
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.task.imports._
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader, compiler}

object ContractCompiler {

  def compileAnnotatedFunc(
      af: Expressions.ANNOTATEDFUNC,
      version: StdLibVersion,
      saveExprContext: Boolean
  ): CompileM[(Option[AnnotatedFunction], List[(String, Types.FINAL)], Expressions.ANNOTATEDFUNC, Iterable[CompilationError])] = {

    def getCompiledAnnotatedFunc(
        annListWithErr: (Option[Iterable[Annotation]], Iterable[CompilationError]),
        compiledBody: CompilationStepResultDec
    ): CompileM[AnnotatedFunction] = (annListWithErr._1, compiledBody.dec) match {
      case (Some(List(c: CallableAnnotation)), func: Terms.FUNC) =>
        callableReturnType(version)
          .ensureOr(expectedType => callableResultError(expectedType, compiledBody.t))(compiledBody.t <= _)
          .bimap(
            Generic(0, 0, _),
            _ => CallableFunction(c, func): AnnotatedFunction
          )
          .toCompileM

      case (Some(List(c: VerifierAnnotation)), func: Terms.FUNC) =>
        for {
          _ <- Either
            .cond(
              compiledBody.t match {
                case _ if compiledBody.t <= BOOLEAN => true
                case _                              => false
              },
              (),
              Generic(0, 0, s"VerifierFunction must return BOOLEAN or it super type, but got '${compiledBody.t}'")
            )
            .toCompileM
        } yield VerifierFunction(c, func)

      case _ =>
        raiseError(CompilationError.Generic(0, 0, "Annotated function compilation failed"))
    }

    val annotationsWithErrM = af.anns.toList
      .traverse[CompileM, Annotation] { ann =>
        for {
          n    <- handlePart(ann.name)
          args <- ann.args.toList.traverse[CompileM, String](handlePart)
          ann  <- Annotation.parse(n, args).toCompileM
        } yield ann
      }
      .handleError()

    for {
      annotationsWithErr <- annotationsWithErrM
      annotationBindings = annotationsWithErr._1
        .map(
          _.flatMap(_.dic(version).toList).map(nameAndType => (nameAndType._1, VariableInfo(AnyPos, nameAndType._2)))
        )
        .getOrElse(List.empty)
      compiledBody <- local {
        modify[Id, CompilerContext, CompilationError](vars.modify(_)(_ ++ annotationBindings)).flatMap(
          _ => compiler.ExpressionCompiler.compileFunc(af.f.position, af.f, saveExprContext, annotationBindings.map(_._1))
        )
      }
      annotatedFuncWithErr <- getCompiledAnnotatedFunc(annotationsWithErr, compiledBody._1).handleError()

      errorList     = annotatedFuncWithErr._2 ++ annotationsWithErr._2 ++ compiledBody._1.errors
      typedParams   = compiledBody._2
      parseNodeExpr = af.copy(f = compiledBody._1.parseNodeExpr.asInstanceOf[Expressions.FUNC])
      resultAnnFunc = if (annotatedFuncWithErr._2.isEmpty && !compiledBody._1.dec.isItFailed) {
        Some(annotatedFuncWithErr._1.get)
      } else {
        None
      }

    } yield (resultAnnFunc, typedParams, parseNodeExpr, errorList)
  }

  def compileDeclaration(dec: Expressions.Declaration, saveExprContext: Boolean): CompileM[CompilationStepResultDec] = {
    dec match {
      case l: Expressions.LET =>
        for {
          compiledLet      <- compileLet(dec.position, l, saveExprContext)
          updateCtxWithErr <- updateCtx(compiledLet.dec.name, compiledLet.t, dec.position).handleError()
        } yield compiledLet.copy(errors = compiledLet.errors ++ updateCtxWithErr._2)
      case f: FUNC =>
        for {
          compiledFunc <- compileFunc(dec.position, f, saveExprContext)
          (funcName, compiledFuncBodyType, argTypes) = (compiledFunc._1.dec.name, compiledFunc._1.t, compiledFunc._2)
          typeSig                                    = FunctionTypeSignature(compiledFuncBodyType, argTypes, FunctionHeader.User(funcName))
          updateCtxWithErr <- updateCtx(funcName, typeSig, dec.position).handleError()
        } yield compiledFunc._1.copy(errors = compiledFunc._1.errors ++ updateCtxWithErr._2)
    }
  }

  private def compileContract(
      ctx: CompilerContext,
      parsedDapp: Expressions.DAPP,
      version: StdLibVersion,
      saveExprContext: Boolean = false
  ): CompileM[(Option[DApp], Expressions.DAPP, Iterable[CompilationError])] = {
    for {
      decsCompileResult <- parsedDapp.decs.traverse[CompileM, CompilationStepResultDec](dec => compileDeclaration(dec, saveExprContext))
      decs           = decsCompileResult.map(_.dec)
      parsedNodeDecs = decsCompileResult.map(_.parseNodeExpr)
      duplicateVarsErr   <- validateDuplicateVarsInContract(parsedDapp).handleError()
      annFuncArgTypesErr <- validateAnnotatedFuncsArgTypes(ctx, parsedDapp).handleError()
      compiledAnnFuncsWithErr <- parsedDapp.fs
        .traverse[CompileM, (Option[AnnotatedFunction], List[(String, Types.FINAL)], Expressions.ANNOTATEDFUNC, Iterable[CompilationError])](
          af => local(compileAnnotatedFunc(af, version, saveExprContext))
        )
      annotatedFuncs   = compiledAnnFuncsWithErr.filter(_._1.nonEmpty).map(_._1.get)
      parsedNodeAFuncs = compiledAnnFuncsWithErr.map(_._3)

      duplicatedFuncNames = annotatedFuncs.map(_.u.name).groupBy(identity).collect { case (x, List(_, _, _*)) => x }.toList
      alreadyDefinedErr <- Either
        .cond(
          duplicatedFuncNames.isEmpty,
          (),
          AlreadyDefined(parsedDapp.position.start, parsedDapp.position.start, duplicatedFuncNames.mkString(", "), isFunction = true)
        )
        .toCompileM
        .handleError()

      funcArgumentCountErr <- Either
        .cond(
          annotatedFuncs.forall(_.u.args.size <= ContractLimits.MaxInvokeScriptArgs),
          (),
          Generic(
            parsedDapp.position.start,
            parsedDapp.position.end,
            s"Script functions can have no more than ${ContractLimits.MaxInvokeScriptArgs} arguments"
          )
        )
        .toCompileM
        .handleError()

      callableFuncsWithParams = compiledAnnFuncsWithErr.filter(_._1.exists(_.isInstanceOf[CallableFunction]))
      callableFuncs           = callableFuncsWithParams.map(_._1.get.asInstanceOf[CallableFunction])
      callableFuncsTypeInfo = callableFuncsWithParams.map {
        case (_, typedParams, _, _) => typedParams.map(_._2)
      }

      mappedCallableTypes = if (version <= V3)
        MetaMapper.toProto(V1)(callableFuncsTypeInfo)
      else
        MetaMapper.toProto(V2)(callableFuncsTypeInfo)

      metaWithErr <- mappedCallableTypes
        .leftMap(Generic(parsedDapp.position.start, parsedDapp.position.start, _))
        .toCompileM
        .handleError()

      verifierFunctions = annotatedFuncs.filter(_.isInstanceOf[VerifierFunction]).map(_.asInstanceOf[VerifierFunction])
      verifierFuncOptWithErr <- (verifierFunctions match {
        case Nil => Option.empty[VerifierFunction].pure[CompileM]
        case vf :: Nil =>
          if (vf.u.args.isEmpty)
            Option.apply(vf).pure[CompileM]
          else
            raiseError[Id, CompilerContext, CompilationError, Option[VerifierFunction]](
              Generic(parsedDapp.position.start, parsedDapp.position.start, "Verifier function must have 0 arguments")
            )
        case _ =>
          raiseError[Id, CompilerContext, CompilationError, Option[VerifierFunction]](
            Generic(parsedDapp.position.start, parsedDapp.position.start, "Can't have more than 1 verifier function defined")
          )
      }).handleError()

      errorList = duplicateVarsErr._2 ++
        annFuncArgTypesErr._2 ++
        alreadyDefinedErr._2 ++
        funcArgumentCountErr._2 ++
        metaWithErr._2 ++
        verifierFuncOptWithErr._2
      subExprErrorList = decsCompileResult.flatMap(_.errors) ++ compiledAnnFuncsWithErr.flatMap(_._4)
      parsedDappResult = parsedDapp.copy(decs = parsedNodeDecs, fs = parsedNodeAFuncs)

      result = if (errorList.isEmpty && !compiledAnnFuncsWithErr.exists(_._1.isEmpty)) {
        (Some(DApp(metaWithErr._1.get, decs, callableFuncs, verifierFuncOptWithErr._1.get)), parsedDappResult, subExprErrorList)
      } else {
        (None, parsedDappResult, errorList ++ subExprErrorList)
      }
    } yield result
  }

  def handleValid[T](part: PART[T]): CompileM[PART.VALID[T]] = part match {
    case x: PART.VALID[T]         => x.pure[CompileM]
    case PART.INVALID(p, message) => raiseError(Generic(p.start, p.end, message))
  }

  private def validateAnnotatedFuncsArgTypes(ctx: CompilerContext, contract: Expressions.DAPP): CompileM[Unit] =
    contract.fs
      .traverse { func =>
        for {
          funcName <- handleValid(func.f.name)
          funcArgs <- func.f.args.map(_._2).toList.flatTraverse(resolveGenericType(func, funcName, _))
          () <- funcArgs
            .map { case (argType, typeParam) => (argType.v, typeParam.map(_.v)) }
            .find(!checkAnnotatedParamType(_))
            .map(t => argTypesError[Unit](func, funcName, typeStr(t)))
            .getOrElse(().pure[CompileM])
        } yield ()
      }
      .map(_ => ())

  private def argTypesError[T](
      func: Expressions.ANNOTATEDFUNC,
      funcName: PART.VALID[String],
      typeName: String
  ): CompileM[T] =
    raiseError[Id, CompilerContext, CompilationError, T](
      WrongArgumentType(func.f.position.start, func.f.position.end, funcName.v, typeName, allowedCallableTypesV4)
    )

  private def typeStr(t: (String, Option[Type])) =
    t._2.fold(t._1)(typeParam => s"${t._1}[$typeParam]")

  private def resolveGenericType(
    func: Expressions.ANNOTATEDFUNC,
    funcName: PART.VALID[String],
    t: Type
  ): CompileM[List[(PART.VALID[String], Option[PART.VALID[Type]])]] =
    t match {
      case Expressions.Single(name, parameter) =>
        for {
          argType   <- handleValid(name)
          typeParam <- parameter.traverse(handleValid)
        } yield List((argType, typeParam))

      case Expressions.Union(types) =>
        types.toList.flatTraverse(resolveGenericType(func, funcName, _))

      case Expressions.Tuple(types) =>
        argTypesError(func, funcName, types.mkString("(", ", ", ")"))
    }

  private def checkAnnotatedParamType(t: (String, Option[Type])): Boolean = {
    t match {
      case (singleType, None) => primitiveCallableTypes.contains(singleType)
      case (genericType, Some(Expressions.Single(PART.VALID(_, tp), None))) => primitiveCallableTypes.contains(tp) && genericType == "List"
      case (genericType, Some(Expressions.Union(u))) => genericType == "List" && u.forall { t => 
        t match {
          case Expressions.Single(PART.VALID(_, tp), None) => primitiveCallableTypes.contains(tp)
          case _ => false
        }
      }
      case _ => false
    }
  }

  val primitiveCallableTypes: Set[String] =
    Set(LONG, BYTESTR, BOOLEAN, STRING).map(_.name)

  val allowedCallableTypesV4: Set[String] =
    primitiveCallableTypes + "List[]"

  private def validateDuplicateVarsInContract(contract: Expressions.DAPP): CompileM[Any] = {
    for {
      ctx <- get[Id, CompilerContext, CompilationError]
      annotationVars = contract.fs.flatMap(_.anns.flatMap(_.args)).traverse[CompileM, PART.VALID[String]](handleValid)
      annotatedFuncArgs: Seq[(Seq[Expressions.PART[String]], Seq[Expressions.PART[String]])] = contract.fs.map(
        af => (af.anns.flatMap(_.args), af.f.args.map(_._1))
      )
      annAndFuncArgsIntersection = annotatedFuncArgs.toVector.traverse[CompileM, Option[PART.VALID[String]]] {
        case (annSeq, argSeq) =>
          for {
            anns <- annSeq.toList.traverse[CompileM, PART.VALID[String]](handleValid)
            args <- argSeq.toList.traverse[CompileM, PART.VALID[String]](handleValid)
          } yield anns.map(a => args.find(p => a.v == p.v)).find(_.nonEmpty).flatten
      }
      _ <- annotationVars.flatMap(
        a =>
          a.find(v => ctx.varDefs.contains(v.v)).fold(().pure[CompileM]) { p =>
            raiseError[Id, CompilerContext, CompilationError, Unit](
              Generic(p.position.start, p.position.start, s"Annotation binding `${p.v}` overrides already defined var")
            )
          }
      )
      _ <- annAndFuncArgsIntersection.flatMap {
        _.headOption.flatten match {
          case None => ().pure[CompileM]
          case Some(PART.VALID(p, n)) =>
            raiseError[Id, CompilerContext, CompilationError, Unit](Generic(p.start, p.start, s"Script func arg `$n` override annotation bindings"))
        }
      }
    } yield ()
  }

  def apply(c: CompilerContext, contract: Expressions.DAPP, version: StdLibVersion): Either[String, DApp] = {
    compileContract(c, contract, version)
      .run(c)
      .map(
        _._2
          .leftMap(e => s"Compilation failed: ${Show[CompilationError].show(e)}")
          .flatMap(
            res => Either.cond(res._3.isEmpty, res._1.get, s"Compilation failed: [${res._3.map(e => Show[CompilationError].show(e)).mkString("; ")}]")
          )
      )
      .value
  }

  def compile(input: String, ctx: CompilerContext, version: StdLibVersion): Either[String, DApp] = {
    Parser.parseContract(input) match {
      case fastparse.Parsed.Success(xs, _) =>
        ContractCompiler(ctx, xs, version) match {
          case Left(err) => Left(err.toString)
          case Right(c)  => Right(c)
        }
      case f @ fastparse.Parsed.Failure(_, _, _) => Left(f.toString)
    }
  }

  def compileWithParseResult(
      input: String,
      ctx: CompilerContext,
      version: StdLibVersion,
      saveExprContext: Boolean = true
  ): Either[String, (Option[DApp], Expressions.DAPP, Iterable[CompilationError])] = {
    Parser.parseDAPPWithErrorRecovery(input) match {
      case Right((parseResult, removedCharPosOpt)) =>
        compileContract(ctx, parseResult, version, saveExprContext)
          .run(ctx)
          .map(
            _._2
              .map { compRes =>
                val errorList =
                  compRes._3 ++
                    (if (removedCharPosOpt.isEmpty) Nil
                     else
                       List(
                         Generic(
                           removedCharPosOpt.get.start,
                           removedCharPosOpt.get.end,
                           "Parsing failed. Some chars was removed as result of recovery process."
                         )
                       ))
                (compRes._1, compRes._2, errorList)
              }
              .leftMap(e => s"Compilation failed: ${Show[CompilationError].show(e)}")
          )
          .value

      case Left(error) => Left(error.toString)
    }
  }
}
