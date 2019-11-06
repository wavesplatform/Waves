package com.wavesplatform.lang.v1.compiler

import cats.{Id, Show}
import cats.implicits._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp._
import com.wavesplatform.lang.contract.meta.{MetaMapper, V1}
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.compiler.CompilationError.{AlreadyDefined, Generic, WrongArgumentType}
import com.wavesplatform.lang.v1.compiler.CompilerContext.vars
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler._
import com.wavesplatform.lang.v1.compiler.Types.{BOOLEAN, FINAL, UNION}
import com.wavesplatform.lang.v1.evaluator.ctx.FunctionTypeSignature
import com.wavesplatform.lang.v1.evaluator.ctx.impl._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types._
import com.wavesplatform.lang.v1.parser.Expressions.{FUNC, PART, Pos}
import com.wavesplatform.lang.v1.parser.{Expressions, Parser, ParserV2}
import com.wavesplatform.lang.v1.task.imports._
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader, compiler}

import scala.util.{Failure, Success}
object ContractCompiler {

  def compileAnnotatedFunc(
      af: Expressions.ANNOTATEDFUNC,
      version: StdLibVersion
  ): CompileM[(Option[AnnotatedFunction], List[(String, Types.FINAL)], Expressions.ANNOTATEDFUNC, Iterable[CompilationError])] = {

    def getCompiledAnnotatedFunc(
        annListWithErr: (Option[Iterable[Annotation]], Iterable[CompilationError]),
        compiledBody: CompilationStepResultDec
    ): CompileM[AnnotatedFunction] = annListWithErr._1 match {
      case Some(List(c: CallableAnnotation)) =>
        callableReturnType(version)
          .ensureOr(expectedType => callableResultError(expectedType, compiledBody.t))(compiledBody.t <= _)
          .bimap(
            Generic(0, 0, _),
            _ => CallableFunction(c, compiledBody.dec.asInstanceOf[Terms.FUNC]): AnnotatedFunction
          )
          .toCompileM

      case Some(List(c: VerifierAnnotation)) =>
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
        } yield VerifierFunction(c, compiledBody.dec.asInstanceOf[Terms.FUNC])

      case None =>
        raiseError(annListWithErr._2.head)
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
      annotationBindings = annotationsWithErr._1.map(_.flatMap(_.dic(version).toList)).getOrElse(List.empty)
      compiledBody <- local {
        modify[Id, CompilerContext, CompilationError](vars.modify(_)(_ ++ annotationBindings)).flatMap(
          _ => compiler.ExpressionCompiler.compileFunc(af.f.position, af.f, annotationBindings.map(_._1))
        )
      }
      annotatedFuncWithErr <- getCompiledAnnotatedFunc(annotationsWithErr, compiledBody._1).handleError()

      errorList     = annotatedFuncWithErr._2 ++ compiledBody._1.errors
      typedParams   = compiledBody._2
      parseNodeExpr = af.copy(f = compiledBody._1.parseNodeExpr.asInstanceOf[Expressions.FUNC])
      resultAnnFunc = if (annotatedFuncWithErr._2.isEmpty && !compiledBody._1.dec.isItFailed) {
        Some(annotatedFuncWithErr._1.get)
      } else {
        None
      }

    } yield (resultAnnFunc, typedParams, parseNodeExpr, errorList)
  }

  def compileDeclaration(dec: Expressions.Declaration): CompileM[CompilationStepResultDec] = {
    dec match {
      case l: Expressions.LET =>
        for {
          compiledLet      <- compileLet(dec.position, l)
          updateCtxWithErr <- updateCtx(compiledLet.dec.name, compiledLet.t, dec.position).handleError()
        } yield compiledLet.copy(errors = compiledLet.errors ++ updateCtxWithErr._2)
      case f: FUNC =>
        for {
          compiledFunc <- compileFunc(dec.position, f)
          (funcName, compiledFuncBodyType, argTypes) = (compiledFunc._1.dec.name, compiledFunc._1.t, compiledFunc._2)
          typeSig                                    = FunctionTypeSignature(compiledFuncBodyType, argTypes, FunctionHeader.User(funcName))
          updateCtxWithErr <- updateCtx(funcName, typeSig).handleError()
        } yield compiledFunc._1.copy(errors = compiledFunc._1.errors ++ updateCtxWithErr._2)
    }
  }

  private def compileContract(
      ctx: CompilerContext,
      parsedDapp: Expressions.DAPP,
      version: StdLibVersion
  ): CompileM[(Option[DApp], Expressions.DAPP, Iterable[CompilationError])] = {
    for {
      decsCompileResult <- parsedDapp.decs.traverse[CompileM, CompilationStepResultDec](compileDeclaration)
      decs           = decsCompileResult.map(_.dec)
      parsedNodeDecs = decsCompileResult.map(_.parseNodeExpr)
      duplicateVarsErr   <- validateDuplicateVarsInContract(parsedDapp).handleError()
      annFuncArgTypesErr <- validateAnnotatedFuncsArgTypes(ctx, parsedDapp).handleError()
      funcNameWithWrongSize = parsedDapp.fs
        .map(af => Expressions.PART.toOption[String](af.name))
        .filter(fNameOpt => fNameOpt.nonEmpty && fNameOpt.get.getBytes("UTF-8").size > ContractLimits.MaxAnnotatedFunctionNameInBytes)
        .map(_.get)
      funcNameSizeErr <- Either
        .cond(
          funcNameWithWrongSize.isEmpty,
          (),
          Generic(
            parsedDapp.position.start,
            parsedDapp.position.end,
            s"Annotated function name size in bytes must be less than ${ContractLimits.MaxAnnotatedFunctionNameInBytes} for functions with name: ${funcNameWithWrongSize
              .mkString(", ")}"
          )
        )
        .toCompileM
        .handleError()
      compiledAnnFuncsWithErr <- parsedDapp.fs
        .traverse[CompileM, (Option[AnnotatedFunction], List[(String, Types.FINAL)], Expressions.ANNOTATEDFUNC, Iterable[CompilationError])](
          af => local(compileAnnotatedFunc(af, version))
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
      metaWithErr <- MetaMapper
        .toProto(V1)(callableFuncsTypeInfo)
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
        funcNameSizeErr._2 ++
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

  private def validateAnnotatedFuncsArgTypes(ctx: CompilerContext, contract: Expressions.DAPP): CompileM[Any] = {
    for {
      annotatedFuncsArgTypesCM <- contract.fs
        .map { af =>
          (af.f.position, af.f.name, af.f.args.flatMap(_._2))
        }
        .toVector
        .traverse[CompileM, (Pos, String, List[String])] {
          case (pos, funcNamePart, argTypesPart) =>
            for {
              argTypes <- argTypesPart.toList
                .pure[CompileM]
                .ensure(Generic(contract.position.start, contract.position.start, "Annotated function should not have generic parameter types"))(
                  _.forall(_._2.isEmpty)
                )
                .flatMap(_.traverse(t => handleValid(t._1)))
              funcName <- handleValid(funcNamePart)
            } yield (pos, funcName.v, argTypes.map(_.v))
        }
        .pure[CompileM]
      _ <- annotatedFuncsArgTypesCM.flatMap { annotatedFuncs =>
        annotatedFuncs.find(af => af._3.find(!Types.nativeTypeList.contains(_)).nonEmpty).fold(().pure[CompileM]) { af =>
          val wrongArgType = af._3.find(!Types.nativeTypeList.contains(_)).getOrElse("")
          raiseError[Id, CompilerContext, CompilationError, Unit](
            WrongArgumentType(af._1.start, af._1.end, af._2, wrongArgType, Types.nativeTypeList)
          )
        }
      }
    } yield ()
  }

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
          .leftMap(e => s"Compilation failed. ${Show[CompilationError].show(e)}")
          .flatMap(res => Either.cond(res._3.isEmpty, res._1.get, s"Compilation failed: ${res._3.map(e => Show[CompilationError].show(e))}"))
      )
      .value
  }

  def compile(input: String, ctx: CompilerContext, version: StdLibVersion): Either[String, DApp] = {
    Parser.parseContract(input) match {
      case fastparse.core.Parsed.Success(xs, _) =>
        ContractCompiler(ctx, xs, version) match {
          case Left(err) => Left(err.toString)
          case Right(c)  => Right(c)
        }
      case f @ fastparse.core.Parsed.Failure(_, _, _) => Left(f.toString)
    }
  }
  def compileWithParseResult(
      input: String,
      ctx: CompilerContext,
      version: StdLibVersion
  ): Either[String, (Option[DApp], Expressions.DAPP, Iterable[CompilationError])] = {
    ParserV2.parseDAPP(input) match {
      case Right((parseResult, removedCharPosOpt)) =>
        compileContract(ctx, parseResult, version)
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
