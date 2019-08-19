package com.wavesplatform.lang.v1.compiler

import cats.Show
import cats.implicits._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp._
import com.wavesplatform.lang.contract.meta.{MetaMapper, V1}
import com.wavesplatform.lang.v1.compiler.CompilationError.{AlreadyDefined, Generic, WrongArgumentType}
import com.wavesplatform.lang.v1.compiler.CompilerContext.vars
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler._
import com.wavesplatform.lang.v1.compiler.Terms.DECLARATION
import com.wavesplatform.lang.v1.compiler.Types.{BOOLEAN, UNION}
import com.wavesplatform.lang.v1.evaluator.ctx.FunctionTypeSignature
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{FieldNames, WavesContext}
import com.wavesplatform.lang.v1.parser.Expressions.{FUNC, PART, Pos}
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.task.imports._
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader, compiler}
object ContractCompiler {

  def compileAnnotatedFunc(af: Expressions.ANNOTATEDFUNC): CompileM[(AnnotatedFunction, List[(String, Types.FINAL)])] = {
    val annotationsM: CompileM[List[Annotation]] = af.anns.toList.traverse[CompileM, Annotation] { ann =>
      for {
        n    <- handlePart(ann.name)
        args <- ann.args.toList.traverse[CompileM, String](handlePart)
        ann  <- Annotation.parse(n, args).toCompileM
      } yield ann
    }
    val r = for {
      annotations <- annotationsM
      annotationBindings = annotations.flatMap(_.dic.toList)
      compiledBody <- local {
        for {
          _ <- modify[CompilerContext, CompilationError](vars.modify(_)(_ ++ annotationBindings))
          r <- compiler.ExpressionCompiler.compileFunc(af.f.position, af.f, annotationBindings.map(_._1))
        } yield r
      }
    } yield (annotations, compiledBody)

    r flatMap {
      case (List(c: CallableAnnotation), (func, tpe, typedParams)) =>
        for {
          _ <- Either
            .cond(
              tpe match {
                case _ if tpe <= UNION(WavesContext.writeSetType, WavesContext.scriptTransferSetType, WavesContext.scriptResultType) =>
                  true
                case _ => false
              },
              (),
              Generic(0, 0, s"${FieldNames.Error}, but got '$tpe'")
            )
            .toCompileM
        } yield (CallableFunction(c, func), typedParams)

      case (List(c: VerifierAnnotation), (func, tpe, typedParams)) =>
        for {
          _ <- Either
            .cond(tpe match {
              case _ if tpe <= BOOLEAN => true
              case _                   => false
            }, (), Generic(0, 0, s"VerifierFunction must return BOOLEAN or it super type, but got '$tpe'"))
            .toCompileM
        } yield (VerifierFunction(c, func), typedParams)
    }
  }

  def compileDeclaration(dec: Expressions.Declaration): CompileM[DECLARATION] = {
    dec match {
      case l: Expressions.LET =>
        for {
          compiledLet <- compileLet(dec.position, l)
          (letName, letType, letExpr) = compiledLet
          _ <- updateCtx(letName, letType, dec.position)
        } yield Terms.LET(letName, letExpr)
      case f: FUNC =>
        for {
          cf <- compileFunc(dec.position, f)
          (func, compiledFuncBodyType, argTypes) = cf
          typeSig                                = FunctionTypeSignature(compiledFuncBodyType, argTypes, FunctionHeader.User(func.name))
          _ <- updateCtx(func.name, typeSig)
        } yield func
    }
  }

  private def compileContract(ctx: CompilerContext, contract: Expressions.DAPP): CompileM[DApp] = {
    for {
      decs <- contract.decs.traverse[CompileM, DECLARATION](compileDeclaration)
      _    <- validateDuplicateVarsInContract(contract)
      _    <- validateAnnotatedFuncsArgTypes(ctx, contract)
      funcNameWithWrongSize = contract.fs
        .map(af => Expressions.PART.toOption[String](af.name))
        .filter(fNameOpt => fNameOpt.nonEmpty && fNameOpt.get.getBytes("UTF-8").size > ContractLimits.MaxAnnotatedFunctionNameInBytes)
        .map(_.get)
      _ <- Either
        .cond(
          funcNameWithWrongSize.isEmpty,
          (),
          Generic(
            contract.position.start,
            contract.position.end,
            s"Annotated function name size in bytes must be less than ${ContractLimits.MaxAnnotatedFunctionNameInBytes} for functions with name: ${funcNameWithWrongSize
              .mkString(", ")}"
          )
        )
        .toCompileM
      l <- contract.fs.traverse[CompileM, (AnnotatedFunction, List[(String, Types.FINAL)])](af => local(compileAnnotatedFunc(af)))
      annotatedFuncs = l.map(_._1)

      duplicatedFuncNames = annotatedFuncs.map(_.u.name).groupBy(identity).collect { case (x, List(_, _, _*)) => x }.toList
      _ <- Either
        .cond(
          duplicatedFuncNames.isEmpty,
          (),
          AlreadyDefined(contract.position.start, contract.position.start, duplicatedFuncNames.mkString(", "), isFunction = true)
        )
        .toCompileM

      _ <- Either
        .cond(
          annotatedFuncs.forall(_.u.args.size <= ContractLimits.MaxInvokeScriptArgs),
          (),
          Generic(contract.position.start,
                  contract.position.end,
                  s"Script functions can have no more than ${ContractLimits.MaxInvokeScriptArgs} arguments")
        )
        .toCompileM

      callableFuncsWithParams = l.filter(_._1.isInstanceOf[CallableFunction])
      callableFuncs = callableFuncsWithParams.map(_._1.asInstanceOf[CallableFunction])
      callableFuncsTypeInfo = callableFuncsWithParams.map {
        case (f, typedParams) => typedParams.map(_._2)
      }
      meta <- MetaMapper.toProto(V1)(callableFuncsTypeInfo)
        .leftMap(Generic(contract.position.start, contract.position.start, _))
        .toCompileM

      verifierFunctions = annotatedFuncs.filter(_.isInstanceOf[VerifierFunction]).map(_.asInstanceOf[VerifierFunction])
      verifierFuncOpt <- verifierFunctions match {
        case Nil => Option.empty[VerifierFunction].pure[CompileM]
        case vf :: Nil =>
          if (vf.u.args.isEmpty)
            Option.apply(vf).pure[CompileM]
          else
            raiseError[CompilerContext, CompilationError, Option[VerifierFunction]](
              Generic(contract.position.start, contract.position.start, "Verifier function must have 0 arguments"))
        case _ =>
          raiseError[CompilerContext, CompilationError, Option[VerifierFunction]](
            Generic(contract.position.start, contract.position.start, "Can't have more than 1 verifier function defined"))
      }
    } yield DApp(meta, decs, callableFuncs, verifierFuncOpt)
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
              argTypes <- argTypesPart.toList.pure[CompileM]
                .ensure(Generic(contract.position.start, contract.position.start, "Annotated function should not have generic parameter types"))(_.forall(_._2.isEmpty))
                .flatMap(_.traverse(t => handleValid(t._1)))
              funcName <- handleValid(funcNamePart)
            } yield (pos, funcName.v, argTypes.map(_.v))
        }
        .pure[CompileM]
      _ <- annotatedFuncsArgTypesCM.flatMap { annotatedFuncs =>
        annotatedFuncs.find(af => af._3.find(!Types.nativeTypeList.contains(_)).nonEmpty).fold(().pure[CompileM]) { af =>
          val wrongArgType = af._3.find(!Types.nativeTypeList.contains(_)).getOrElse("")
          raiseError[CompilerContext, CompilationError, Unit](WrongArgumentType(af._1.start, af._1.end, af._2, wrongArgType, Types.nativeTypeList))
        }
      }
    } yield ()
  }

  private def validateDuplicateVarsInContract(contract: Expressions.DAPP): CompileM[Any] = {
    for {
      ctx <- get[CompilerContext, CompilationError]
      annotationVars = contract.fs.flatMap(_.anns.flatMap(_.args)).traverse[CompileM, PART.VALID[String]](handleValid)
      annotatedFuncArgs: Seq[(Seq[Expressions.PART[String]], Seq[Expressions.PART[String]])] = contract.fs.map(af =>
        (af.anns.flatMap(_.args), af.f.args.map(_._1)))
      annAndFuncArgsIntersection = annotatedFuncArgs.toVector.traverse[CompileM, Option[PART.VALID[String]]] {
        case (annSeq, argSeq) =>
          for {
            anns <- annSeq.toList.traverse[CompileM, PART.VALID[String]](handleValid)
            args <- argSeq.toList.traverse[CompileM, PART.VALID[String]](handleValid)
          } yield anns.map(a => args.find(p => a.v == p.v)).find(_.nonEmpty).flatten
      }
      _ <- annotationVars.flatMap(a =>
        a.find(v => ctx.varDefs.contains(v.v)).fold(().pure[CompileM]) { p =>
          raiseError[CompilerContext, CompilationError, Unit](
            Generic(p.position.start, p.position.start, s"Annotation binding `${p.v}` overrides already defined var"))
      })
      _ <- annAndFuncArgsIntersection.flatMap {
        _.headOption.flatten match {
          case None => ().pure[CompileM]
          case Some(PART.VALID(p, n)) =>
            raiseError[CompilerContext, CompilationError, Unit](Generic(p.start, p.start, s"Script func arg `$n` override annotation bindings"))
        }
      }
    } yield ()
  }

  def apply(c: CompilerContext, contract: Expressions.DAPP): Either[String, DApp] = {
    compileContract(c, contract)
      .run(c)
      .map(_._2.leftMap(e => s"Compilation failed: ${Show[CompilationError].show(e)}"))
      .value
  }

  def compile(input: String, ctx: CompilerContext): Either[String, DApp] = {
    Parser.parseContract(input) match {
      case fastparse.core.Parsed.Success(xs, _) =>
        ContractCompiler(ctx, xs) match {
          case Left(err) => Left(err.toString)
          case Right(c)  => Right(c)
        }
      case f @ fastparse.core.Parsed.Failure(_, _, _) => Left(f.toString)
    }
  }
}
