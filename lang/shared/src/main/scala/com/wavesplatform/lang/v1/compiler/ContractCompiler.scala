package com.wavesplatform.lang.v1.compiler
import cats.Show
import cats.implicits._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp._
import com.wavesplatform.lang.v1.compiler.CompilationError.{AlreadyDefined, Generic}
import com.wavesplatform.lang.v1.compiler.CompilerContext.vars
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler._
import com.wavesplatform.lang.v1.compiler.Terms.DECLARATION
import com.wavesplatform.lang.v1.compiler.Types.{BOOLEAN, UNION}
import com.wavesplatform.lang.v1.evaluator.ctx.FunctionTypeSignature
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{FieldNames, WavesContext}
import com.wavesplatform.lang.v1.parser.Expressions.FUNC
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.task.imports._
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader, compiler}
object ContractCompiler {

  def compileAnnotatedFunc(af: Expressions.ANNOTATEDFUNC): CompileM[AnnotatedFunction] = {
    val annotationsM: CompileM[List[Annotation]] = af.anns.toList.traverse[CompileM, Annotation] { ann =>
      for {
        n    <- handlePart(ann.name)
        args <- ann.args.toList.traverse[CompileM, String](handlePart)
        ann  <- Annotation.parse(n, args).toCompileM
      } yield ann
    }
    val r = for {
      annotations <- annotationsM
      annotationBindings = annotations.flatMap(_.dic.toList).map { case (n, t) => (n, (t, "Annotation-bound value")) }
      compiledBody <- local {
        for {
          _ <- modify[CompilerContext, CompilationError](vars.modify(_)(_ ++ annotationBindings))
          r <- compiler.ExpressionCompiler.compileFunc(af.f.position, af.f, annotationBindings.map(_._1))
        } yield r
      }
    } yield (annotations, compiledBody)

    r flatMap {
      case (List(c: CallableAnnotation), (func, tpe, _)) =>
        for {
          _ <- Either
            .cond(
              tpe match {
                case _
                    if tpe <= UNION(WavesContext.writeSetType.typeRef,
                                    WavesContext.scriptTransferSetType.typeRef,
                                    WavesContext.scriptResultType.typeRef) =>
                  true
                case _ => false
              },
              (),
              Generic(0, 0, s"${FieldNames.Error}, but got '$tpe'")
            )
            .toCompileM
           _ <- Either.cond(
             func.name.getBytes().size <= ContractLimits.MaxCallableFunctionNameInBytes,
             (),
             Generic(af.f.name.position.start, af.f.name.position.end, s"Callable function name size in bytes must be less than ${ContractLimits.MaxCallableFunctionNameInBytes}"))
            .toCompileM
        } yield CallableFunction(c, func)
      case (List(c: VerifierAnnotation), (func, tpe, _)) =>
        for {
          _ <- Either
            .cond(tpe match {
              case _ if tpe <= BOOLEAN => true
              case _                   => false
            }, (), Generic(0, 0, s"VerifierFunction must return BOOLEAN or it super type, but got '$tpe'"))
            .toCompileM
        } yield VerifierFunction(c, func)
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

  private def compileContract(contract: Expressions.DAPP): CompileM[DApp] = {
    for {
      ds <- contract.decs.traverse[CompileM, DECLARATION](compileDeclaration)
      _  <- validateDuplicateVarsInContract(contract)
      l  <- contract.fs.traverse[CompileM, AnnotatedFunction](af => local(compileAnnotatedFunc(af)))
      duplicatedFuncNames = l.map(_.u.name).groupBy(identity).collect { case (x, List(_, _, _*)) => x }.toList
      _ <- Either
        .cond(
          duplicatedFuncNames.isEmpty,
          (),
          AlreadyDefined(contract.position.start, contract.position.start, duplicatedFuncNames.mkString(", "), isFunction = true)
        )
        .toCompileM

      _ <- Either
        .cond(
          l.forall(_.u.args.size <= ContractLimits.MaxInvokeScriptArgs),
          (),
          Generic(contract.position.start,
                  contract.position.end,
                  s"Script functions can have no more than ${ContractLimits.MaxInvokeScriptArgs} arguments")
        )
        .toCompileM
      verifierFunctions = l.filter(_.isInstanceOf[VerifierFunction]).map(_.asInstanceOf[VerifierFunction])
      v <- verifierFunctions match {
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
      fs = l.filter(_.isInstanceOf[CallableFunction]).map(_.asInstanceOf[CallableFunction])
    } yield DApp(ds, fs, v)
  }

  private def validateDuplicateVarsInContract(contract: Expressions.DAPP): CompileM[Any] = {
    for {
      ctx <- get[CompilerContext, CompilationError]
      annotationVars = contract.fs.flatMap(_.anns.flatMap(_.args)).traverse[CompileM, String](handlePart)
      annotatedFuncArgs: Seq[(Seq[Expressions.PART[String]], Seq[Expressions.PART[String]])] = contract.fs.map(af =>
        (af.anns.flatMap(_.args), af.f.args.map(_._1)))
      annAndFuncArgsIntersection = annotatedFuncArgs.toVector.traverse[CompileM, Boolean] {
        case (annSeq, argSeq) =>
          for {
            anns <- annSeq.toList.traverse[CompileM, String](handlePart)
            args <- argSeq.toList.traverse[CompileM, String](handlePart)
          } yield anns.forall(a => args.contains(a))
      }
      _ <- annotationVars
        .ensure(Generic(contract.position.start, contract.position.start, "Annotation bindings overrides already defined var"))(aVs =>
          aVs.forall(!ctx.varDefs.contains(_)))
      _ <- annAndFuncArgsIntersection
        .ensure(Generic(contract.position.start, contract.position.start, "Script func args override annotation bindings")) { is =>
          !(is contains true)
        }
    } yield ()
  }

  def apply(c: CompilerContext, contract: Expressions.DAPP): Either[String, DApp] =
    compileContract(contract)
      .run(c)
      .map(_._2.leftMap(e => s"Compilation failed: ${Show[CompilationError].show(e)}"))
      .value

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
