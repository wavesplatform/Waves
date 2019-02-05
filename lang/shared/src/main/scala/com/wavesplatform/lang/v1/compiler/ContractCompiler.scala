package com.wavesplatform.lang.v1.compiler
import cats.Show
import cats.implicits._
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract._
import com.wavesplatform.lang.v1.compiler.CompilationError.Generic
import com.wavesplatform.lang.v1.compiler.CompilerContext.vars
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler._
import com.wavesplatform.lang.v1.compiler.Terms.DECLARATION
import com.wavesplatform.lang.v1.compiler.Types.{BOOLEAN, UNION}
import com.wavesplatform.lang.v1.evaluator.ctx.FunctionTypeSignature
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.Expressions.FUNC
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.task.imports._
import com.wavesplatform.lang.v1.{FunctionHeader, compiler}
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
          r <- compiler.ExpressionCompiler.compileFunc(AnyPos, af.f)
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
                                    WavesContext.contractTransferSetType.typeRef,
                                    WavesContext.contractResultType.typeRef) =>
                  true
                case _ => false
              },
              (),
              Generic(0, 0, s"ContractFunction must return WriteSet/TransferSet/ContractResult or it super type, but got '$tpe'")
            )
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

  private def compileContract(contract: Expressions.CONTRACT): CompileM[Contract] = {
    for {
      ds <- contract.decs.traverse[CompileM, DECLARATION](compileDeclaration)
      l  <- contract.fs.traverse[CompileM, AnnotatedFunction](af => local(compileAnnotatedFunc(af)))
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
    } yield Contract(ds, fs, v)
  }

  def apply(c: CompilerContext, contract: Expressions.CONTRACT): Either[String, Contract] =
    compileContract(contract)
      .run(c)
      .map(_._2.leftMap(e => s"Compilation failed: ${Show[CompilationError].show(e)}"))
      .value

  def compile(input: String, ctx: CompilerContext): Either[String, Contract] = {
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
