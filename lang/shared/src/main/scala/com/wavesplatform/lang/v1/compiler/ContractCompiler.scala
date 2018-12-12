package com.wavesplatform.lang.v1.compiler
import cats.Show
import cats.implicits._
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract._
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.CompilationError.Generic
import com.wavesplatform.lang.v1.compiler.CompilerContext.vars
import com.wavesplatform.lang.v1.compiler.ExpressionCompilerV1.handlePart
import com.wavesplatform.lang.v1.compiler.Types.{BOOLEAN, CASETYPEREF}
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.lang.v1.task.imports._

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
          r <- compiler.ExpressionCompilerV1.compileFunc(AnyPos, af.f)
        } yield r
      }
    } yield (annotations, compiledBody)

    r flatMap {
      case (List(c: CallableAnnotation), (func, tpe, _)) =>
        for {
          _ <- Either
            .cond(tpe match {
              case CASETYPEREF("WriteSet", _) => true
              case CASETYPEREF("PaymentSet", _) => true
              case CASETYPEREF("ContractResult", _) => true
              case _                          => false
            }, (), Generic(0, 0, s"ContractFunction must return WriteSet/PaymentSet/ContractResult, but got '$tpe'"))
            .toCompileM
        } yield ContractFunction(c, None, func)
      case (List(c: VerifierAnnotation), (func, tpe, _)) =>
        for {
          _ <- Either
            .cond(tpe match {
              case BOOLEAN => true
              case _       => false
            }, (), Generic(0, 0, s"VerifierFunction must return BOOLEAN, but got '$tpe'"))
            .toCompileM
        } yield VerifierFunction(c, func)
    }
  }

  private def compileContract(contract: Expressions.CONTRACT): CompileM[Contract] = {
    for {
      l <- contract.fs.traverse[CompileM, AnnotatedFunction](compileAnnotatedFunc)
      v  = l.find(_.isInstanceOf[VerifierFunction]).map(_.asInstanceOf[VerifierFunction])
      fs = l.filter(_.isInstanceOf[ContractFunction]).map(_.asInstanceOf[ContractFunction])
    } yield Contract(List.empty, fs, v)
  }

  def apply(c: CompilerContext, contract: Expressions.CONTRACT): Either[String, Contract] =
    compileContract(contract)
      .run(c)
      .map(_._2.leftMap(e => s"Compilation failed: ${Show[CompilationError].show(e)}"))
      .value
}
