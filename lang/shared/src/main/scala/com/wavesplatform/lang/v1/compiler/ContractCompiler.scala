package com.wavesplatform.lang.v1.compiler
import cats.implicits._
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract.{AnnotatedFunction, Annotation, CallableAnnotation, ContractFunction}
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.ExpressionCompilerV1.handlePart
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
    for {
      annotations <- annotationsM
//      predefs     = annotations.flatMap(_.dic.toList).toMap TODO: unused
      compiledBody <- local {
        compiler.ExpressionCompilerV1.compileFunc(AnyPos, af.f)
      }
    } yield ContractFunction(annotations.head.asInstanceOf[CallableAnnotation], None, compiledBody._1)

  }
  def compileContract(contract: Expressions.CONTRACT): CompileM[Contract] = {
    for {
      l <- contract.fs.traverse[CompileM, AnnotatedFunction](compileAnnotatedFunc)
    } yield Contract(List.empty, l.map(_.asInstanceOf[ContractFunction]), None)
  }
}
