package com.wavesplatform.lang.v1.compiler
import cats.Show
import cats.implicits._
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract._
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.CompilationError.Generic
import com.wavesplatform.lang.v1.compiler.CompilerContext.vars
import com.wavesplatform.lang.v1.compiler.ExpressionCompilerV1.handlePart
import com.wavesplatform.lang.v1.compiler.Terms.{BLOCK, DECLARATION}
import com.wavesplatform.lang.v1.compiler.Types.{BOOLEAN, UNION}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.lang.v1.parser.Expressions.TRUE
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
    compiler.ExpressionCompilerV1.compileBlock(dec.position, dec, TRUE(AnyPos)).map(el => el._1.asInstanceOf[BLOCK].dec)
  }

  private def compileContract(contract: Expressions.CONTRACT): CompileM[Contract] = {
    for {
      ds <- contract.decs.traverse[CompileM, DECLARATION](compileDeclaration)
      l  <- contract.fs.traverse[CompileM, AnnotatedFunction](compileAnnotatedFunc)
      v  = l.find(_.isInstanceOf[VerifierFunction]).map(_.asInstanceOf[VerifierFunction])
      fs = l.filter(_.isInstanceOf[CallableFunction]).map(_.asInstanceOf[CallableFunction])
    } yield Contract(ds, fs, v)
  }

  def apply(c: CompilerContext, contract: Expressions.CONTRACT): Either[String, Contract] =
    compileContract(contract)
      .run(c)
      .map(_._2.leftMap(e => s"Compilation failed: ${Show[CompilationError].show(e)}"))
      .value
}
