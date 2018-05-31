package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.compiler.CompilerContext._
import com.wavesplatform.lang.v1.compiler.Terms.TYPE
import com.wavesplatform.lang.v1.evaluator.ctx.PredefFunction.FunctionTypeSignature
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, PredefCaseType}

case class CompilerContext(predefTypes: Map[String, PredefCaseType], varDefs: TypeDefs, functionDefs: FunctionSigs, tmpArgsIdx: Int = 0) {
  def functionTypeSignaturesByName(name: String): Seq[FunctionTypeSignature] = functionDefs.getOrElse(name, Seq.empty)
}

object CompilerContext {

  type TypeDefs     = Map[String, TYPE]
  type FunctionSigs = Map[String, Seq[FunctionTypeSignature]]

  val empty = CompilerContext(Map.empty, Map.empty, Map.empty, 0)

  def fromEvaluationContext(ctx: EvaluationContext): CompilerContext = {
    val map = ctx.functions.values.groupBy(_.name).mapValues(_.map(_.signature).toSeq)
    CompilerContext(predefTypes = ctx.caseTypeDefs, varDefs = ctx.letDefs.mapValues(_.tpe), functionDefs = map)
  }
}
