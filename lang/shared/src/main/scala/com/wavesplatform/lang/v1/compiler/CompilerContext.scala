package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.compiler.CompilerContext._
import com.wavesplatform.lang.v1.compiler.Terms.TYPE
import com.wavesplatform.lang.v1.ctx.PredefFunction.FunctionTypeSignature
import com.wavesplatform.lang.v1.ctx.{Context, PredefType}

case class CompilerContext(predefTypes: Map[String, PredefType], varDefs: TypeDefs, functionDefs: FunctionSigs) {
  def functionTypeSignaturesByName(name: String): Seq[FunctionTypeSignature] = functionDefs.getOrElse(name, Seq.empty)
}

object CompilerContext {

  type TypeDefs     = Map[String, TYPE]
  type FunctionSigs = Map[String, Seq[FunctionTypeSignature]]

  val empty = CompilerContext(Map.empty, Map.empty, Map.empty)

  def fromExecutionContext(ctx: Context): CompilerContext = {
    val map = ctx.functions.values.groupBy(_.name).mapValues(_.map(_.signature).toSeq)
    CompilerContext(predefTypes = ctx.typeDefs, varDefs = ctx.letDefs.mapValues(_.tpe), functionDefs = map)
  }
}
