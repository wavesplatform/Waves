package com.wavesplatform.lang.v1.compiler

import cats.Monoid
import com.wavesplatform.lang.v1.compiler.CompilerContext._
import com.wavesplatform.lang.v1.compiler.Terms.TYPE
import com.wavesplatform.lang.v1.evaluator.ctx.PredefFunction.FunctionTypeSignature
import com.wavesplatform.lang.v1.evaluator.ctx.{DefinedType, PredefFunction}
import shapeless._

case class CompilerContext(predefTypes: Map[String, DefinedType], varDefs: VariableTypes, functionDefs: FunctionTypes, tmpArgsIdx: Int = 0) {
  def functionTypeSignaturesByName(name: String): Seq[FunctionTypeSignature] = functionDefs.getOrElse(name, Seq.empty)
}

object CompilerContext {

  def build(predefTypes: Seq[DefinedType], varDefs: VariableTypes, functions: Seq[PredefFunction]) = new CompilerContext(
    predefTypes = predefTypes.map(t => t.name -> t).toMap,
    varDefs = varDefs,
    functionDefs = functions.groupBy(_.name).mapValues(_.map(_.signature))
  )

  type VariableTypes = Map[String, TYPE]
  type FunctionTypes = Map[String, Seq[FunctionTypeSignature]]

  val empty = CompilerContext(Map.empty, Map.empty, Map.empty, 0)

  implicit val monoid: Monoid[CompilerContext] = new Monoid[CompilerContext] {
    override val empty: CompilerContext = CompilerContext.empty

    override def combine(x: CompilerContext, y: CompilerContext): CompilerContext =
      CompilerContext(predefTypes = x.predefTypes ++ y.predefTypes, varDefs = x.varDefs ++ y.varDefs, functionDefs = x.functionDefs ++ y.functionDefs)
  }

  val types: Lens[CompilerContext, Map[String, DefinedType]]                    = lens[CompilerContext] >> 'predefTypes
  val vars: Lens[CompilerContext, Map[String, TYPE]]                            = lens[CompilerContext] >> 'varDefs
  val functions: Lens[CompilerContext, Map[String, Seq[FunctionTypeSignature]]] = lens[CompilerContext] >> 'functionDefs
}
