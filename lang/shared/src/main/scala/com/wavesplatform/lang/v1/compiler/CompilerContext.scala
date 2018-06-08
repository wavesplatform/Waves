package com.wavesplatform.lang.v1.compiler

import cats.Monoid
import com.wavesplatform.lang.v1.compiler.CompilerContext._
import com.wavesplatform.lang.v1.compiler.Types.TYPE
import com.wavesplatform.lang.v1.evaluator.ctx.PredefFunction.FunctionTypeSignature
import com.wavesplatform.lang.v1.evaluator.ctx.{DefinedType, PredefFunction}
import shapeless._

case class CompilerContext(predefTypes: Map[String, DefinedType], varDefs: VariableTypes, functionDefs: FunctionTypes, tmpArgsIdx: Int = 0) {
  def functionTypeSignaturesByName(name: String): List[FunctionTypeSignature] = functionDefs.getOrElse(name, List.empty)
  def hasFunction(name: String): Boolean                                      = functionDefs.contains(name)
}

object CompilerContext {

  def build(predefTypes: Seq[DefinedType], varDefs: VariableTypes, functions: Seq[PredefFunction]) = new CompilerContext(
    predefTypes = predefTypes.map(t => t.name -> t).toMap,
    varDefs = varDefs,
    functionDefs = functions.groupBy(_.name).map { case (k, v) => k -> v.map(_.signature).toList }
  )

  type VariableTypes = Map[String, TYPE]
  type FunctionTypes = Map[String, List[FunctionTypeSignature]]

  val empty = CompilerContext(Map.empty, Map.empty, Map.empty, 0)

  implicit val monoid: Monoid[CompilerContext] = new Monoid[CompilerContext] {
    override val empty: CompilerContext = CompilerContext.empty

    override def combine(x: CompilerContext, y: CompilerContext): CompilerContext =
      CompilerContext(predefTypes = x.predefTypes ++ y.predefTypes, varDefs = x.varDefs ++ y.varDefs, functionDefs = x.functionDefs ++ y.functionDefs)
  }

  val types: Lens[CompilerContext, Map[String, DefinedType]] = lens[CompilerContext] >> 'predefTypes
  val vars: Lens[CompilerContext, VariableTypes]             = lens[CompilerContext] >> 'varDefs
  val functions: Lens[CompilerContext, FunctionTypes]        = lens[CompilerContext] >> 'functionDefs
}
