package com.wavesplatform.lang.v1.compiler

import cats.Monoid
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.CompilerContext._
import com.wavesplatform.lang.v1.compiler.Types.{CASETYPEREF, FINAL}
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.ctx.{BaseFunction, FunctionTypeSignature}
import shapeless._

case class CompilerContext(predefTypes: Map[String, FINAL], varDefs: VariableTypes, functionDefs: FunctionTypes, tmpArgsIdx: Int = 0) {
  private lazy val allFuncDefs: FunctionTypes = predefTypes.collect {
    case (_, t @ CASETYPEREF(typeName, fields)) =>
      typeName -> List(FunctionTypeSignature(CASETYPEREF(typeName, fields), fields, FunctionHeader.User(typeName)))
  } ++ functionDefs

  def functionTypeSignaturesByName(name: String): List[FunctionTypeSignature] = allFuncDefs.getOrElse(name, List.empty)

}

object CompilerContext {

  def build(predefTypes: Seq[FINAL], varDefs: VariableTypes, functions: Seq[BaseFunction[NoContext]]) = new CompilerContext(
    predefTypes = predefTypes.map(t => t.name -> t).toMap,
    varDefs = varDefs,
    functionDefs = functions.groupBy(_.name).map { case (k, v) => k -> v.map(_.signature).toList }
  )

  type VariableTypes = Map[String, FINAL]
  type FunctionTypes = Map[String, List[FunctionTypeSignature]]

  val empty = CompilerContext(Map.empty, Map.empty, Map.empty, 0)

  implicit val monoid: Monoid[CompilerContext] = new Monoid[CompilerContext] {
    override val empty: CompilerContext = CompilerContext.empty

    override def combine(x: CompilerContext, y: CompilerContext): CompilerContext =
      CompilerContext(predefTypes = x.predefTypes ++ y.predefTypes, varDefs = x.varDefs ++ y.varDefs, functionDefs = x.functionDefs ++ y.functionDefs)
  }

  val types: Lens[CompilerContext, Map[String, FINAL]] = lens[CompilerContext] >> 'predefTypes
  val vars: Lens[CompilerContext, VariableTypes]       = lens[CompilerContext] >> 'varDefs
  val functions: Lens[CompilerContext, FunctionTypes]  = lens[CompilerContext] >> 'functionDefs
}
