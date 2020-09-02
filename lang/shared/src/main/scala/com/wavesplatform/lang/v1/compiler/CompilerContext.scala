package com.wavesplatform.lang.v1.compiler

import cats.Monoid
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.CompilerContext._
import com.wavesplatform.lang.v1.compiler.Types.{CASETYPEREF, FINAL}
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.ctx.{BaseFunction, FunctionTypeSignature}
import com.wavesplatform.lang.v1.parser.Expressions.Pos
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import shapeless._

case class CompilerContext(predefTypes: Map[String, FINAL], varDefs: VariableTypes, functionDefs: FunctionTypes, tmpArgsIdx: Int = 0) {
  private lazy val allFuncDefs: FunctionTypes =
    predefTypes.collect {
      case (_, t @ CASETYPEREF(typeName, fields, false)) =>
        typeName ->
          FunctionInfo(AnyPos, List(FunctionTypeSignature(CASETYPEREF(typeName, fields), fields, FunctionHeader.User(typeName))))
    } ++ functionDefs

  def functionTypeSignaturesByName(name: String): List[FunctionTypeSignature] = allFuncDefs.getOrElse(name, FunctionInfo(AnyPos, List.empty)).fSigList

  def getSimpleContext(): Map[String, Pos] = {
    (varDefs.map(el => el._1 -> el._2.pos) ++ functionDefs.map(el => el._1 -> el._2.pos))
      .filter(_._2.start != -1)
  }
}

object CompilerContext {

  def build(predefTypes: Seq[FINAL], varDefs: VariableTypes, functions: Seq[BaseFunction[NoContext]]) = new CompilerContext(
    predefTypes = predefTypes.map(t => t.name -> t).toMap,
    varDefs = varDefs,
    functionDefs = functions.groupBy(_.name).map { case (k, v) => k -> FunctionInfo(AnyPos, v.map(_.signature).toList) }
  )

  case class VariableInfo(pos: Pos, vType: FINAL)
  case class FunctionInfo(pos: Pos, fSigList: List[FunctionTypeSignature])

  type VariableTypes = Map[String, VariableInfo]
  type FunctionTypes = Map[String, FunctionInfo]

  val empty = CompilerContext(Map.empty, Map.empty, Map.empty, 0)

  implicit val monoid: Monoid[CompilerContext] = new Monoid[CompilerContext] {
    override val empty: CompilerContext = CompilerContext.empty

    override def combine(x: CompilerContext, y: CompilerContext): CompilerContext =
      CompilerContext(predefTypes = x.predefTypes ++ y.predefTypes, varDefs = x.varDefs ++ y.varDefs, functionDefs = x.functionDefs ++ y.functionDefs)
  }

  val types: Lens[CompilerContext, Map[String, FINAL]] = lens[CompilerContext] >> Symbol("predefTypes")
  val vars: Lens[CompilerContext, VariableTypes]       = lens[CompilerContext] >> Symbol("varDefs")
  val functions: Lens[CompilerContext, FunctionTypes]  = lens[CompilerContext] >> Symbol("functionDefs")
}
