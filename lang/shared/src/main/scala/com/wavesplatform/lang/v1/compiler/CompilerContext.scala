package com.wavesplatform.lang.v1.compiler

import cats.Monoid
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.CompilerContext._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.ctx.{BaseFunction, FunctionTypeSignature}
import com.wavesplatform.lang.v1.parser.Expressions.Pos
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import shapeless._

case class CompilerContext(
    predefTypes: Map[String, FINAL],
    varDefs: VariableTypes,
    functionDefs: FunctionTypes,
    tmpArgsIdx: Int = 0,
    arbitraryDeclarations: Boolean = false
) {
  private lazy val allFuncDefs: FunctionTypes =
    predefTypes.collect {
      case (_, CASETYPEREF(typeName, fields, false)) =>
        typeName ->
          FunctionInfo(AnyPos, List(FunctionTypeSignature(CASETYPEREF(typeName, fields), fields, FunctionHeader.User(typeName))))
    } ++ functionDefs

  private def resolveFunction(name: String): FunctionInfo =
    if (arbitraryDeclarations) {
      val primitives = List(LONG, BYTESTR, BOOLEAN, STRING)
      val maybeAllTypes = UNION(
        UNION(primitives),
        UNION.create(predefTypes.values.toSeq),
        LIST(UNION(primitives))
      )
      def signature(name: String, i: Int) = {
        FunctionTypeSignature(maybeAllTypes, Seq.fill(i)(("arg", maybeAllTypes)), FunctionHeader.User(name))
      }
      allFuncDefs
        .withDefault(name => FunctionInfo(AnyPos, (0 to 22).map(i => signature(name, i)).toList))
        .apply(name)
    } else {
      allFuncDefs.getOrElse(name, FunctionInfo(AnyPos, List.empty))
    }

  def functionTypeSignaturesByName(name: String): List[FunctionTypeSignature] =
    resolveFunction(name).fSigList

  def resolveVar(name: String): Option[VariableInfo] =
    if (arbitraryDeclarations) {
      Some(varDefs.getOrElse(name, VariableInfo(AnyPos, NOTHING)))
    } else {
      varDefs.get(name)
    }

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
