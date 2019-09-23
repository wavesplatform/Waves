package com.wavesplatform.lang.v1.repl

import com.wavesplatform.lang.v1.compiler.CompilerContext

import scala.collection.immutable.ListMap

case class StateView(ctx: CompilerContext) {
  lazy val totalCtx: String =
    declMap.values.mkString("\n")

  lazy val declMap: Map[String, String] =
     (ListMap() ++ funcs ++ values ++ types).withDefault(s => s"$s not found in context")

  private lazy val funcs: Map[String, String] =
    ctx.functionDefs
      .map { case (name, signatures) => (name, DeclPrinter.overloadFuncStr(name, signatures))}

  private lazy val values: Map[String, String] =
    ctx.varDefs
      .map { case (name, t) => (name, DeclPrinter.letStr(name, t)) }

  private lazy val types: Map[String, String] =
    ctx.predefTypes.mapValues(DeclPrinter.typeStr)
}
