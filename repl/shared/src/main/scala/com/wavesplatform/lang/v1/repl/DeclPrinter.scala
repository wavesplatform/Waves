package com.wavesplatform.lang.v1.repl

import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.evaluator.ctx.FunctionTypeSignature

object DeclPrinter {
  def overloadFuncStr(name: String, signatures: List[FunctionTypeSignature]): String =
    signatures
      .map(funcStr(name, _))
      .mkString("\n")

  private val definedStr = "defined "

  def declaredFuncStr(name: String, f: FunctionTypeSignature): String =
    definedStr + funcStr(name, f)

  def declaredLetStr(name: String, t: FINAL): String =
    definedStr + letStr(name, t)

  def funcStr(name: String, f: FunctionTypeSignature): String = {
    val FunctionTypeSignature(result, params, _) = f
    val paramsStr = params
      .map { case (name, t) => s"${name.filterNot(internalVarPrefixes.contains)}: $t" }
      .mkString(", ")
    s"func $name($paramsStr): $result"
  }

  def letStr(name: String, t: FINAL): String =
    s"let $name: ${typeStrRec(t)}"

  def typeStr(t: FINAL): String =
    s"type ${typeStrRec(t)}"

  private def typeStrRec(t: FINAL): String =
    t.name + (
      if (t.fields.isEmpty) ""
      else t.fields
        .map { case (name, fieldType) => s"$name: ${typeStrRec(fieldType)}" }
        .mkString(" { ", ", ", " }")
    )
}
