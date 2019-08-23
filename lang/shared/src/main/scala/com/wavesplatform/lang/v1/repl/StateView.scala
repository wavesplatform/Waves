package com.wavesplatform.lang.v1.repl

import com.wavesplatform.lang.v1.compiler.CompilerContext
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, FunctionTypeSignature}

import scala.collection.immutable.ListMap

case class StateView(ctx: CompilerContext) {
  lazy val totalCtx: String =
    declMap.values.mkString("\n")

  lazy val declMap: Map[String, String] =
     (ListMap() ++ funcs ++ values ++ types).withDefault(s => s"$s not found in context")

  private lazy val funcs: Map[String, String] =
    ctx.functionDefs
      .map { case (name, signatures) => (name, funcsStr(name, signatures))}


  private def funcsStr(name: String, signatures: List[FunctionTypeSignature]) =
    signatures
      .map { case FunctionTypeSignature(result, params, _) =>
        val paramsStr = params
          .map { case (name, t) => s"${name.filterNot(Set('@', '$') contains)}: $t" }
          .mkString(", ")
        s"func $name($paramsStr): $result"
      }
      .mkString("\n")

  private lazy val values: Map[String, String] =
    ctx.varDefs
      .map { case (name, t) => (name, s"let $name: ${typeStrRec(t)}") }

  private lazy val types: Map[String, String] =
    ctx.predefTypes
      .mapValues(t => s"type ${typeStrRec(t)}")

  private def typeStrRec(t: FINAL): String =
    t.name + (
      if (t.fields.isEmpty) ""
      else t.fields
            .map { case (name, fieldType) => s"$name: ${typeStrRec(fieldType)}" }
            .mkString(" { ", ", ", " }")
    )
}
