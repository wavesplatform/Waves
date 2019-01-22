package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.parser.Expressions

object Decompiler {

  def apply(e :EXPR): String =
    e match {
      case x: Terms.CONST_BOOLEAN => x.toString().toLowerCase()
      case Terms.IF(cond, it, iff) => "if (" + Decompiler(cond) + ") then " + Decompiler(it) + " else " + Decompiler(iff)
        // отсутствие ветви else?
      case Terms.CONST_LONG(t) => t.toLong.toString()
      case Terms.CONST_STRING(s) => """"""" + s + """""""
      case x: Terms.ARR => ???
      case Terms.BLOCKV1(let,expr) => "let " + let.name + " = " + Decompiler(let.value) + "; " + Decompiler(expr)
      case x: Terms.BLOCKV2 => "v2let " + x.dec + " = " + Decompiler(x.body) // test it
      case x: Terms.CONST_BYTEVECTOR => "'" + x.bs + "'"
      case x: Terms.CaseObj => ??? // never happens
      case x: Terms.DECLARATION => "TODO: declaration?"
      case x: Terms.EVALUATED => "TODO: evaluated"
      //case x: Terms.FUNC => "def" + x.name + "(" + x.args map { case (arg, typearg) => arg + " : " + typearg } + x.body
      case Terms.FUNCTION_CALL(func, args) => func + "(" + args.foreach((arg)=>arg.toString()) + ")"
      case Terms.REF(ref) => ref
    }
}
