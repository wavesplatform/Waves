package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._

object Decompiler {

  def apply(e: EXPR): String =
    e match {
      case Terms.TRUE => "true"
      case Terms.FALSE => "false"
      case Terms.CONST_BOOLEAN(b) => b.toString().toLowerCase()
      case Terms.IF(cond, it, iff) => "if (" + Decompiler(cond) + ") then " + Decompiler(it) + " else " + Decompiler(iff)
      case Terms.CONST_LONG(t) => t.toLong.toString()
      case Terms.CONST_STRING(s) => """"""" + s + """""""
      case x: Terms.ARR => ???
      case Terms.BLOCKV1(let, expr) => "let " + let.name + " = " + Decompiler(let.value) + "; " + Decompiler(expr)
      case Terms.BLOCKV2(decl, body) => Decompiler(decl) + "; " + Decompiler(body)
      case x: Terms.CONST_BYTEVECTOR => "'" + x.bs + "'"
      case x: Terms.CaseObj => ??? // never happens
      case Terms.FUNCTION_CALL(func, args) => func match {
        case FunctionHeader.Native(name) => "Native_" + name + "(" + args.map(Decompiler(_)).mkString(",") + ")"
        case FunctionHeader.User(name) => name + "(" + args.map(Decompiler(_)).mkString(",") + ")"
      }
      case Terms.REF(ref) => ref
    }

  def apply (e: DECLARATION): String =
    e match {
      case Terms.FUNC(name, args, body) => "func " + name + " (" +
        args.map((x => x.toString)).mkString(",") + ") = " + body
      case Terms.LET(name, value) => "let " + name + " = " + Decompiler(value)
    }

}
