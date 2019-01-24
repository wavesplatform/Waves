package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._

object Decompiler {

  def out (in :String, ident :Int):String =
    Array.fill( 4*ident )(" ").mkString("") + in

  def apply(e: EXPR, ident :Int): String =
    e match {
      case Terms.TRUE => out("true", ident)
      case Terms.FALSE => out("false", ident)
      case Terms.CONST_BOOLEAN(b) => out(b.toString().toLowerCase(), ident)
      case Terms.IF(cond, it, iff) =>
        out("{ if (\n"
          + out(Decompiler(cond, 1 + ident), 1 + ident) + "\n"
          + out(")\n", 2 + ident)
          + out("then <ident=" + ident + ">\n", ident)
          + Decompiler(it, 1 + ident) + "\n"
          + out("else\n", ident)
          + Decompiler(iff, 1 + ident) + "\n"
          + out("}\n", ident)
          , ident)
      case Terms.CONST_LONG(t) => out(t.toLong.toString(), ident)
      case Terms.CONST_STRING(s) => out('"' + s + '"', ident)
      case Terms.ARR(_) => ??? // newer happens
      case Terms.BLOCKV1(let, expr) => out("{ let " + let.name + " = " + Decompiler(let.value, 0) + "; " + Decompiler(expr, 0) + " }", ident)
      case Terms.BLOCKV2(decl, body) =>
        out("{\n"
          + Decompiler(decl, 1 + ident) + ";\n"
          + Decompiler(body, 1 + ident) + "\n"
          , 0) + out("}", ident)
      case Terms.CONST_BYTEVECTOR(bs) => out("'" + bs + "'", ident) // TODO: need test for bytevector
      case _: Terms.CaseObj => ??? // never happens
      case Terms.FUNCTION_CALL(func, args) => func match {
        case FunctionHeader.Native(name) => out("Native_" + name + "(" + args.map(Decompiler(_, 0)).mkString(",") + ")", ident)
        case FunctionHeader.User(name) => out(name + "(" + args.map(Decompiler(_, 0)).mkString(",") + ")", ident)
      }
      case Terms.REF(ref) => ref
    }

  def apply (e: DECLARATION, ident :Int): String =
    e match {
      case Terms.FUNC(name, args, body) => out("{ func " + name + " (" +
        args.map((x => x.toString)).mkString(",") + ") = " + body + " }", ident)
      case Terms.LET(name, value) => out("let " + name + " = " + Decompiler(value, 0 + ident), ident)
    }

}
