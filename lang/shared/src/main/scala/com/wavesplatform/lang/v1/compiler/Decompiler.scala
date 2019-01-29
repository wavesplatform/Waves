package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._

// TODO: сделать ручку
// TODO: переполнение стека
// TODO: декомпилить контракт, который может быть списком функций

object Decompiler {

  def out (in :String, ident :Int):String =
    Array.fill( 4*ident )(" ").mkString("") + in

  def decl (e: DECLARATION, ident :Int, opCodes :Map[Int,String]): String =
    e match {
      case Terms.FUNC(name, args, body) => out("{ func " + name + " (" +
        args.map(_.toString).mkString(",") + ") = " + body + " }", ident)
      case Terms.LET(name, value) => out("let " + name + " =\n" + expr(value, 1 + ident, opCodes), ident)
    }

  def expr(e: EXPR, ident :Int, OpCodes :Map[Int,String]): String =
    e match {
      case Terms.TRUE => out("true", ident)
      case Terms.FALSE => out("false", ident)
      case Terms.CONST_BOOLEAN(b) => out(b.toString.toLowerCase(), ident)
      case Terms.IF(cond, it, iff) =>
          out("{ if (\n", ident) +
          expr(cond, 1 + ident, OpCodes) + "\n" +
          out(")\n", 1 + ident) +
          out("then\n", ident) +
          expr(it, 1 + ident, OpCodes) + "\n" +
          out("else\n", ident) +
          expr(iff, 1 + ident, OpCodes) + "\n" +
          out("}", ident)
      case Terms.CONST_LONG(t) => out(t.toLong.toString, ident)
      case Terms.CONST_STRING(s) => out('"' + s + '"', ident)
      case Terms.LET_BLOCK(let, exprPar) => out("{ let " + let.name + " = " +
        expr(let.value, 0, OpCodes) + "; " + expr(exprPar, 0, OpCodes) + " }", ident)
      case Terms.BLOCK(declPar, body) =>
        out("{\n", ident) +
        decl(declPar, 1 + ident, OpCodes) + ";\n" +
        expr(body, 1 + ident, OpCodes) + "\n" +
        out("}", ident)
      case Terms.CONST_BYTESTR(bs) => out("'" + bs + "'", ident) // TODO: need test for bytestr
      case Terms.FUNCTION_CALL(func, args) => func match {
        case FunctionHeader.Native(name) => out(
          OpCodes.getOrElse(name, "<Native_" + name + ">") +
          "(" + args.map(expr(_, 0, OpCodes)).mkString(",") + ")", ident)
        case FunctionHeader.User(name) => out(name + "(" + args.map(expr(_, 0, OpCodes)).mkString(",") + ")", ident)
      }
      case Terms.REF(ref) => out(ref, ident)
      // TODO: need case and test for GETTER(_,_)

      case Terms.ARR(_) => ??? // never happens
      case _: Terms.CaseObj => ??? // never happens

    }

  def apply(e0 :EXPR, opCodes:scala.collection.immutable.Map[Int,String]): String =
    expr(e0, 0, opCodes)

  def apply(e0 :DECLARATION, opCodes:scala.collection.immutable.Map[Int,String]): String =
    decl(e0, 0, opCodes)

}
