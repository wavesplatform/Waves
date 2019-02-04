package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract.ContractFunction
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._

object Decompiler {

  private def out (in :String, ident :Int):String =
    Array.fill( 4*ident )(" ").mkString("") + in

  private def decl (e: DECLARATION, ident :Int, opcodes :Map[Short,String]): String =
    e match {
      case Terms.FUNC(name, args, body) =>
        out("func " + name + " (" + args.map(_.toString).mkString(","), ident) + ") = {\n" +
        out(Decompiler(body, opcodes) + "\n", 1 + ident) +
        out("}", ident)
      case Terms.LET(name, value) => out("let " + name + " =\n" + expr(value, 1 + ident, opcodes), ident)
    }

  private def expr(e: EXPR, ident :Int, opcodes :Map[Short,String]): String =
    e match {
      case Terms.TRUE => out("true", ident)
      case Terms.FALSE => out("false", ident)
      case Terms.CONST_BOOLEAN(b) => out(b.toString.toLowerCase(), ident)
      case Terms.IF(cond, it, iff) =>
          out("{ if (\n", ident) +
          expr(cond, 1 + ident, opcodes) + "\n" +
          out(")\n", 1 + ident) +
          out("then\n", ident) +
          expr(it, 1 + ident, opcodes) + "\n" +
          out("else\n", ident) +
          expr(iff, 1 + ident, opcodes) + "\n" +
          out("}", ident)
      case Terms.CONST_LONG(t) => out(t.toLong.toString, ident)
      case Terms.CONST_STRING(s) => out('"' + s + '"', ident)
      case Terms.LET_BLOCK(let, exprPar) => out("{ let " + let.name + " = " +
        expr(let.value, 0, opcodes) + "; " + expr(exprPar, 0, opcodes) + " }", ident)
      case Terms.BLOCK(declPar, body) =>
        out("{\n", ident) +
        decl(declPar, 1 + ident, opcodes) + ";\n" +
        expr(body, 1 + ident, opcodes) + "\n" +
        out("}", ident)
      case Terms.CONST_BYTESTR(bs) => out("base58'" + bs.base58 + "'", ident)
      case Terms.FUNCTION_CALL(func, args) => func match {
        case FunctionHeader.Native(name) => out(
          opcodes.getOrElse(name, "<Native_" + name + ">") +
          "(" + args.map(expr(_, 0, opcodes)).mkString(",") + ")", ident)
        case FunctionHeader.User(name) => out(name + "(" + args.map(expr(_, 0, opcodes)).mkString(",") + ")", ident)
      }
      case Terms.REF(ref) => out(ref, ident)
      case Terms.GETTER(get_expr, fld) => out(expr(get_expr, ident, opcodes) + "." + fld, ident)
      case Terms.ARR(_) => ??? // never happens
      case _: Terms.CaseObj => ??? // never happens
    }

  def apply(e :Contract, opcodes:Map[Short,String]): String = {
    e match {
      case Contract(dec, cfs, vf) =>
        dec.map(expr => expr.toString).mkString("") +
        cfs.map(expr => expr match {
          case ContractFunction(annotation, u) =>
            out("@Callable(" + annotation.invocationArgName + ")\n", 0) +
            Decompiler.decl(u, 0, opcodes)
          case _ => ???
        }).mkString("") +
        vf.getOrElse("")
    }
  }

  def apply(e0 :EXPR, opcodes:Map[Short,String]): String =
    expr(e0, 0, opcodes)

  def apply(e0 :DECLARATION, opcodes:Map[Short,String]): String =
    decl(e0, 0, opcodes)

}
