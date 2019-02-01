package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.parser.Expressions.PART.VALID
import com.wavesplatform.lang.v1.parser.Expressions.{ANNOTATEDFUNC, ANNOTATION, BINARY_OP, CONTRACT, MATCH, MATCH_CASE}
import com.wavesplatform.lang.v1.parser.Expressions.Pos.{AnyPos, RealPos}

object Decompiler {

  def out (in :String, ident :Int):String =
    Array.fill( 4*ident )(" ").mkString("") + in

  def decl (e: DECLARATION, ident :Int, opcodes :Map[Short,String]): String =
    e match {
      case Terms.FUNC(name, args, body) =>
        out("func " + name + " (" + args.map(_.toString).mkString(","), ident) + ") = {\n" +
        out(body + "\n", 1 + ident) +
        out("}", ident)
      case Terms.LET(name, value) => out("let " + name + " =\n" + expr(value, 1 + ident, opcodes), ident)
    }

  def expr(e: EXPR, ident :Int, opcodes :Map[Short,String]): String =
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
      case Terms.CONST_BYTESTR(bs) => out("'" + bs + "'", ident)
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

  def apply(e :CONTRACT, opcodes:Map[Short,String]): String = {
    e match {
      case CONTRACT(pos, decl, fs) => fs.map(expr => Decompiler.apply(expr, opcodes)).mkString("\n")
    }
  }

  def apply(e0 :ANNOTATEDFUNC, opcodes:Map[Short,String]): String = {
    e0 match {
      case ANNOTATEDFUNC(position, anns, f) => Decompiler(anns, opcodes) + Decompiler(f :com.wavesplatform.lang.v1.parser.Expressions.FUNC, opcodes)
    }
  }
  
  def apply(e0: Seq[ANNOTATION], opCodes:Map[Short,String]): String = {
    e0.mkString(",-todo-")
  }
  
  def apply(e0 :com.wavesplatform.lang.v1.parser.Expressions.FUNC, opcodes:Map[Short,String]): String = {
    Decompiler(e0, opcodes)
  }

  def apply(e0 :EXPR, opcodes:Map[Short,String]): String =
    expr(e0, 0, opcodes)

  def apply(e0 :DECLARATION, opcodes:Map[Short,String]): String =
    decl(e0, 0, opcodes)

}
