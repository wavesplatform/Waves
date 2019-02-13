package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract.{CallableFunction, VerifierFunction}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._

object Decompiler {

  case class Delay(seq: Seq[Either[String, Function[Unit, Delay]]])

  def show(d: Delay): String = {
    var w = d.seq
    val s = new StringBuffer()
    while (w.nonEmpty) {
      w.head match {
        case Left(o) =>
          s.append(o)
          w = w.tail
        case Right(f) =>
          w = f(()).seq ++ w.tail
      }
    }
    s.toString
  }

  private def out(in: String, ident: Int): String =
    Array.fill(4 * ident)(" ").mkString("") + in

  private def decl(e: DECLARATION, ident: Int, ctx: DecompilerContext): String =
    e match {
      case Terms.FUNC(name, args, body) =>
        out("func " + name + " (" + args.map(_.toString).mkString(","), ident) + ") = {\n" +
          show(expr(body, 1 + ident, ctx)) + "\n" +
          out("}\n", ident)
      case Terms.LET(name, value) =>
        out("let " + name + " =\n", 0 + ident) +
          show(expr(value, 1 + ident, ctx))
    }

  private def argso(args: List[Terms.EXPR], ctx: DecompilerContext): Seq[Either[String, Function[Unit, Delay]]] = {
    args.foldLeft(Seq[Either[String, Function[Unit, Delay]]]()) { (alfa, beta) =>
      val gamma = Right((_: Unit) => expr(beta, 0, ctx))
      if (alfa.isEmpty) Seq(gamma)
      else alfa ++ Seq(Left(", "), gamma)
    }
  }

  private def expr(e: EXPR, ident: Int, ctx: DecompilerContext): Delay =
    e match {
      case Terms.TRUE             => Delay(Seq(Left(out("true", ident))))
      case Terms.FALSE            => Delay(Seq(Left(out("false", ident))))
      case Terms.CONST_BOOLEAN(b) => Delay(Seq(Left(out(b.toString.toLowerCase(), ident))))
      case Terms.IF(cond, it, iff) =>
        Delay(
          Seq(
            Left(out("{\n", 0 + ident)),
            Left(out("if (\n", 1 + ident)),
            Right(_ => expr(cond, 2 + ident, ctx)),
            Left("\n"),
            Left(out(")\n", 1 + ident)),
            Left(out("then\n", 1 + ident)),
            Right(_ => expr(it, 2 + ident, ctx)),
            Left("\n"),
            Left(out("else\n", 1 + ident)),
            Right(_ => expr(iff, 2 + ident, ctx)),
            Left("\n"),
            Left(out("}", 0 + ident))
          ))
      case Terms.CONST_LONG(t)   => Delay(Seq(Left(out(t.toLong.toString, ident))))
      case Terms.CONST_STRING(s) => Delay(Seq(Left(out('"' + s + '"', ident))))
      case Terms.LET_BLOCK(let, exprPar) =>
        Delay(
          Seq(
            Left(out("{ let " + let.name + " = ", ident)),
            Right(_ => expr(let.value, 0, ctx)),
            Left(out("; ", 0)),
            Right(_ => expr(exprPar, 0, ctx)),
            Left(out(" }", 0))
          ))
      case Terms.BLOCK(declPar, body) =>
        Delay(
          Seq(
            Left(out("{\n", ident) + decl(declPar, 1 + ident, ctx) + ";\n"),
            Right(_ => expr(body, 1 + ident, ctx)),
            Left(out("\n", 0)),
            Left(out("}", ident))
          ))
      case Terms.CONST_BYTESTR(bs) => Delay(Seq(Left(out("base58'" + bs.base58 + "'", ident))))
      case Terms.FUNCTION_CALL(func, args) =>
        func match {
          case FunctionHeader.User(name) => Delay(Seq(Left(out(name + "(", ident))) ++ argso(args, ctx) ++ Seq(Left(")")))
          case FunctionHeader.Native(name) =>
            val binOp: Option[String] = ctx.binaryOps.get(name)
            binOp match {
              case Some(binOp) =>
                Delay(Seq(
                  Left(out("(", ident)),
                  Right(_ => expr(args.head, 0, ctx)),
                  Left(out(" " + binOp + " ", 0)),
                  Right(_ => expr(args.tail.head, 0, ctx)),
                  Left(out(")", 0))))
              case None =>
                val opCode = ctx.opCodes.get(name)
                opCode match {
                  case None =>
                    Delay(
                      Seq(Left(out("Decompile Error: Wrong opcode: <" + name + "> with args:", ident))) ++
                        Seq(Left(out("(", 0))) ++ argso(args, ctx) ++ Seq(Left(")")))
                  case Some(opCode) =>
                    Delay(Seq(Left(out(opCode.toString + "(", ident))) ++ argso(args, ctx) ++ Seq(Left(")")))
                }
            }
        }
      case Terms.REF(ref) => Delay(Seq(Left(out(ref, ident))))
      case Terms.GETTER(get_expr, fld) =>
        Delay(
          Seq(
            Left(out("", 0)),
            Right(_ => expr(get_expr, ident, ctx)),
            Left(out("." + fld, 0))
          ))
      case Terms.ARR(_)     => ??? // never happens
      case _: Terms.CaseObj => ??? // never happens
    }

  def apply(e: Contract, ctx: DecompilerContext): String = {
    e match {
      case Contract(dec, cfs, vf) =>
        dec.map(expr => decl(expr, 0, ctx)).mkString("\n\n") +
          cfs
            .map {
              case CallableFunction(annotation, u) =>
                out("\n@Callable(" + annotation.invocationArgName + ")\n", 0) +
                  Decompiler.decl(u, 0, ctx)
            }
            .mkString("\n") +
          (vf match {
            case Some(VerifierFunction(annotation, u)) =>
              out("\n@Verifier(" + annotation.txArgName + ")\n", 0) +
                Decompiler.decl(u, 0, ctx)
            case None => ""
          })
    }
  }

  def apply(e0: EXPR, ctx: DecompilerContext): String =
    show(expr(e0, 0, ctx))

  def apply(e0: DECLARATION, ctx: DecompilerContext): String =
    decl(e0, 0, ctx)

}
