package com.wavesplatform.lang.v1.compiler

import cats.implicits._
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract.{CallableFunction, VerifierFunction}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import monix.eval.Coeval

object Decompiler {
  private[lang] def pure[A](a: A) = Coeval.evalOnce(a)

  private def out(in: String, ident: Int): String =
    Array.fill(4 * ident)(" ").mkString("") + in

  private def pureOut(in: String, ident: Int): Coeval[String] = pure(out(in, ident))

  private val NEWLINE = scala.util.Properties.lineSeparator

  private def decl(e: Coeval[DECLARATION], ctx: DecompilerContext): Coeval[String] =
    e flatMap {
      case Terms.FUNC(name, args, body) =>
        expr(pure(body), ctx.inc()).map(
          fb =>
            out("func " + name + " (" + args.mkString(",") + ") = {" + NEWLINE, ctx.ident) +
              out(fb + NEWLINE, ctx.ident) +
              out("}", ctx.ident))
      case Terms.LET(name, value) => expr(pure(value), ctx.inc()).map(e => out("let " + name + " =" + NEWLINE + e, ctx.ident))
    }

  private[lang] def expr(e: Coeval[EXPR], ctx: DecompilerContext): Coeval[String] =
    e flatMap {
      case Terms.TRUE                 => pureOut("true", ctx.ident)
      case Terms.FALSE                => pureOut("false", ctx.ident)
      case Terms.CONST_BOOLEAN(b)     => pureOut(b.toString.toLowerCase(), ctx.ident)
      case Terms.CONST_LONG(t)        => pureOut(t.toLong.toString, ctx.ident)
      case Terms.CONST_STRING(s)      => pureOut('"' + s + '"', ctx.ident)
      case Terms.CONST_BYTESTR(bs)    => pureOut("base58'" + bs.base58 + "'", ctx.ident)
      case Terms.REF(ref)             => pureOut(ref, ctx.ident)
      case Terms.GETTER(getExpr, fld) => expr(pure(getExpr), ctx).map(a => a + "." + fld)
      case Terms.IF(cond, it, iff) =>
        val ident2 = ctx.inc().inc()
        for {
          c   <- expr(pure(cond), ident2)
          it  <- expr(pure(it), ident2)
          iff <- expr(pure(iff), ident2)
        } yield
          out("{" + NEWLINE, ctx.ident) +
            out("if (" + NEWLINE, ctx.inc().ident) +
            out(c + NEWLINE, 0) +
            out(")" + NEWLINE, ctx.inc().ident) +
            out("then" + NEWLINE, ctx.inc().ident) +
            out(it + NEWLINE, 0) +
            out("else" + NEWLINE, ctx.inc().ident) +
            out(iff + NEWLINE, 0) +
            out("}", ctx.ident)
      case Terms.BLOCK(declPar, body) =>
        for {
          d <- decl(pure(declPar), ctx.inc())
          b <- expr(pure(body), ctx.inc())
        } yield
          out("{" + NEWLINE, ctx.ident) +
            out(d + ";" + NEWLINE, 0) +
            out(b + NEWLINE, 0) +
            out("}", ctx.ident)

      case Terms.LET_BLOCK(let, exprPar) => expr(pure(Terms.BLOCK(let, exprPar)), ctx)
      case Terms.FUNCTION_CALL(func, args) =>
        val argsCoeval = args
          .map(a => expr(pure(a), ctx.zero()))
          .toVector
          .sequence

        func match {
          case FunctionHeader.Native(name) =>
            ctx.binaryOps.get(name) match {
              case Some(binOp) =>
                argsCoeval.map(as => out("(" + as.head + " " + binOp + " " + as.tail.head + ")", ctx.ident))
              case None =>
                argsCoeval.map(
                  as =>
                    out(ctx.opCodes.getOrElse(name, "Native<" + name + ">") + "(" + as.mkString(", ")
                          + ")",
                        ctx.ident))
            }

          case FunctionHeader.User(name) => argsCoeval.map(as => out(name + "(" + as.mkString(", ") + ")", ctx.ident))
        }
      case _: Terms.ARR     => ??? // never happens
      case _: Terms.CaseObj => ??? // never happens
    }

  def apply(e: Contract, ctx: DecompilerContext): String = {

    def intersperse(s: Seq[Coeval[String]]): Coeval[String] = s.toVector.sequence.map(v => v.mkString(NEWLINE + NEWLINE))

    import e._

    val decls: Seq[Coeval[String]] = dec.map(expr => decl(pure(expr), ctx))
    val callables: Seq[Coeval[String]] = cfs
      .map {
        case CallableFunction(annotation, u) =>
          Decompiler.decl(pure(u), ctx).map(out(NEWLINE + "@Callable(" + annotation.invocationArgName + ")" + NEWLINE, 0) + _)
      }

    val verifier: Seq[Coeval[String]] = vf.map {
      case VerifierFunction(annotation, u) =>
        Decompiler.decl(pure(u), ctx).map(out(NEWLINE + "@Verifier(" + annotation.invocationArgName + ")" + NEWLINE, 0) + _)
    }.toSeq

    val result = for {
      d <- intersperse(decls)
      c <- intersperse(callables)
      v <- intersperse(verifier)
    } yield d + NEWLINE + c + NEWLINE + v

    result()
  }

  def apply(e0: EXPR, ctx: DecompilerContext): String =
    expr(pure(e0), ctx).apply()

}
