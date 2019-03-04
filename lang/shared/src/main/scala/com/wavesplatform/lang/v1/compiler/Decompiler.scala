package com.wavesplatform.lang.v1.compiler

import cats.implicits._
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract.{CallableFunction, VerifierFunction}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import monix.eval.Coeval

object Decompiler {

  sealed trait Braces
  case object NoBraces     extends Braces
  case object PreferBraces extends Braces

  sealed trait FirstLinePolicy
  case object DontIndentFirstLine extends FirstLinePolicy
  case object IdentFirstLine extends FirstLinePolicy

  private[lang] def pure[A](a: A) = Coeval.evalOnce(a)

  private def out(in: String, ident: Int): String =
    Array.fill(4 * ident)(" ").mkString("") + in

  private def pureOut(in: String, ident: Int): Coeval[String] = pure(out(in, ident))

  private val NEWLINE = scala.util.Properties.lineSeparator

  private def decl(e: Coeval[DECLARATION], ctx: DecompilerContext): Coeval[String] =
    e flatMap {
      case Terms.FUNC(name, args, body) =>
        expr(pure(body), ctx, PreferBraces, DontIndentFirstLine).map(
          fb =>
            out("func " + name + " (" + args.mkString(",") + ") = ", ctx.ident) +
              out(fb + NEWLINE, ctx.ident))
      case Terms.LET(name, value) => expr(pure(value), ctx, PreferBraces, DontIndentFirstLine).map(e => out("let " + name + " = " + e, ctx.ident))
    }

  private[lang] def expr(e: Coeval[EXPR], ctx: DecompilerContext, braces: Braces, firstLinePolicy: FirstLinePolicy): Coeval[String] =
    e flatMap {
      case Terms.BLOCK(declPar, body) =>
        val braceThis = braces match {
          case NoBraces     => false
          case PreferBraces => true // body.isInstanceOf[BLOCK] || body.isInstanceOf[LET_BLOCK]
        }
        val identedBlockText = if (braceThis) ctx.inc() else ctx
        for {
          d <- decl(pure(declPar), identedBlockText)
          b <- expr(pure(body), identedBlockText, NoBraces, IdentFirstLine)
        } yield {
          if (braceThis)
            out("{" + NEWLINE, ident = 0) +
              out(d + NEWLINE, 0) +
              out(b + NEWLINE, 0) +
              out("}", ctx.inc().ident)
          else
            out(d + NEWLINE, 0) +
              out(b, 0)
        }
      case Terms.LET_BLOCK(let, exprPar) => expr(pure(Terms.BLOCK(let, exprPar)), ctx, braces, firstLinePolicy)

      case Terms.TRUE                 => pureOut("true", if(firstLinePolicy == DontIndentFirstLine) 0 else ctx.ident)
      case Terms.FALSE                => pureOut("false", if(firstLinePolicy == DontIndentFirstLine) 0 else ctx.ident)
      case Terms.CONST_BOOLEAN(b)     => pureOut(b.toString.toLowerCase(), if(firstLinePolicy == DontIndentFirstLine) 0 else ctx.ident)
      case Terms.CONST_LONG(t)        => pureOut(t.toLong.toString, if(firstLinePolicy == DontIndentFirstLine) 0 else ctx.ident)
      case Terms.CONST_STRING(s)      => pureOut('"' + s + '"', if(firstLinePolicy == DontIndentFirstLine) 0 else ctx.ident)
      case Terms.CONST_BYTESTR(bs)    => pureOut("base58'" + bs.base58 + "'", if(firstLinePolicy == DontIndentFirstLine) 0 else ctx.ident)
      case Terms.REF(ref)             => pureOut(ref, if(firstLinePolicy == DontIndentFirstLine) 0 else ctx.ident)
      case Terms.GETTER(getExpr, fld) => expr(pure(getExpr), ctx, PreferBraces, firstLinePolicy).map(a => a + "." + fld)
      case Terms.IF(cond, it, iff) =>

        for {
          c   <- expr(pure(cond), ctx, PreferBraces, DontIndentFirstLine)
          it  <- expr(pure(it), ctx.inc(), PreferBraces, DontIndentFirstLine)
          iff <- expr(pure(iff), ctx.inc(), PreferBraces, DontIndentFirstLine)
        } yield
          out("if (" + c + ")" + NEWLINE, if(firstLinePolicy == DontIndentFirstLine) 0 else ctx.ident ) +
            out("then " + it + NEWLINE, ctx.inc().ident) +
            out("else " + iff, ctx.inc().ident)
      case Terms.FUNCTION_CALL(func, args) =>
        val argsCoeval = args
          .map(a => expr(pure(a), ctx.zero(), PreferBraces, DontIndentFirstLine))
          .toVector
          .sequence

        func match {
          case FunctionHeader.Native(name) =>
            ctx.binaryOps.get(name) match {
              case Some(binOp) =>
                argsCoeval.map(as => out("(" + as.head + " " + binOp + " " + as.tail.head + ")", if(firstLinePolicy == DontIndentFirstLine) 0 else ctx.ident))
              case None =>
                argsCoeval.map(
                  as =>
                    out(ctx.opCodes.getOrElse(name, "Native<" + name + ">") + "(" + as.mkString(", ")
                          + ")",
                      if(firstLinePolicy == DontIndentFirstLine) 0 else ctx.ident))
            }

          case FunctionHeader.User(name) => argsCoeval.map(as => out(name + "(" + as.mkString(", ") + ")", if(firstLinePolicy == DontIndentFirstLine) 0 else ctx.ident))
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
    expr(pure(e0), ctx, NoBraces, IdentFirstLine).apply()

}
