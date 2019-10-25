package com.wavesplatform.lang.v1.compiler

import cats.implicits._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableFunction, VerifierFunction}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{ExtractedFuncPrefix, ExtractedFuncPostfix}
import monix.eval.Coeval

import scala.util.Try

object Decompiler {

  sealed trait BlockBraces
  case object NoBraces             extends BlockBraces
  case object BracesWhenNeccessary extends BlockBraces

  sealed trait FirstLinePolicy
  case object DontIndentFirstLine extends FirstLinePolicy
  case object IdentFirstLine      extends FirstLinePolicy

  private[lang] def pure[A](a: A) = Coeval.evalOnce(a)

  private def out(in: String, ident: Int): String =
    Array.fill(4 * ident)(" ").mkString("") + in

  private def pureOut(in: String, ident: Int): Coeval[String] = pure(out(in, ident))

  private val NEWLINE = "\n"

  private def decl(e: Coeval[DECLARATION], ctx: DecompilerContext): Coeval[String] =
    e flatMap {
      case Terms.FUNC(name, args, body) =>
        expr(pure(body), ctx, BracesWhenNeccessary, DontIndentFirstLine).map(
          fb =>
            out("func " + name + " (" + args.mkString(",") + ") = ", ctx.ident) +
              out(fb + NEWLINE, ctx.ident))
      case Terms.LET(name, value) =>
        expr(pure(value), ctx, BracesWhenNeccessary, DontIndentFirstLine).map(e => out("let " + name + " = " + e, ctx.ident))
    }

  private def extrTypes(Name: String, e: EXPR): Coeval[Option[List[String]]] = {
    e match {
      case FUNCTION_CALL(FunctionHeader.Native(1), List(REF(Name), CONST_STRING(typeName))) => pure(Some(List(typeName)))
      case IF(FUNCTION_CALL(FunctionHeader.Native(1), List(REF(Name), CONST_STRING(typeName))), TRUE, t) =>
        extrTypes(Name, t) map (_.map(tl => typeName :: tl))
      case _ => pure(None)
    }
  }

  object ANY_LET {
    def unapply(e: EXPR): Option[(String, EXPR, EXPR)] = {
      e match {
        case LET_BLOCK(LET(name, v), body) => Some((name, v, body))
        case BLOCK(LET(name, v), body) => Some((name, v, body))
        case _ => None
      }
    }
  }

  private def caseExpr(Name: String, e: EXPR, ctx: DecompilerContext): Coeval[(String, Option[EXPR])] = {
    e match {
      case IF(
            tc,
            ANY_LET(name, REF(Name), cExpr),
            tailExpr) => extrTypes(Name, tc) flatMap {
        case None => expr(pure(e), ctx.incrementIdent(), NoBraces, IdentFirstLine) map { e =>
           ("case _ => " ++ NEWLINE ++ e, None)
        }
        case Some(tl) => expr(pure(cExpr), ctx.incrementIdent(), NoBraces, IdentFirstLine) map { e =>
           ("case " ++ name ++ ": " ++ tl.mkString("|") ++ " => " ++ NEWLINE ++ e, Some(tailExpr))
        }
      }
      case IF(
            tc,
            cExpr,
            tailExpr) => extrTypes(Name, tc) flatMap {
        case None => expr(pure(e), ctx.incrementIdent(), NoBraces, IdentFirstLine) map { e =>
           ("case _ => " ++ NEWLINE ++ e, None)
        }
        case Some(tl) => expr(pure(cExpr), ctx.incrementIdent(), NoBraces, IdentFirstLine) map { e =>
           ("case _: " ++ tl.mkString("|") ++ " => " ++ NEWLINE ++ e, Some(tailExpr))
        }
      }
      case ANY_LET(name, REF(Name), e) => expr(pure(e), ctx.incrementIdent(), NoBraces, IdentFirstLine) map { e =>
        ("case " ++ name ++ " => " ++ NEWLINE ++ e, None)
      }
      case _ => expr(pure(e), ctx.incrementIdent(), NoBraces, IdentFirstLine) map { e =>
        ("case _ => " ++ NEWLINE ++ e, None)
      }
    }
  }

  private def matchBlock(name: String, body: Coeval[EXPR], ctx: DecompilerContext): Coeval[String] = {
    for {
      e <- body
      p <- caseExpr(name, e, ctx)
      c = p._1 ++ NEWLINE
      t <- p._2.fold(pure(Option.empty[String]))(e => matchBlock(name, pure(e), ctx).map(Some.apply))
    } yield {
      t.fold(out(c, ctx.ident)) { t => out(c, ctx.ident) ++ t }
    }
  }

  val MatchRef = """(\$match\d*)""".r

  private[lang] def expr(e: Coeval[EXPR], ctx: DecompilerContext, braces: BlockBraces, firstLinePolicy: FirstLinePolicy): Coeval[String] = {
    def argsStr(args: List[EXPR]) = args.map(argStr).toVector.sequence
    def listStr(elems: List[EXPR]) = argsStr(elems).map(_.mkString("[", ", ", "]"))
    def argStr(elem: EXPR) = expr(pure(elem), ctx, BracesWhenNeccessary, DontIndentFirstLine)

    val i = if (firstLinePolicy == DontIndentFirstLine /*braces == BracesWhenNeccessary*/) 0 else ctx.ident

    e flatMap {
      case Terms.BLOCK(Terms.LET(MatchRef(name), e), body) => matchBlock(name, pure(body), ctx.incrementIdent()) flatMap { b =>
        expr(pure(e), ctx.incrementIdent(), NoBraces, DontIndentFirstLine) map { ex =>
          out("match " ++ ex ++ " {" ++ NEWLINE, ctx.ident) ++
          out( b, 0) ++
          out("}", ctx.ident)
        }
      }
      case Terms.BLOCK(declPar, body) =>
        val braceThis = braces match {
          case NoBraces             => false
          case BracesWhenNeccessary => true
        }
        val modifiedCtx = if (braceThis) ctx.incrementIdent() else ctx
        for {
          d <- decl(pure(declPar), modifiedCtx)
          b <- expr(pure(body), modifiedCtx, NoBraces, IdentFirstLine)
        } yield {
          if (braceThis)
            out("{" + NEWLINE, ident = 0) +
              out(d + NEWLINE, 0) +
              out(b + NEWLINE, 0) +
              out("}", ctx.ident + 1)
          else
            out(d + NEWLINE, 0) +
              out(b, 0)
        }
      case Terms.LET_BLOCK(let, exprPar) => expr(pure(Terms.BLOCK(let, exprPar)), ctx, braces, firstLinePolicy)
      case Terms.TRUE                    => pureOut("true", i)
      case Terms.FALSE                   => pureOut("false", i)
      case Terms.CONST_BOOLEAN(b)        => pureOut(b.toString.toLowerCase(), i)
      case Terms.CONST_LONG(t)           => pureOut(t.toLong.toString, i)
      case Terms.CONST_STRING(s)         => pureOut('"' + s + '"', i)
      case Terms.CONST_BYTESTR(bs)       => pureOut(if(bs.size <= 128) { "base58'" ++ bs.toString ++ "'" } else { "base64'" ++ bs.base64Raw ++ "'" }, i)
      case Terms.REF(ref)                => pureOut(ref, i)
      case Terms.GETTER(getExpr, fld)    => expr(pure(getExpr), ctx, BracesWhenNeccessary, firstLinePolicy).map(a => a + "." + fld)
      case Terms.IF(cond, it, iff) =>
        for {
          c   <- expr(pure(cond), ctx, BracesWhenNeccessary, DontIndentFirstLine)
          it  <- expr(pure(it), ctx.incrementIdent(), BracesWhenNeccessary, DontIndentFirstLine)
          iff <- expr(pure(iff), ctx.incrementIdent(), BracesWhenNeccessary, DontIndentFirstLine)
        } yield
          out("if (" + c + ")" + NEWLINE, i) +
            out("then " + it + NEWLINE, ctx.ident + 1) +
            out("else " + iff, ctx.ident + 1)
      case FUNCTION_CALL(`cons`, args) =>
        collectListArgs(args) match {
          case (elems, None)               => listStr(elems)
          case (List(elem), Some(listVar)) => argStr(elem).map(v => s"$v :: $listVar")
          case (elems, Some(listVar))      => listStr(elems).map(v => s"$v :: $listVar")
        }
      case FUNCTION_CALL(`listElem`, List(list, index)) =>
        for (l <- argStr(list); i <- argStr(index)) yield s"$l[$i]"
      case Terms.FUNCTION_CALL(func, args) =>
        val argsCoeval = argsStr(args)
        func match {
          case FunctionHeader.Native(id) if ctx.binaryOps.contains(id) =>
            argsCoeval.map(as => out(s"(${as(0)} ${ctx.binaryOps(id)} ${as(1)})", i))

          case header =>
            val name = extractFunctionName(ctx, header)
            argsCoeval.map(as => out(s"$name(${as.mkString(", ")})", i))
        }
      case _: Terms.ARR     => ??? // never happens
      case _: Terms.CaseObj => ??? // never happens
    }
  }

  private val extractedFuncR = s"$ExtractedFuncPrefix(\\w+)\\((.+)\\)".r

  private def extractFunctionName(ctx: DecompilerContext, header: FunctionHeader) =
    header match {
      case inner@User(_, name) =>
        extractedFuncR.findFirstMatchIn(name)
          .flatMap(m =>
            (m.group(1), m.group(2)) match {
              case ("User", name) => Some(User(name))
              case ("Native", id) => Try(id.toShort).toOption.map(Native)
              case _              => None
            }
          )
          .map(getFunctionName(ctx, _) + ExtractedFuncPostfix)
          .getOrElse(getFunctionName(ctx, inner))

      case h => getFunctionName(ctx, h)
  }

  private def getFunctionName(ctx: DecompilerContext, header: FunctionHeader) =
    header match {
      case Native(id)    => ctx.opCodes.getOrElse(id, s"Native<$id>")
      case User(_, name) => name
    }

  private val nil  = REF("nil")
  private val cons = Native(FunctionIds.CREATE_LIST)
  private val listElem = Native(FunctionIds.GET_LIST)

  private def collectListArgs(args: List[EXPR]): (List[EXPR], Option[String]) = {
    def flattenRec(args: List[EXPR]): List[EXPR] = args match {
      case a :: FUNCTION_CALL(`cons`, nextArgs) :: Nil => a :: flattenRec(nextArgs)
      case l => l
    }
    flattenRec(args) match {
      case a :+ `nil`        => (a, None)
      case a :+ REF(listVar) => (a, Some(listVar))
      case l                 => (l, None)
    }
  }

  def apply(e: DApp, ctx: DecompilerContext): String = {

    def intersperse(s: Seq[Coeval[String]]): Coeval[String] = s.toVector.sequence.map(v => v.mkString(NEWLINE + NEWLINE))

    import e._

    val decls: Seq[Coeval[String]] = decs.map(expr => decl(pure(expr), ctx))
    val callables: Seq[Coeval[String]] = callableFuncs
      .map {
        case CallableFunction(annotation, u) =>
          Decompiler.decl(pure(u), ctx).map(out(NEWLINE + "@Callable(" + annotation.invocationArgName + ")" + NEWLINE, 0) + _)
      }

    val verifier: Seq[Coeval[String]] = verifierFuncOpt.map {
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
