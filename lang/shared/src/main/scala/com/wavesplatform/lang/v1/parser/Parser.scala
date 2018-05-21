package com.wavesplatform.lang.v1.parser

import Expressions._
import BinaryOperation._
import UnaryOperation._
import fastparse.{WhitespaceApi, core}
import scodec.bits.ByteVector

object Parser {

  private val Global = com.wavesplatform.lang.hacks.Global // Hack for IDEA

  private val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(" ", "\t", "\r", "\n").rep)
  }

  import White._
  import fastparse.noApi._
  private val Base58Chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  private val keywords    = Set("let", "base58", "true", "false", "if", "then", "else", "match", "case")

  private val lowerChar             = CharIn('a' to 'z')
  private val upperChar             = CharIn('A' to 'Z')
  private val char                  = lowerChar | upperChar
  private val digit                 = CharIn('0' to '9')
  private val unicodeSymbolP        = P("u" ~ P(digit | char) ~ P(digit | char) ~ P(digit | char) ~ P(digit | char))
  private val escapedUnicodeSymbolP = P("\\" ~ (CharIn("\"\\bfnrt") | unicodeSymbolP))
  private val varName               = (char.repX(min = 1, max = 1) ~~ (digit | char).repX()).!.filter(!keywords.contains(_))

  private val expr = P(binaryOp(opsByPriority) | atom)

  private val numberP: P[CONST_LONG] = P(CharIn("+-").rep(max = 1) ~ digit.repX(min = 1)).!.map(t => CONST_LONG(t.toLong))
  private val trueP: P[TRUE.type]    = P("true").map(_ => TRUE)
  private val falseP: P[FALSE.type]  = P("false").map(_ => FALSE)
  private val bracesP: P[EXPR]       = P("(" ~ expr ~ ")")
  private val curlyBracesP: P[EXPR]  = P("{" ~ expr ~ "}")
  private val letP: P[LET]           = P("let" ~ varName ~ "=" ~ expr).map { case ((x, y)) => LET(x, y) }
  private val refP: P[REF]           = P(varName).map(x => REF(x))
  private val ifP: P[IF]             = P("if" ~ bracesP ~ "then" ~ expr ~ "else" ~ expr).map { case (x, y, z) => IF(x, y, z) }

  private val functionCallArgs: P[Seq[EXPR]] = expr.rep(sep = ",")

  private val extractableAtom: P[EXPR] = P(curlyBracesP | bracesP | refP)

  private abstract class Accessor
  private case class Getter(name: String)   extends Accessor
  private case class Args(args: Seq[EXPR])  extends Accessor
  private case class ListIndex(index: EXPR) extends Accessor

  private val typesP: P[Seq[String]]    = varName.rep(min = 1, sep = "|")
  private val matchCaseP: P[MATCH_CASE] = P("case" ~ (varName.map(Some.apply) | P("_").map(_ => None)) ~ (P(":" ~ typesP) | P("").map(_ => List())) ~ "=>" ~ expr).map { case (v, types, e) => MATCH_CASE(v, types, e) }
  private lazy val matchP: P[MATCH]     = P("match" ~ expr ~ "{" ~ matchCaseP.rep(min = 1) ~ "}").map { case (e, cases) => MATCH(e, cases.toList) }

  private val accessP: P[Accessor] = P(("." ~~ varName).map(Getter.apply) | ("(" ~/ functionCallArgs.map(Args.apply) ~ ")")) | ("[" ~/ expr.map(
    ListIndex.apply) ~ "]")

  private val maybeAccessP: P[EXPR] = P(extractableAtom ~~ accessP.rep).map {
    case (e, f) =>
      f.foldLeft(e) { (e, a) =>
        a match {
          case Getter(n) => GETTER(e, n)
          case Args(args) =>
            e match {
              case REF(functionName) => FUNCTION_CALL(functionName, args.toList)
              case _                 => ???
            }
          case ListIndex(index) => FUNCTION_CALL("getElement", List(e, index))
        }
      }
  }

  private val byteVectorP: P[CONST_BYTEVECTOR] =
    P("base58'" ~~ CharsWhileIn(Base58Chars, 0).! ~~ "'")
      .map { x =>
        if (x.isEmpty) Right(Array.emptyByteArray) else Global.base58Decode(x)
      }
      .flatMap {
        case Left(e)   => Fail.opaque(e)
        case Right(xs) => PassWith(CONST_BYTEVECTOR(ByteVector(xs)))
      }

  private val stringP: P[CONST_STRING] =
    P("\"" ~~ (CharsWhile(!"\"\\".contains(_: Char)) | escapedUnicodeSymbolP).rep.! ~~ "\"").map(CONST_STRING)

  private val block: P[EXPR] = P(letP ~ expr).map(Function.tupled(BLOCK.apply))

  private val atom = P(ifP | matchP | byteVectorP | stringP | numberP | trueP | falseP | block | maybeAccessP)

  def unaryOp(ops: Seq[(P[Any], EXPR => EXPR)]): P[EXPR] = {
    ops.foldRight(atom) {
       (op, acc) => (op._1.map(_ => ()) ~ P(unaryOp(ops))).map(op._2) | acc
    }
  }

  private def binaryOp(rest: List[(BinaryOperation)]): P[EXPR] = rest match {
    case Nil => unaryOp(unaryOps)
    case kind :: restOps =>
      val operand = binaryOp(restOps)
      P(operand ~ (kind.parser.!.map(_ => kind) ~ operand).rep()).map {
        case (left: EXPR, r: Seq[(BinaryOperation, EXPR)]) =>
          r.foldLeft(left) { case (acc, (currKind, currOperand)) => currKind.expr(acc)(currOperand) }
      }
  }

  def apply(str: String): core.Parsed[EXPR, Char, String] = P(Start ~ expr ~ End).parse(str)
}
