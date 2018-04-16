package com.wavesplatform.lang.v1

import com.wavesplatform.lang.v1.Terms.Untyped._
import com.wavesplatform.lang.v1.Terms._
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
  private val keywords    = Set("let", "base58", "true", "false", "if", "then", "else")

  private val lowerChar             = CharIn('a' to 'z')
  private val upperChar             = CharIn('A' to 'Z')
  private val char                  = lowerChar | upperChar
  private val digit                 = CharIn('0' to '9')
  private val unicodeSymbolP        = P("u" ~ P(digit | char) ~ P(digit | char) ~ P(digit | char) ~ P(digit | char))
  private val escapedUnicodeSymbolP = P("\\" ~ (CharIn("\"\\bfnrt") | unicodeSymbolP))
  private val varName               = (char.repX(min = 1, max = 1) ~~ (digit | char).repX()).!.filter(!keywords.contains(_))

  private def numberP: P[CONST_LONG] = P((CharIn(Seq('+', '-')).rep(max = 1) ~ digit.rep(min = 1)).!.map(t => CONST_LONG(t.toLong)))
  private def trueP: P[TRUE.type]    = P("true").map(_ => TRUE)
  private def falseP: P[FALSE.type]  = P("false").map(_ => FALSE)
  private def bracesP: P[EXPR]       = P("(" ~ block ~ ")")
  private def curlyBracesP: P[EXPR]  = P("{" ~ block ~ "}")
  private def letP: P[LET]           = P("let " ~ varName ~ "=" ~ block).map { case ((x, y)) => LET(x, y) }
  private def refP: P[REF]           = P(varName).map(x => REF(x))
  private def ifP: P[IF]             = P("if" ~ "(" ~ block ~ ")" ~ "then" ~ block ~ "else" ~ block).map { case (x, y, z) => IF(x, y, z) }

  private def functionCallArgs: P[Seq[EXPR]] = expr.rep(min = 0, sep = ",")

  private def functionCallP: P[FUNCTION_CALL] = P(varName ~ "(" ~ functionCallArgs ~ ")").map {
    case (functionName, args) => FUNCTION_CALL(functionName, args.toList)
  }

  private def extractableAtom: P[EXPR] = P(curlyBracesP | bracesP | functionCallP | refP)

  private def getterP: P[GETTER] = P(extractableAtom ~ "." ~ varName).map { case (e, f) => GETTER(e, f) }

  private def byteVectorP: P[CONST_BYTEVECTOR] =
    P("base58'" ~ CharsWhileIn(Base58Chars, 0).! ~ "'")
      .map { x =>
        if (x.isEmpty) Right(Array.emptyByteArray) else Global.base58Decode(x)
      }
      .flatMap {
        case Left(e)   => Fail.opaque(e)
        case Right(xs) => PassWith(CONST_BYTEVECTOR(ByteVector(xs)))
      }

  private def stringP: P[CONST_STRING] =
    P("\"" ~ (CharsWhile(!"\"\\".contains(_: Char)) | escapedUnicodeSymbolP).rep.! ~ "\"").map(CONST_STRING)

  private def block: P[EXPR] = P("\n".rep ~ letP.rep ~ expr ~ ";".rep).map {
    case ((Nil, y)) => y
    case ((all, y)) => all.foldRight(y) { case (r, curr) => BLOCK(Some(r), curr) }
  }

  private def binaryOp(rest: List[(String, BINARY_OP_KIND)]): P[EXPR] = rest match {
    case Nil => atom
    case (lessPriorityOp, kind) :: restOps =>
      val operand = binaryOp(restOps)
      P(operand ~ (lessPriorityOp.!.map(_ => kind) ~ operand).rep()).map {
        case ((left: EXPR, r: Seq[(BINARY_OP_KIND, EXPR)])) =>
          r.foldLeft(left) { case (acc, (currKind, currOperand)) => BINARY_OP(acc, currKind, currOperand) }
      }
  }

  private def expr = P(binaryOp(opsByPriority) | atom)

  private def atom =
    P(ifP | byteVectorP | stringP | numberP | trueP | falseP | getterP | bracesP | curlyBracesP | functionCallP | refP)

  def apply(str: String): core.Parsed[EXPR, Char, String] = P(block ~ End).parse(str)
}
