package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang.Terms.Untyped._
import com.wavesplatform.lang.traits.Base58
import fastparse.{WhitespaceApi, core}
import scodec.bits.ByteVector

abstract class ParserImpl { this: Base58 =>

  private val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(" ", "\t", "\r", "\n").rep)
  }

  import White._
  import fastparse.noApi._
  private val Base58Chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  private val StringChars = """1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-"""
  private val keywords = Set("let", "base58", "true", "false", "if", "then", "else")

  private val lowerChar = CharIn('a' to 'z')
  private val upperChar = CharIn('A' to 'Z')
  private val char      = lowerChar | upperChar
  private val digit     = CharIn('0' to '9')
  private val varName   = (char.repX(min = 1, max = 1) ~~ (digit | char).repX()).!.filter(!keywords.contains(_))

  private def numberP: P[CONST_LONG] = P(digit.rep(min = 1).!.map(t => CONST_LONG(t.toInt)))
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

  private def getterP: P[GETTER] = P(refP ~ "." ~ varName).map { case ((b, f)) => GETTER(b, f) }

  private def byteVectorP: P[CONST_BYTEVECTOR] =
    P("base58'" ~ CharsWhileIn(Base58Chars).! ~ "'").map(x => CONST_BYTEVECTOR(ByteVector(base58Decode(x).get)))

  private def stringP: P[CONST_STRING] =
    P("\"" ~ CharsWhileIn(StringChars).! ~ "\"").map(x => CONST_STRING(x))

  private def block: P[EXPR] = P("\n".rep ~ letP.rep ~ expr ~ ";".rep).map {
    case ((Nil, y)) => y
    case ((all, y)) => all.foldRight(y) { case (r, curr) => BLOCK(Some(r), curr) }
  }

  private val opsByPriority = List[(String, BINARY_OP_KIND)](
    "||" -> OR_OP,
    "&&" -> AND_OP,
    "==" -> EQ_OP,
    ">=" -> GE_OP,
    ">"  -> GT_OP,
    "+"  -> SUM_OP
  )

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
    P(ifP | functionCallP | byteVectorP | stringP |numberP | trueP | falseP | bracesP | curlyBracesP | getterP | refP)

  def apply(str: String): core.Parsed[EXPR, Char, String] = P(block ~ End).parse(str)
}
