package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._
import fastparse.{WhitespaceApi, core, noApi}
import scodec.bits.ByteVector
import scorex.crypto.encode.Base58

object Parser {

  private val Base58Chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  private val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(" ", "\t", "\r").rep)
  }

  import fastparse.noApi._
  import White._

  private def numberP: P[CONST_INT]     = P(CharIn('0' to '9').rep(min = 1).!.map(t => CONST_INT(t.toInt)))
  private def trueP: P[TRUE.type]       = P("true").map(_ => TRUE)
  private def falseP: P[FALSE.type]     = P("false").map(_ => FALSE)
  private def bracesP: P[Expr]          = P("(" ~ expr ~ ")")
  private def sigVerifyP: P[SIG_VERIFY] = P("checkSig" ~ "(" ~ expr ~ "," ~ expr ~ "," ~ expr ~ ")").map { case ((x, y, z)) => SIG_VERIFY(x, y, z) }
  private def byteVectorP: P[CONST_BYTEVECTOR] =
    P("base58'" ~ CharsWhileIn(Base58Chars).! ~ "'").map(x => CONST_BYTEVECTOR(ByteVector(Base58.decode(x).get)))

  private def letP: P[LET] = P("let" ~ CharIn('A' to 'Z').! ~ "=" ~ expr ~ "\n").map { case ((x, y)) => LET(x, y) }

  private def stmt: P[Expr] = P(letP.rep ~ expr).map {
    case ((Nil, y)) => y
    case ((all, y)) => all.foldRight(y) { case (r, curr) => CExpr(Some(r), curr) }

  }

  private val priority = List("||", "&&", "==", ">=", ">", "+")

  private def binaryOp(rest: List[String]): P[Expr] = rest match {
    case Nil => atom
    case lessPriorityOp :: restOps =>
      val operand = binaryOp(restOps)
      P(operand ~ (lessPriorityOp.! ~ operand).rep()).map {
        case ((left: Expr, r: Seq[(String, Expr)])) =>
          r.foldLeft(left) {
            case (r2, (op, y)) =>
              op match {
                case "||" => OR(r2, y)
                case "&&" => AND(r2, y)
                case "==" => EQ_INT(r2, y)
                case ">=" => GE(r2, y)
                case ">" =>
                  GT(r2, y)
                case "+" =>
                  SUM(r2, y)
              }
          }

      }
  }

  private def expr = P(binaryOp(priority) | atom)

  private def atom = P(byteVectorP | numberP | trueP | falseP | sigVerifyP | bracesP)

  def apply(str: String): core.Parsed[Expr, Char, String] = stmt.parse(str)
}
