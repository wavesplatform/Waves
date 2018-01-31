package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._
import fastparse.{WhitespaceApi, core}
import scodec.bits.ByteVector
import scorex.crypto.encode.Base58

object Parser  {

  private val Base58Chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  private  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(" ", "\t", "\n").rep)
  }
  import fastparse.noApi._
  import White._

  private def numberP: P[CONST_INT]            = P(CharIn('0' to '9').rep(min = 1).!.map(t => CONST_INT(t.toInt)))
  private def sumP: P[SUM]                     = P((atom ~ "+" ~ expr).map { case ((x, y)) => SUM(x, y) })
  private def trueP: P[TRUE.type]              = P("true").map(_ => TRUE)
  private def falseP: P[FALSE.type]            = P("false").map(_ => FALSE)
  private def byteVectorP: P[CONST_BYTEVECTOR] = P("base58'" ~ CharsWhileIn(Base58Chars) ~ "'").!.map(x => CONST_BYTEVECTOR(ByteVector(Base58.decode(x).get)))
  private def andP: P[AND]                     = P((atom ~ "&&" ~ expr).map { case ((x, y)) => AND(x, y) })
  private def orP: P[OR]                       = P((atom ~ "||" ~ expr).map { case ((x, y)) => OR(x, y) })
  private def eqIntP: P[EQ_INT]                = P((atom ~ "==" ~ expr).map { case ((x, y)) => EQ_INT(x, y) })
  private def gtP: P[GT]                       = P((atom ~ ">" ~ expr).map { case ((x, y)) => GT(x, y) })
  private def geP: P[GE]                       = P((atom ~ ">=" ~ expr).map { case ((x, y)) => GE(x, y) })
  private def bracesP: P[Expr]                 = P("(" ~ expr ~ ")")

  private def stmt = P(expr)
  private def expr = P(sumP | gtP | geP | andP | orP | eqIntP | eqIntP | gtP | geP | atom)
  private def atom = P(byteVectorP | numberP | trueP | falseP | bracesP)

  def apply(str: String): core.Parsed[Expr, Char, String] = stmt.parse(str)
}
