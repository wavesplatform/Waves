package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._
import fastparse.WhitespaceApi
import scodec.bits.ByteVector
import scorex.crypto.encode.Base58

object Parser extends App {

  val Base58Chars  = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(" ", "\t", "\n").rep)
  }
  import fastparse.noApi._
  import White._

  def numberP: P[CONST_INT]            = P(CharIn('0' to '9').rep(min = 1).!.map(t => CONST_INT(t.toInt)))
  def sumP: P[SUM]                     = P((atom ~ "+" ~ expr).map { case ((x, y)) => SUM(x, y) })
  def trueP: P[TRUE.type]              = P("true").map(_ => TRUE)
  def falseP: P[FALSE.type]            = P("false").map(_ => FALSE)
  def byteVectorP: P[CONST_BYTEVECTOR] = P("base58'" ~ CharsWhileIn(Base58Chars) ~ "'").!.map(x => CONST_BYTEVECTOR(ByteVector(Base58.decode(x).get)))
  def andP: P[AND]                     = P((atom ~ "&&" ~ expr).map { case ((x, y)) => AND(x, y) })
  def orP: P[OR]                       = P((atom ~ "||" ~ expr).map { case ((x, y)) => OR(x, y) })
  def eqIntP: P[EQ_INT]                = P((atom ~ "==" ~ expr).map { case ((x, y)) => EQ_INT(x, y) })
  def gtP: P[GT]                       = P((atom ~ ">" ~ expr).map { case ((x, y)) => GT(x, y) })
  def geP: P[GE]                       = P((atom ~ ">=" ~ expr).map { case ((x, y)) => GE(x, y) })
  def bracesP: P[Expr]                 = P("(" ~ expr ~ ")")

  def stmt = P(expr)
  def expr = P(sumP | andP | orP | eqIntP | eqIntP | gtP | geP | atom)
  def atom = P(byteVectorP | numberP | trueP | falseP | bracesP)
  println(stmt.parse("(1)"))
  println(stmt.parse("(1+1) + 1"))
  println(stmt.parse("(1+1) + (1+1)"))
  println(stmt.parse("((1+1) + (1+1))"))

}
