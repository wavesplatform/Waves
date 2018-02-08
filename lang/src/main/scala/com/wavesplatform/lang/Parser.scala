package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._
import fastparse.{WhitespaceApi, core, noApi}
import scodec.bits.ByteVector
import scorex.crypto.encode.Base58

object Parser {

  private val Base58Chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  private val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(" ", "\t", "\r", "\n").rep)
  }

  import fastparse.noApi._
  import White._

  val varName = CharIn('A' to 'Z').rep(1).!

  private def numberP: P[CONST_INT]    = P(CharIn('0' to '9').rep(min = 1).!.map(t => CONST_INT(t.toInt)))
  private def trueP: P[TRUE.type]      = P("true").map(_ => TRUE)
  private def falseP: P[FALSE.type]    = P("false").map(_ => FALSE)
  private def bracesP: P[Expr]         = P("(" ~ block ~ ")")
  private def curlyBracesP: P[Expr]    = P("{" ~ block ~ "}")
  private def letP: P[LET]             = P("let " ~ varName ~ "=" ~ block).map { case ((x, y)) => LET(x, y) }
  private def refP: P[REF]             = P(varName).map(x => REF(x))
  private def ifP: P[IF]               = P("if" ~ "(" ~ block ~ ")" ~ "then" ~ block ~ "else" ~ block).map { case (x, y, z) => IF(x, y, z) }
  private def isDefined: P[IS_DEFINED] = P("isDefined" ~ "(" ~ block ~ ")").map(b => IS_DEFINED(b))
  private def getP: P[GET]             = P("get" ~ "(" ~ block ~ ")").map(b => GET(b))
  private def someP: P[SOME]           = P("Some" ~ "(" ~ block ~ ")").map(x => SOME(x))
  private def noneP: P[NONE.type]      = P("None").map(_ => NONE)
  private def heightP: P[HEIGHT.type]  = P("h").map(_ => HEIGHT)

  private def patmat1P: P[Block] =
    P("match" ~ "(" ~ block ~ ")" ~ "{" ~ "case" ~ "None" ~ "=>" ~ block ~ "case" ~ "Some" ~ "(" ~ varName ~ ")" ~ "=>" ~ block ~ "}")
      .map { case ((exp, ifNone, ref, ifSome)) => patmat(exp, ref, ifSome, ifNone) }

  private def patmat2P: P[Block] =
    P("match" ~ "(" ~ block ~ ")" ~ "{" ~ "case" ~ "Some" ~ "(" ~ varName ~ ")" ~ "=>" ~ block ~ "case" ~ "None" ~ "=>" ~ block ~ "}")
      .map { case ((exp, ref, ifSome, ifNone)) => patmat(exp, ref, ifSome, ifNone) }

  def patmat(exp: Block, ref: String, ifSome: Block, ifNone: Block): Block =
    Block(
      Some(LET("$exp", exp)),
      IF(IS_DEFINED(REF("$exp")),
         Block(
           Some(LET(ref, GET(REF("$exp")))),
           ifSome
         ),
         ifNone)
    )

  private def sigVerifyP: P[SIG_VERIFY] = P("checkSig" ~ "(" ~ block ~ "," ~ block ~ "," ~ block ~ ")").map {
    case ((x, y, z)) => SIG_VERIFY(x, y, z)
  }
  private def byteVectorP: P[CONST_BYTEVECTOR] =
    P("base58'" ~ CharsWhileIn(Base58Chars).! ~ "'").map(x => CONST_BYTEVECTOR(ByteVector(Base58.decode(x).get)))

  private def block: P[Expr] = P("\n".rep ~ letP.rep ~ expr ~ ";".rep).map {
    case ((Nil, y)) => y
    case ((all, y)) => all.foldRight(y) { case (r, curr) => Block(Some(r), curr) }

  }

  private val priority = List("||", "&&", "==", ">=", ">", "+", "-", "*")

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
                case "==" => EQ(r2, y)
                case ">=" => GE(r2, y)
                case ">"  => GT(r2, y)
                case "+"  => SUM(r2, y)
              }
          }

      }
  }

  private def expr = P(binaryOp(priority) | atom)

  private def atom =
    P(ifP | patmat1P | patmat2P | byteVectorP | numberP | trueP | falseP | noneP | someP | bracesP | curlyBracesP | sigVerifyP | refP | isDefined | getP )

  def apply(str: String): core.Parsed[Expr, Char, String] = block.parse(str)
}
