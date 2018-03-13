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

  private val lowerChar = CharIn('a' to 'z')
  private val upperChar = CharIn('A' to 'Z')
  private val char      = lowerChar | upperChar
  private val digit     = CharIn('0' to '9')
  private val varName   = (char.rep(min = 1) ~ (digit | char).rep()).!

  private def numberP: P[CONST_INT] = P(digit.rep(min = 1).!.map(t => CONST_INT(t.toInt)))
  private def trueP: P[TRUE.type]   = P("true").map(_ => TRUE)
  private def falseP: P[FALSE.type] = P("false").map(_ => FALSE)
  private def bracesP: P[EXPR]      = P("(" ~ block ~ ")")
  private def curlyBracesP: P[EXPR] = P("{" ~ block ~ "}")
  private def letP: P[LET]          = P("let " ~ varName ~ "=" ~ block).map { case ((x, y)) => LET(x, y) }
  private def refP: P[REF]          = P(varName).map(x => REF(x))
  private def ifP: P[IF]            = P("if" ~ "(" ~ block ~ ")" ~ "then" ~ block ~ "else" ~ block).map { case (x, y, z) => IF(x, y, z) }
  //private def isDefined: P[IS_DEFINED] = P("isDefined" ~ "(" ~ block ~ ")").map(b => IS_DEFINED(b))
  //private def getP: P[GET]             = P("extract" ~ "(" ~ block ~ ")").map(b => GET(b))
  private def someP: P[SOME]      = P("Some" ~ "(" ~ block ~ ")").map(x => SOME(x))
  private def noneP: P[NONE.type] = P("None").map(_ => NONE)

  private def functionCallArgs: P[Seq[EXPR]] = expr.rep(min = 0, sep = ",")

  private def functionCallP: P[FUNCTION_CALL] = P(varName ~ "(" ~ functionCallArgs ~ ")").map {
    case (functionName, args) => FUNCTION_CALL(functionName, args.toList)
  }

  private def getterP: P[GETTER] = P(refP ~ "." ~ varName).map { case ((b, f)) => GETTER(b, f) }

  private def patmat1P: P[BLOCK] =
    P("match" ~ "(" ~ block ~ ")" ~ "{" ~ "case" ~ "None" ~ "=>" ~ block ~ "case" ~ "Some" ~ "(" ~ varName ~ ")" ~ "=>" ~ block ~ "}")
      .map { case ((exp, ifNone, ref, ifSome)) => patmat(exp, ref, ifSome, ifNone) }

  private def patmat2P: P[BLOCK] =
    P("match" ~ "(" ~ block ~ ")" ~ "{" ~ "case" ~ "Some" ~ "(" ~ varName ~ ")" ~ "=>" ~ block ~ "case" ~ "None" ~ "=>" ~ block ~ "}")
      .map { case ((exp, ref, ifSome, ifNone)) => patmat(exp, ref, ifSome, ifNone) }

  def patmat(exp: EXPR, ref: String, ifSome: EXPR, ifNone: EXPR): BLOCK =
    BLOCK(
      Some(LET("@exp", exp)),
      IF(IS_DEFINED(REF("@exp")),
         BLOCK(
           Some(LET(ref, GET(REF("@exp")))),
           ifSome
         ),
         ifNone)
    )

  private def byteVectorP: P[CONST_BYTEVECTOR] =
    P("base58'" ~ CharsWhileIn(Base58Chars).! ~ "'").map(x => CONST_BYTEVECTOR(ByteVector(base58Decode(x).get)))

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
    P(functionCallP | ifP | patmat1P | patmat2P | byteVectorP | numberP | trueP | falseP | noneP | someP | bracesP | curlyBracesP | getterP | refP) // | isDefined | getP )

  def apply(str: String): core.Parsed[EXPR, Char, String] = P(block ~ End).parse(str)
}
