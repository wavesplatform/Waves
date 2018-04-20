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

  private val numberP: P[CONST_LONG] = P(CharIn("+-").rep(max = 1) ~ digit.repX(min = 1)).!.map(t => CONST_LONG(t.toLong))
  private val trueP: P[TRUE.type]    = P("true").map(_ => TRUE)
  private val falseP: P[FALSE.type]  = P("false").map(_ => FALSE)
  private val bracesP: P[EXPR]       = P("(" ~ expr ~ ")")
  private val curlyBracesP: P[EXPR]  = P("{" ~ expr ~ "}")
  private val letP: P[LET]           = P("let" ~ varName ~ "=" ~ expr).map { case ((x, y)) => LET(x, y) }
  private val refP: P[REF]           = P(varName).map(x => REF(x))
  private val ifP: P[IF]             = P("if" ~ bracesP ~ "then" ~ expr ~ "else" ~ expr).map { case (x, y, z) => IF(x, y, z) }

  private val functionCallArgs: P[Seq[EXPR]] = expr.rep(sep = ",")

  private val functionCallP: P[FUNCTION_CALL] = P(varName ~~ "(" ~ functionCallArgs ~ ")").map {
    case (functionName, args) => FUNCTION_CALL(functionName, args.toList)
  }

  private val extractableAtom: P[EXPR] = P(curlyBracesP | bracesP | functionCallP | refP)

  private val maybeGetterP: P[EXPR] = P(extractableAtom ~ ("." ~ varName).?).map {
    case (e, f) => f.fold(e)(GETTER(e, _))
  }

  private val byteVectorP: P[CONST_BYTEVECTOR] =
    P("base58'" ~ CharsWhileIn(Base58Chars, 0).! ~ "'")
      .map { x =>
        if (x.isEmpty) Right(Array.emptyByteArray) else Global.base58Decode(x)
      }
      .flatMap {
        case Left(e)   => Fail.opaque(e)
        case Right(xs) => PassWith(CONST_BYTEVECTOR(ByteVector(xs)))
      }

  private val stringP: P[CONST_STRING] =
    P("\"" ~ (CharsWhile(!"\"\\".contains(_: Char)) | escapedUnicodeSymbolP).rep.! ~ "\"").map(CONST_STRING)

  private val block: P[EXPR] = P(letP ~ expr).map(Function.tupled(BLOCK.apply))

  private val atom      = P(ifP | byteVectorP | stringP | numberP | trueP | falseP | block | maybeGetterP)
  private lazy val expr = P(binaryOp(opsByPriority) | atom)

  private def binaryOp(rest: List[(String, BINARY_OP_KIND)]): P[EXPR] = rest match {
    case Nil => atom
    case (lessPriorityOp, kind) :: restOps =>
      val operand = binaryOp(restOps)
      P(operand ~ (lessPriorityOp.!.map(_ => kind) ~ operand).rep()).map {
        case ((left: EXPR, r: Seq[(BINARY_OP_KIND, EXPR)])) =>
          r.foldLeft(left) { case (acc, (currKind, currOperand)) => BINARY_OP(acc, currKind, currOperand) }
      }
  }

  def apply(str: String): core.Parsed[EXPR, Char, String] = P(Start ~ expr ~ End).parse(str)
}
