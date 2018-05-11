package com.wavesplatform.lang.v1.parser

import com.wavesplatform.lang.v1.parser.BinaryOperation._
import com.wavesplatform.lang.v1.parser.Expressions._
import com.wavesplatform.lang.v1.parser.UnaryOperation._
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

  val keywords               = Set("let", "base58", "true", "false", "if", "then", "else", "match", "case")
  private val lowerChar      = CharIn('a' to 'z')
  private val upperChar      = CharIn('A' to 'Z')
  private val char           = lowerChar | upperChar
  private val digit          = CharIn('0' to '9')
  private val unicodeSymbolP = P("\\u" ~/ Pass ~~ (char | digit).repX(min = 0, max = 4))
  private val notEndOfString = CharPred(_ != '\"')
  private val specialSymbols = P("\\" ~~ notEndOfString.?)

  private val escapedUnicodeSymbolP = P(NoCut(unicodeSymbolP) | specialSymbols)
  private val stringP: P[EXPR] = P("\"" ~/ Pass ~~ (escapedUnicodeSymbolP | notEndOfString).!.repX ~~ "\"")
    .map { xs =>
      var errors         = Vector.empty[String]
      val consumedString = new StringBuilder

      xs.foreach { x =>
        if (x.startsWith("\\u")) {
          if (x.length == 6) {
            val hexCode = x.drop(2)
            try {
              val int           = Integer.parseInt(hexCode, 16)
              val unicodeSymbol = new String(Character.toChars(int))
              consumedString.append(unicodeSymbol)
            } catch {
              case _: NumberFormatException =>
                consumedString.append(x)
                errors :+= s"Can't parse '$hexCode' as HEX string in '$x'"
              case _: IllegalArgumentException =>
                consumedString.append(x)
                errors :+= s"Invalid UTF-8 symbol: '$x'"
            }
          } else {
            consumedString.append(x)
            errors :+= s"Incomplete UTF-8 symbol definition: '$x'"
          }
        } else if (x.startsWith("\\")) {
          if (x.length == 2) {
            consumedString.append(x(1) match {
              case 'b' => "\b"
              case 'f' => "\f"
              case 'n' => "\n"
              case 'r' => "\r"
              case 't' => "\t"
              case _ =>
                errors :+= s"Unknown escaped symbol: '$x'"
                x
            })
          } else {
            consumedString.append(x)
            errors :+= s"Invalid escaped symbol: '$x'"
          }
        } else {
          consumedString.append(x)
        }
      }

      if (errors.isEmpty) PART.VALID(consumedString.toString)
      else PART.INVALID(consumedString.toString, errors.mkString(";"))
    }
    .map(CONST_STRING(_))

  private val varName: P[PART[String]] = (char ~~ (digit | char).repX()).!.map { x =>
    if (keywords.contains(x)) PART.INVALID(x, "keywords are restricted")
    else PART.VALID(x)
  }

  private val invalid: P[INVALID] = P(AnyChars(1).! ~ fallBackExpr.?).map {
    case (xs, next) => foldInvalid(xs, next)
  }

  private def foldInvalid(xs: String, next: Option[EXPR]): INVALID = next match {
    case Some(INVALID(nextXs, nextNext)) => foldInvalid(xs + nextXs, nextNext)
    case x                               => INVALID(xs, x)
  }

  private val numberP: P[CONST_LONG] = P(CharIn("+-").? ~ digit.repX(min = 1)).!.map(t => CONST_LONG(t.toLong))
  private val trueP: P[TRUE.type]    = P("true").map(_ => TRUE)
  private val falseP: P[FALSE.type]  = P("false").map(_ => FALSE)
  private val bracesP: P[EXPR]       = P("(" ~ fallBackExpr ~ ")")
  private val curlyBracesP: P[EXPR]  = P("{" ~ fallBackExpr ~ "}")
  private val letP: P[LET]           = P("let" ~ varName ~ "=" ~ fallBackExpr).map(Function.tupled(LET(_, _, Seq.empty)))
  private val refP: P[REF]           = P(varName).map(REF(_))
  private val ifP: P[IF]             = P("if" ~ bracesP ~ "then" ~ fallBackExpr ~ "else" ~ fallBackExpr).map { case (x, y, z) => IF(x, y, z) }

  private val functionCallArgs: P[Seq[EXPR]] = fallBackExpr.rep(sep = ",")

  private val extractableAtom: P[EXPR] = P(curlyBracesP | bracesP | refP)

  private abstract class Accessor
  private case class Getter(name: PART[String]) extends Accessor
  private case class Args(args: Seq[EXPR])      extends Accessor
  private case class ListIndex(index: EXPR)     extends Accessor

  private val matchCaseP: P[MATCH_CASE] = {
    val restMatchCaseInvalidP: P[String] = P((!"=>" ~~ AnyChars(1).!).repX.map(_.mkString))
    val varDefP: P[Option[PART[String]]] = varName.map(Some(_)) | "_".!.map(_ => None)
    val typesP: P[Seq[PART[String]]]     = varName.rep(min = 1, sep = "|")
    val typesDefP = (
      ":" ~
        (typesP | restMatchCaseInvalidP.map(x => Seq(PART.INVALID(x, "the type for variable should be specified: `case varName: Type => expr`"))))
    ).?.map(_.getOrElse(List.empty))

    P(
      "case" ~/ (
        (varDefP ~ typesDefP) |
          restMatchCaseInvalidP.map { x =>
            (
              Some(PART.INVALID(x, "invalid syntax, should be: `case varName: Type => expr` or `case _ => expr`")),
              Seq.empty[PART[String]]
            )
          }
      ) ~ "=>" ~/ baseExpr.?
    ).map {
      case (v, types, e) =>
        MATCH_CASE(
          newVarName = v,
          types = types,
          expr = e.getOrElse(INVALID("expected expression"))
        )
    }
  }
  private lazy val matchP: P[EXPR] = P("match" ~/ fallBackExpr ~ "{" ~ NoCut(matchCaseP).rep ~ "}")
    .map {
      case (_, Nil)   => INVALID("pattern matching requires case branches")
      case (e, cases) => MATCH(e, cases.toList)
    }

  private val accessP
    : P[Accessor] = P(("." ~~ varName).map(Getter) | ("(" ~/ functionCallArgs.map(Args) ~ ")")) | ("[" ~/ fallBackExpr.map(ListIndex) ~ "]")

  private val maybeAccessP: P[EXPR] = P(extractableAtom ~~ NoCut(accessP).rep).map {
    case (e, f) =>
      f.foldLeft(e) { (e, a) =>
        a match {
          case Getter(n) => GETTER(e, n)
          case Args(args) =>
            e match {
              case REF(functionName) => FUNCTION_CALL(functionName, args.toList)
              case _                 => FUNCTION_CALL(PART.INVALID("", s"$e is not a function name"), args.toList)
            }
          case ListIndex(index) => FUNCTION_CALL("getElement", List(e, index))
        }
      }
  }

  private val byteVectorP: P[EXPR] =
    P("base58'" ~/ Pass ~~ CharPred(_ != '\'').repX.! ~~ "'")
      .map { xs =>
        val decoded = if (xs.isEmpty) Right(Array.emptyByteArray) else Global.base58Decode(xs)
        decoded match {
          case Left(_)  => CONST_BYTEVECTOR(PART.INVALID(xs, "Can't parse Base58 string"))
          case Right(r) => CONST_BYTEVECTOR(PART.VALID(ByteVector(r)))
        }
      }

  private val block: P[EXPR] = P(letP ~ fallBackExpr).map(Function.tupled(BLOCK))

  private val baseAtom      = P(ifP | NoCut(matchP) | byteVectorP | stringP | numberP | trueP | falseP | block | maybeAccessP)
  private lazy val baseExpr = P(binaryOp(baseAtom, opsByPriority) | baseAtom)

  private lazy val fallBackExpr = {
    val fallBackAtom = P(baseAtom | invalid)
    P(binaryOp(fallBackAtom, opsByPriority) | fallBackAtom)
  }

  private def binaryOp(atom: P[EXPR], rest: List[BinaryOperation]): P[EXPR] = rest match {
    case Nil => unaryOp(atom, unaryOps)
    case kind :: restOps =>
      val operand = binaryOp(atom, restOps)
      P(operand ~ (kind.parser.!.map(_ => kind) ~/ operand).rep()).map {
        case (left: EXPR, r: Seq[(BinaryOperation, EXPR)]) =>
          r.foldLeft(left) { case (acc, (currKind, currOperand)) => currKind.expr(acc)(currOperand) }
      }
  }

  def unaryOp(atom: P[EXPR], ops: Seq[(P[Any], EXPR => EXPR)]): P[EXPR] = ops.foldRight(atom) {
    case ((parser, transformer), acc) =>
      (parser.map(_ => ()) ~ P(unaryOp(atom, ops))).map(transformer) | acc
  }

  def apply(str: String): core.Parsed[Seq[EXPR], Char, String] = P(Start ~ fallBackExpr.rep(min = 1) ~ End).parse(str)
}
