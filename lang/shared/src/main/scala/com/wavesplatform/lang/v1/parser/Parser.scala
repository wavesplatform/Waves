package com.wavesplatform.lang.v1.parser

import com.wavesplatform.lang.v1.parser.BinaryOperation._
import com.wavesplatform.lang.v1.parser.Expressions._
import com.wavesplatform.lang.v1.parser.UnaryOperation._
import fastparse.{WhitespaceApi, core}
import scodec.bits.ByteVector

object Parser {

  private val Global                 = com.wavesplatform.lang.hacks.Global // Hack for IDEA
  private val Whitespaces: Set[Char] = " \t\r\n".toSet

  private val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(Whitespaces.toSeq).rep)
  }

  import White._
  import fastparse.noApi._

  val keywords               = Set("let", "base58", "base64", "true", "false", "if", "then", "else", "match", "case")
  private val lowerChar      = CharIn('a' to 'z')
  private val upperChar      = CharIn('A' to 'Z')
  private val char           = lowerChar | upperChar
  private val digit          = CharIn('0' to '9')
  private val unicodeSymbolP = P("\\u" ~/ Pass ~~ (char | digit).repX(min = 0, max = 4))
  private val notEndOfString = CharPred(_ != '\"')
  private val specialSymbols = P("\\" ~~ notEndOfString.?)
  private val comment        = P("#" ~~ CharPred(_ != '\n').repX).rep

  private val escapedUnicodeSymbolP: P[(Int, String, Int)] = P(Index ~~ (NoCut(unicodeSymbolP) | specialSymbols).! ~~ Index)
  private val stringP: P[EXPR] = P(Index ~~ "\"" ~/ Pass ~~ (escapedUnicodeSymbolP | notEndOfString).!.repX ~~ "\"" ~~ Index)
    .map {
      case (start, xs, end) =>
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
                  errors :+= s"can't parse '$hexCode' as HEX string in '$x'"
                case _: IllegalArgumentException =>
                  consumedString.append(x)
                  errors :+= s"invalid UTF-8 symbol: '$x'"
              }
            } else {
              consumedString.append(x)
              errors :+= s"incomplete UTF-8 symbol definition: '$x'"
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
                  errors :+= s"""unknown escaped symbol: '$x'. The valid are \b, \f, \n, \r, \t"""
                  x
              })
            } else {
              consumedString.append(x)
              errors :+= s"""invalid escaped symbol: '$x'. The valid are \b, \f, \n, \r, \t"""
            }
          } else {
            consumedString.append(x)
          }
        }

        val r =
          if (errors.isEmpty) PART.VALID(start + 1, end - 1, consumedString.toString)
          else PART.INVALID(start + 1, end - 1, errors.mkString(";"))
        (start, end, r)
    }
    .map(Function.tupled(CONST_STRING))

  private val varName: P[PART[String]] = (Index ~~ (char ~~ (digit | char).repX()).! ~~ Index)
    .map {
      case (start, x, end) =>
        if (keywords.contains(x)) PART.INVALID(start, end, s"keywords are restricted: $x")
        else PART.VALID(start, end, x)
    }

  private val varName2: P[PART[String]] = (Index ~~ (char ~~ (digit | char).repX()).! ~~ Index)
  //    .map {
  //    case (start, x, end) =>
  //      if (keywords.contains(x)) PART.INVALID(start, end, s"keywords are restricted: $x")
  //      else PART.VALID(start, end, x)
  //  }
    .filter {
      case (start, x, end) => !keywords.contains(x)
    }
    .map {
      case (start, x, end) =>
        PART.VALID(start, end, x)
    }

  private val invalid: P[INVALID] = {
    val White = WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace("")
    }

    import White._
    import fastparse.noApi._

    P(Index ~ CharPred(x => !Whitespaces.contains(x)).rep(min = 1).! ~ Index)
      .map {
        case (start, xs, end) => foldInvalid(start, end, xs, None)
      }
      .log("invalid")
  }

  private def foldInvalid(start: Int, end: Int, xs: String, next: Option[EXPR]): INVALID = next match {
    case Some(INVALID(_, endInvalid, nextXs, nextNext)) => foldInvalid(start, endInvalid, xs + nextXs, nextNext)
    case x                                              => INVALID(start, end, xs, x)
  }

  private val numberP: P[CONST_LONG] =
    P(Index ~~ (CharIn("+-").? ~~ digit.repX(min = 1)).! ~~ ("_" ~~ digit.repX(min = 1).!).repX(min = 0) ~~ Index)
      .map({ case (start, x1, x2, end) => CONST_LONG(start, end, x2.foldLeft(x1)(_ ++ _).toLong) })

  private val trueP: P[TRUE]        = P(Index ~~ "true".! ~~ Index).map { case (start, _, end) => TRUE(start, end) }
  private val falseP: P[FALSE]      = P(Index ~~ "false".! ~~ Index).map { case (start, _, end) => FALSE(start, end) }
  private val bracesP: P[EXPR]      = P("(" ~ fallBackExpr ~ ")")
  private val curlyBracesP: P[EXPR] = P("{" ~ fallBackExpr ~ "}")
  private val letP: P[LET] = P(Index ~~ "let" ~ comment ~ varName ~ comment ~ "=" ~/ fallBackExpr ~~ Index).map {
    case (start, v, e, end) => LET(start, end, v, e, Seq.empty)
  }
  private val refP: P[REF] = P(varName)
    .map { x =>
      REF(x.start, x.end, x)
    }
    .log("refP")
  private val ifP: P[IF] = P(Index ~~ "if" ~ fallBackExpr ~ "then" ~ fallBackExpr ~ "else" ~ fallBackExpr ~~ Index).map {
    case (start, x, y, z, end) => IF(start, end, x, y, z)
  }

  private val functionCallArgs: P[Seq[EXPR]] = fallBackExpr.rep(sep = ",")

  private val extractableAtom: P[EXPR] = P(curlyBracesP | bracesP | refP).log("extractableAtom")

  private abstract class Accessor
  private case class Getter(name: PART[String]) extends Accessor
  private case class Args(args: Seq[EXPR])      extends Accessor
  private case class ListIndex(index: EXPR)     extends Accessor

  private val matchCaseP: P[MATCH_CASE] = {
    val restMatchCaseInvalidP: P[String] = P((!"=>" ~~ AnyChars(1).!).repX.map(_.mkString))
    val varDefP: P[Option[PART[String]]] = varName.map(Some(_)) | "_".!.map(_ => None)
    val typesP: P[Seq[PART[String]]]     = varName.rep(min = 1, sep = comment ~ "|" ~ comment)
    val typesDefP = (
      ":" ~ comment ~
        (typesP | (Index ~~ restMatchCaseInvalidP ~~ Index).map {
          case (start, _, end) => Seq(PART.INVALID(start, end, "the type for variable should be specified: `case varName: Type => expr`"))
        })
    ).?.map(_.getOrElse(List.empty))

    P(
      Index ~~ "case" ~ comment ~/ (
        (varDefP ~ comment ~ typesDefP) |
          (Index ~~ restMatchCaseInvalidP ~~ Index).map {
            case (start, _, end) =>
              (
                Some(PART.INVALID(start, end, "invalid syntax, should be: `case varName: Type => expr` or `case _ => expr`")),
                Seq.empty[PART[String]]
              )
          }
      ) ~ comment ~ "=>" ~/ baseExpr.?.log("baseExpr") ~~ Index
    ).map {
      case (start, (v, types), e, end) =>
        val exprStart = types.lastOption.orElse(v).fold(start)(_.end)
        MATCH_CASE(
          start = start,
          end = end,
          newVarName = v,
          types = types,
          expr = e.getOrElse(INVALID(exprStart, end, "expected expression"))
        )
    }
  }.log("matchCase")

  private lazy val matchP: P[EXPR] =
    P(Index ~~ "match" ~/ fallBackExpr ~ "{" ~ comment ~ NoCut(matchCaseP).rep(sep = comment) ~ comment ~ "}" ~~ Index)
      .map {
        case (start, _, Nil, end)   => INVALID(start, end, "pattern matching requires case branches")
        case (start, e, cases, end) => MATCH(start, end, e, cases.toList)
      }
      .log("match")

  private val accessP: P[(Int, Accessor, Int)] = P(
    ("" ~ comment ~ Index ~ "." ~/ comment ~ varName.map(Getter) ~~ Index) |
      (Index ~~ "(" ~/ functionCallArgs.map(Args) ~ ")" ~~ Index) |
      (Index ~~ "[" ~/ fallBackExpr.map(ListIndex) ~ "]" ~~ Index)
  )

  private val maybeAccessP: P[EXPR] =
    P(Index ~~ extractableAtom ~~ Index ~~ NoCut(accessP).rep)
      .map {
        case (start, obj, objEnd, accessors) =>
          accessors.foldLeft(obj) {
            case (e, (accessStart, a, accessEnd)) =>
              a match {
                case Getter(n) => GETTER(start, accessEnd, e, n)
                case Args(args) =>
                  e match {
                    case REF(_, _, functionName) => FUNCTION_CALL(start, accessEnd, functionName, args.toList)
                    case _                       => FUNCTION_CALL(start, accessEnd, PART.INVALID(start, objEnd, s"'$obj' is not a function name"), args.toList)
                  }
                case ListIndex(index) => FUNCTION_CALL(start, accessEnd, PART.VALID(accessStart, accessEnd, "getElement"), List(e, index))
              }
          }
      }
      .log("maybeAccess")

  private val refP2: P[REF] = P(varName2)
    .map { x =>
      REF(x.start, x.end, x)
    }
    .log("refP2")

  private val extractableAtom2: P[EXPR] = P(curlyBracesP | bracesP | refP2).log("extractableAtom2")

  private val maybeAccessP2: P[EXPR] =
    P(Index ~~ extractableAtom2 ~~ Index ~~ NoCut(accessP).rep)
      .map {
        case (start, obj, objEnd, accessors) =>
          accessors.foldLeft(obj) {
            case (e, (accessStart, a, accessEnd)) =>
              a match {
                case Getter(n) => GETTER(start, accessEnd, e, n)
                case Args(args) =>
                  e match {
                    case REF(_, _, functionName) => FUNCTION_CALL(start, accessEnd, functionName, args.toList)
                    case _                       => FUNCTION_CALL(start, accessEnd, PART.INVALID(start, objEnd, s"'$obj' is not a function name"), args.toList)
                  }
                case ListIndex(index) => FUNCTION_CALL(start, accessEnd, PART.VALID(accessStart, accessEnd, "getElement"), List(e, index))
              }
          }
      }
      .log("maybeAccess2")

  private val byteVectorP: P[EXPR] =
    P(Index ~~ "base" ~~ ("58" | "64").! ~~ "'" ~/ Pass ~~ CharPred(_ != '\'').repX.! ~~ "'" ~~ Index)
      .map {
        case (start, base, xs, end) =>
          val innerStart = start + 8
          val innerEnd   = end - 1
          val decoded = base match {
            case "58" => Global.base58Decode(xs)
            case "64" => Global.base64Decode(xs)
          }
          decoded match {
            case Left(err) => CONST_BYTEVECTOR(start, end, PART.INVALID(innerStart, innerEnd, err))
            case Right(r)  => CONST_BYTEVECTOR(start, end, PART.VALID(innerStart, innerEnd, ByteVector(r)))
          }
      }

  private val block: P[EXPR] = {
    // Hack to force parse of "\n". Otherwise it is treated as a separator
    val newLineSep = {
      val rawSep = '\n'
      val white = WhitespaceApi.Wrapper {
        import fastparse.all._
        NoTrace(CharIn((Whitespaces - rawSep).toSeq).rep)
      }

      import white._
      P("" ~ rawSep.toString.rep(min = 1))
    }

    P(
      Index ~~
        letP ~~
        Index ~~ (("" ~ ";").!.map(_ => true) | newLineSep.!.map(_ => true) | "".!.map(_ => false)) ~~ Index ~
        fallBackExpr ~~
        Index)
      .map {
        case (start, l, sepStart, sep, sepEnd, e, end) =>
          if (sep) BLOCK(start, end, l, e)
          else BLOCK(start, end, l, INVALID(sepStart, sepEnd, "can't find a separator. Did you mean ';' or '\\n' ?", Some(e)))
      }
  }

  private val baseAtom = comment ~
    P(ifP | NoCut(matchP) | byteVectorP | stringP | numberP | trueP | falseP | block | maybeAccessP) ~
    comment

  private lazy val baseExpr = P(binaryOp(baseAtom2, opsByPriority).log("binaryOp") | baseAtom)

  private val baseAtom2 = comment ~
    P(ifP | NoCut(matchP) | byteVectorP | stringP | numberP | trueP | falseP | block | maybeAccessP2) ~
    comment

  private def invalidWithFallback(fallBackExpr: P[EXPR]): P[INVALID] =
    P(Index ~~ AnyChars(1).! ~~ Index ~~ fallBackExpr.?)
      .map {
        case (start, xs, end, next) => foldInvalid(start, end, xs, next)
      }
      .log("invalidWithFallback")

  private lazy val fallBackExpr = {
    val fallBackAtom = P(baseAtom2 | invalid).log("fallBackAtom")
    P(binaryOp(fallBackAtom, opsByPriority) | fallBackAtom)
  }

  private val baseAtom3 = {
    val fallBackAtom = P(baseAtom2 | invalid).log("fallBackAtom")
//    val b1 = P("(" ~ fallBackExpr ~ ")")
//    val b2 = P("{" ~ fallBackExpr ~ "}")

    binaryOp(fallBackAtom, opsByPriority)
  }

  private def binaryOp(atom: P[EXPR], rest: List[BinaryOperation]): P[EXPR] = rest match {
    case Nil => unaryOp(atom, unaryOps).log("unaryOp")
    case kind :: restOps =>
      val operand = binaryOp(atom, restOps)
      P(
        Index ~~ operand ~ (kind.parser.!.map(_ => kind) ~ (
          NoCut(operand).log("rhs") | Index
            .map(i => INVALID(i, i, "expected a second operator", None))
            .log("index")
        )).rep() ~~ Index).map {
        case (start, left: EXPR, r: Seq[(BinaryOperation, EXPR)], end) =>
          r.foldLeft(left) { case (acc, (currKind, currOperand)) => currKind.expr(start, end, acc, currOperand) }
      }
  }

  def unaryOp(atom: P[EXPR], ops: List[UnaryOperation]): P[EXPR] = ops.foldRight(atom) {
    case (op, acc) =>
      (Index ~~ (op.parser.map(_ => ()) ~ P(unaryOp(atom, ops))) ~~ Index)
        .map { // | invalid
          case (start, expr, end) => op.expr(start, end, expr)
        } | acc
  }

  def apply(str: String): core.Parsed[Seq[EXPR], Char, String] = P(Start ~ baseAtom3.rep(min = 1) ~ End).parse(str)
}
