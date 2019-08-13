package com.wavesplatform.lang.v1.parser

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import com.wavesplatform.lang.v1.parser.Expressions.PART.VALID
import com.wavesplatform.lang.v1.parser.Expressions._
import com.wavesplatform.lang.v1.parser.UnaryOperation._
import fastparse.{WhitespaceApi, core, noApi}

object Parser {

  private val Global                 = com.wavesplatform.lang.hacks.Global // Hack for IDEA
  private val Whitespaces: Set[Char] = " \t\r\n".toSet

  private val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(Whitespaces.toSeq).rep)
  }

  import White._
  import fastparse.noApi._

  val keywords           = Set("let", "base58", "base64", "true", "false", "if", "then", "else", "match", "case", "func")
  val lowerChar          = CharIn('a' to 'z')
  val upperChar          = CharIn('A' to 'Z')
  val char               = lowerChar | upperChar
  val digit              = CharIn('0' to '9')
  val unicodeSymbolP     = P("\\u" ~/ Pass ~~ (char | digit).repX(min = 0, max = 4))
  val notEndOfString     = CharPred(_ != '\"')
  val specialSymbols     = P("\\" ~~ AnyChar)
  val comment: P[Unit]   = P("#" ~~ CharPred(_ != '\n').repX).rep.map(_ => ())
  val directive: P[Unit] = P("{-#" ~ CharPred(el => el != '\n' && el != '#').rep ~ "#-}").rep(sep = comment).map(_ => ())

  val unusedText = comment ~ directive ~ comment

  val escapedUnicodeSymbolP: P[(Int, String, Int)] = P(Index ~~ (NoCut(unicodeSymbolP) | specialSymbols).! ~~ Index)
  val stringP: P[EXPR] = P(Index ~~ "\"" ~/ Pass ~~ (escapedUnicodeSymbolP | notEndOfString).!.repX ~~ "\"" ~~ Index)
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
                case 'b'  => "\b"
                case 'f'  => "\f"
                case 'n'  => "\n"
                case 'r'  => "\r"
                case 't'  => "\t"
                case '\\' => "\\"
                case '"'  => "\""
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
          if (errors.isEmpty) PART.VALID(Pos(start + 1, end - 1), consumedString.toString)
          else PART.INVALID(Pos(start + 1, end - 1), errors.mkString(";"))
        (Pos(start, end), r)
    }
    .map(Function.tupled(CONST_STRING))

  val correctVarName: P[PART[String]] = (Index ~~ (char ~~ (digit | char).repX()).! ~~ Index)
    .filter { case (_, x, _) => !keywords.contains(x) }
    .map { case (start, x, end) => PART.VALID(Pos(start, end), x) }

  val anyVarName: P[PART[String]] = (Index ~~ (char ~~ (digit | char).repX()).! ~~ Index).map {
    case (start, x, end) =>
      if (keywords.contains(x)) PART.INVALID(Pos(start, end), s"keywords are restricted: $x")
      else PART.VALID(Pos(start, end), x)
  }

  val invalid: P[INVALID] = {
    val White = WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace("")
    }

    import White._
    P(Index ~~ CharPred(_ != '\n').rep(min = 1) ~~ Index)
      .map {
        case (start, end) => INVALID(Pos(start, end), "can't parse the expression")
      }
  }

  val border: P[Unit] = CharIn(" \t\n\r({")

  val numberP: P[CONST_LONG] =
    P(Index ~~ (CharIn("+-").? ~~ digit.repX(min = 1)).! ~~ ("_" ~~ digit.repX(min = 1).!).repX(min = 0) ~~ Index)
      .map({ case (start, x1, x2, end) => CONST_LONG(Pos(start, end), x2.foldLeft(x1)(_ ++ _).toLong) })

  val trueP: P[TRUE]        = P(Index ~~ "true".! ~~ !(char | digit) ~~ Index).map { case (start, _, end) => TRUE(Pos(start, end)) }
  val falseP: P[FALSE]      = P(Index ~~ "false".! ~~ !(char | digit) ~~ Index).map { case (start, _, end) => FALSE(Pos(start, end)) }
  val bracesP: P[EXPR]      = P("(" ~ baseExpr ~ ")")
  val curlyBracesP: P[EXPR] = P("{" ~ baseExpr ~ "}")

  val refP: P[REF] = P(correctVarName).map { x =>
    REF(Pos(x.position.start, x.position.end), x)
  }

  val ifP: P[IF] = {
    def optionalPart(keyword: String, branch: String): P[EXPR] = (Index ~ (keyword ~/ Index ~ baseExpr.?).?).map {
      case (ifTruePos, ifTrueRaw) =>
        ifTrueRaw
          .map { case (pos, expr) => expr.getOrElse(INVALID(Pos(pos, pos), s"expected a $branch branch's expression")) }
          .getOrElse(INVALID(Pos(ifTruePos, ifTruePos), s"expected a $branch branch"))
    }

    val thenPart = optionalPart("then", "true")
    val elsePart = optionalPart("else", "false")

    P(Index ~~ "if" ~~ &(border) ~/ Index ~ baseExpr.? ~ thenPart ~ elsePart ~~ Index).map {
      case (start, condPos, condRaw, ifTrue, ifFalse, end) =>
        val cond = condRaw.getOrElse(INVALID(Pos(condPos, condPos), "expected a condition"))
        IF(Pos(start, end), cond, ifTrue, ifFalse)
    } |
      P(Index ~~ "then" ~~ &(border) ~/ Index ~ baseExpr.? ~ elsePart ~~ Index).map {
        case (start, ifTrueExprPos, ifTrueRaw, ifFalse, end) =>
          val ifTrue = ifTrueRaw.getOrElse(INVALID(Pos(ifTrueExprPos, ifTrueExprPos), "expected a true branch's expression"))
          IF(Pos(start, end), INVALID(Pos(start, start), "expected a condition"), ifTrue, ifFalse)
      } |
      P(Index ~~ "else" ~~ &(border) ~/ Index ~ baseExpr.? ~~ Index).map {
        case (start, ifFalseExprPos, ifFalseRaw, end) =>
          val ifFalse = ifFalseRaw.getOrElse(INVALID(Pos(ifFalseExprPos, ifFalseExprPos), "expected a false branch's expression"))
          IF(Pos(start, end), INVALID(Pos(start, start), "expected a condition"), INVALID(Pos(start, start), "expected a true branch"), ifFalse)
      }
  }

  val functionCallArgs: P[Seq[EXPR]] = comment ~ baseExpr.rep(sep = comment ~ "," ~ comment) ~ comment

  val maybeFunctionCallP: P[EXPR] = (Index ~~ refP ~~ P("(" ~/ functionCallArgs ~ ")").? ~~ Index).map {
    case (start, REF(_, functionName), Some(args), accessEnd) => FUNCTION_CALL(Pos(start, accessEnd), functionName, args.toList)
    case (_, id, None, _)                                     => id
  }

  val foldP: P[EXPR] = (Index ~~ P("FOLD<") ~~ digit.repX(min = 1).! ~~ ">(" ~/ baseExpr ~ "," ~ baseExpr ~ "," ~ refP ~ ")" ~~ Index)
    .map { case (start, limit, list, acc, f, end) =>
      Macro.unwrapFold(Pos(start, end), limit.toInt, list, acc, f)
    }

  val list: P[EXPR] = (Index ~~ P("[") ~ functionCallArgs ~ P("]") ~~ Index).map {
    case (s, e, f) =>
      val pos = Pos(s, f)
      e.foldRight(REF(pos, PART.VALID(pos, "nil")): EXPR) { (v, l) =>
        FUNCTION_CALL(pos, PART.VALID(pos, "cons"), List(v, l))
      }
  }

  val extractableAtom: P[EXPR] = P(
    curlyBracesP | bracesP |
      byteVectorP | stringP | numberP | trueP | falseP | list |
      maybeFunctionCallP)

  abstract class Accessor
  case class Method(name: PART[String], args: Seq[EXPR]) extends Accessor
  case class Getter(name: PART[String])                  extends Accessor
  case class ListIndex(index: EXPR)                      extends Accessor

  val typesP: P[Seq[PART[String]]] = anyVarName.rep(min = 1, sep = comment ~ "|" ~ comment)
  val genericTypesP: P[Seq[(PART[String], Option[PART[String]])]] =
    (anyVarName ~~ ("[" ~~ anyVarName ~~ "]").?).rep(min = 1, sep = comment ~ "|" ~ comment)

  val funcP: P[FUNC] = {
    val funcname    = anyVarName
    val argWithType = anyVarName ~ ":" ~ genericTypesP ~ comment
    val args        = "(" ~ comment ~ argWithType.rep(sep = "," ~ comment) ~ ")" ~ comment
    val funcHeader  = Index ~~ "func" ~ funcname ~ comment ~ args ~ "=" ~ P(singleBaseExpr | ("{" ~ baseExpr ~ "}")) ~~ Index
    funcHeader.map {
      case (start, name, args, expr, end) => FUNC(Pos(start, end), name, args, expr)
    }
  }

  val annotationP: P[ANNOTATION] = (Index ~~ "@" ~ anyVarName ~ comment ~ "(" ~ comment ~ anyVarName.rep(sep = ",") ~ comment ~ ")" ~~ Index).map {
    case (start, name: PART[String], args: Seq[PART[String]], end) => ANNOTATION(Pos(start, end), name, args)
  }

  val annotatedFunc: P[ANNOTATEDFUNC] = (Index ~~ annotationP.rep(min = 1) ~ comment ~ funcP ~~ Index).map {
    case (start, as, f, end) => ANNOTATEDFUNC(Pos(start, end), as, f)
  }

  val matchCaseP: P[MATCH_CASE] = {
    val restMatchCaseInvalidP: P[String] = P((!"=>" ~~ AnyChars(1).!).repX.map(_.mkString))
    val varDefP: P[Option[PART[String]]] = anyVarName.map(Some(_)) | "_".!.map(_ => None)

    val typesDefP = (
      ":" ~ comment ~
        (typesP | (Index ~~ restMatchCaseInvalidP ~~ Index).map {
          case (start, _, end) => Seq(PART.INVALID(Pos(start, end), "the type for variable should be specified: `case varName: Type => expr`"))
        })
    ).?.map(_.getOrElse(List.empty))

    P(
      Index ~~ "case" ~~ &(border) ~ comment ~/ (
        (varDefP ~ comment ~ typesDefP) |
          (Index ~~ restMatchCaseInvalidP ~~ Index).map {
            case (start, _, end) =>
              (
                Some(PART.INVALID(Pos(start, end), "invalid syntax, should be: `case varName: Type => expr` or `case _ => expr`")),
                Seq.empty[PART[String]]
              )
          }
      ) ~ comment ~ "=>" ~/ baseExpr.? ~~ Index
    ).map {
      case (start, (v, types), e, end) =>
        val exprStart = types.lastOption.orElse(v).fold(start)(_.position.end)
        MATCH_CASE(
          Pos(start, end),
          newVarName = v,
          types = types,
          expr = e.getOrElse(INVALID(Pos(exprStart, end), "expected expression"))
        )
    }
  }

  lazy val matchP: P[EXPR] =
    P(Index ~~ "match" ~~ &(border) ~/ baseExpr ~ "{" ~ comment ~ matchCaseP.rep(sep = comment) ~ comment ~ "}" ~~ Index)
      .map {
        case (start, _, Nil, end)   => INVALID(Pos(start, end), "pattern matching requires case branches")
        case (start, e, cases, end) => MATCH(Pos(start, end), e, cases.toList)
      }

  val accessP: P[(Int, Accessor, Int)] = P(
    (("" ~ comment ~ Index ~ "." ~/ comment ~ (anyVarName.map(Getter) ~/ comment ~~ ("(" ~/ comment ~ functionCallArgs ~/ comment ~ ")").?).map {
      case ((g @ Getter(name)), args) => args.fold(g: Accessor)(a => Method(name, a))
    }) ~~ Index) | (Index ~~ "[" ~/ baseExpr.map(ListIndex) ~ "]" ~~ Index)
  )

  val maybeAccessP: P[EXPR] =
    P(Index ~~ extractableAtom ~~ Index ~~ NoCut(accessP).rep)
      .map {
        case (start, obj, objEnd, accessors) =>
          accessors.foldLeft(obj) {
            case (e, (accessStart, a, accessEnd)) =>
              a match {
                case Getter(n)        => GETTER(Pos(start, accessEnd), e, n)
                case Method(n, args)  => FUNCTION_CALL(Pos(start, accessEnd), n, (e :: args.toList))
                case ListIndex(index) => FUNCTION_CALL(Pos(start, accessEnd), PART.VALID(Pos(accessStart, accessEnd), "getElement"), List(e, index))
              }
          }
      }

  val byteVectorP: P[EXPR] =
    P(Index ~~ "base" ~~ ("58" | "64" | "16").! ~~ "'" ~/ Pass ~~ CharPred(_ != '\'').repX.! ~~ "'" ~~ Index)
      .map {
        case (start, base, xs, end) =>
          val innerStart = start + 8
          val innerEnd   = end - 1
          val decoded = base match {
            case "16" => Global.base16Decode(xs)
            case "58" => Global.base58Decode(xs)
            case "64" => Global.base64Decode(xs)
          }
          decoded match {
            case Left(err) => CONST_BYTESTR(Pos(start, end), PART.INVALID(Pos(innerStart, innerEnd), err))
            case Right(r)  => CONST_BYTESTR(Pos(start, end), PART.VALID(Pos(innerStart, innerEnd), ByteStr(r)))
          }
      }

  val letP: P[LET] =
    P(Index ~~ "let" ~~ &(CharIn(" \t\n\r")) ~/ comment ~ Index ~ anyVarName.? ~ comment ~ Index ~ ("=" ~/ Index ~ baseExpr.?).? ~~ Index)
      .map {
        case (start, namePos, nameRaw, valuePos, valueRaw, end) =>
          val name = nameRaw.getOrElse(PART.INVALID(Pos(namePos, namePos), "expected a variable's name"))
          val value = valueRaw
            .map { case (pos, expr) => expr.getOrElse(INVALID(Pos(pos, pos), "expected a value's expression")) }
            .getOrElse(INVALID(Pos(valuePos, valuePos), "expected a value"))
          LET(Pos(start, end), name, value, Seq.empty)
      }

  val block: P[EXPR] = blockOr(INVALID(_, "expected ';'"))

  private def blockOr(otherExpr: Pos => EXPR): P[EXPR] = {
    val declaration = letP | funcP

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
        declaration.rep(min = 1) ~/
        Pass ~~
        (
          ("" ~ ";") ~/ (baseExpr | invalid).? |
            newLineSep ~/ (baseExpr | invalid).? |
            (Index ~~ CharPred(_ != '\n').repX).map(pos => Some(otherExpr(Pos(pos, pos))))
        ) ~~
        Index
    ).map {
      case (start, ls, body, end) => {
        ls.reverse
          .foldLeft(body.getOrElse(INVALID(Pos(end, end), "expected a body"))) { (acc, l) =>
            BLOCK(Pos(start, end), l, acc)
          }
      }
    }
  }

  def baseAtom(ep: P[EXPR]) = comment ~
    P(foldP | ifP | matchP | ep | maybeAccessP) ~
    comment

  lazy val baseExpr = P(binaryOp(baseAtom(block), opsByPriority))

  lazy val blockOrDecl = baseAtom(blockOr(p => REF(p, VALID(p, "unit"))))
  lazy val baseExprOrDecl = P(binaryOp(baseAtom(blockOrDecl), opsByPriority))

  val singleBaseAtom = comment ~
    P(ifP | matchP | maybeAccessP) ~
    comment

  val singleBaseExpr = P(binaryOp(singleBaseAtom, opsByPriority))

  val declaration = P(letP | funcP)

  def revp[A, B](l: A, s: Seq[(B, A)], o: Seq[(A, B)] = Seq.empty): (Seq[(A, B)], A) = {
    s.foldLeft((o, l)) { (acc, op) =>
      (acc, op) match { case ((o, l), (b, a)) => ((l, b) +: o) -> a }
    }
  }

  def binaryOp(atom: P[EXPR], rest: List[Either[List[BinaryOperation], List[BinaryOperation]]]): P[EXPR] = rest match {
    case Nil => unaryOp(atom, unaryOps)
    case Left(kinds) :: restOps =>
      val operand = binaryOp(atom, restOps)
      val kind    = kinds.map(_.parser).reduce((pl, pr) => P(pl | pr))
      P(Index ~~ operand ~ P(kind ~ (NoCut(operand) | Index.map(i => INVALID(Pos(i, i), "expected a second operator")))).rep).map {
        case (start, left: EXPR, r: Seq[(BinaryOperation, EXPR)]) =>
          r.foldLeft(left) { case (acc, (currKind, currOperand)) => currKind.expr(start, currOperand.position.end, acc, currOperand) }
      }
    case Right(kinds) :: restOps =>
      val operand = binaryOp(atom, restOps)
      val kind    = kinds.map(_.parser).reduce((pl, pr) => P(pl | pr))
      P(Index ~~ operand ~ P(kind ~ (NoCut(operand) | Index.map(i => INVALID(Pos(i, i), "expected a second operator")))).rep).map {
        case (start, left: EXPR, r: Seq[(BinaryOperation, EXPR)]) =>
          val (ops, s) = revp(left, r)
          ops.foldLeft(s) { case (acc, (currOperand, currKind)) => currKind.expr(start, currOperand.position.end, currOperand, acc) }
      }
  }

  def unaryOp(atom: P[EXPR], ops: List[UnaryOperation]): P[EXPR] = ops.foldRight(atom) {
    case (op, acc) =>
      (Index ~~ op.parser.map(_ => ()) ~ P(unaryOp(atom, ops)) ~~ Index).map {
        case (start, expr, end) => op.expr(start, end, expr)
      } | acc
  }

  def parseExpr(str: String): core.Parsed[EXPR, Char, String] = P(Start ~ unusedText ~ (baseExpr | invalid) ~ End).parse(str)

  def parseExprOrDecl(str: String): core.Parsed[EXPR, Char, String] =
    P(Start ~ unusedText ~ (baseExprOrDecl | invalid) ~ End).parse(str)

  def parseContract(str: String): core.Parsed[DAPP, Char, String] =
    P(Start ~ unusedText ~ (declaration.rep) ~ comment ~ (annotatedFunc.rep) ~ !declaration.rep(min = 1) ~ End ~~ Index)
      .map {
        case (ds, fs, end) => DAPP(Pos(0, end), ds.toList, fs.toList)
      }
      .parse(str) match {
      case (f @ Parsed.Failure(m, o, e)) if (m.toString.startsWith("!(declaration.rep(")) =>
        Parsed.Failure(s"Local functions should be defined before @Callable one: ${str.substring(o)}", o, e)
      case s => s
    }
}
