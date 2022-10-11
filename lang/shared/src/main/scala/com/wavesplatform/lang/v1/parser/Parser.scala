package com.wavesplatform.lang.v1.parser

import cats.instances.either.*
import cats.instances.list.*
import cats.syntax.either.*
import cats.syntax.traverse.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.MaxListLengthV4
import com.wavesplatform.lang.v1.parser.BinaryOperation.*
import com.wavesplatform.lang.v1.parser.Expressions.*
import com.wavesplatform.lang.v1.parser.Expressions.PART.VALID
import com.wavesplatform.lang.v1.parser.UnaryOperation.*
import com.wavesplatform.lang.v1.{ContractLimits, compiler}
import fastparse.*
import fastparse.MultiLineWhitespace.*
import fastparse.Parsed.Failure

import scala.annotation.tailrec

object Parser {

  private val Global                                        = com.wavesplatform.lang.hacks.Global // Hack for IDEA
  implicit def hack(p: fastparse.P[Any]): fastparse.P[Unit] = p.map(_ => ())

  def keywords                 = Set("let", "strict", "base58", "base64", "true", "false", "if", "then", "else", "match", "case", "func")
  def lowerChar[A: P]          = CharIn("a-z")
  def upperChar[A: P]          = CharIn("A-Z")
  def char[A: P]               = lowerChar | upperChar
  def digit[A: P]              = CharIn("0-9")
  def spaces[A: P]             = CharIn(" \t\n\r")
  def unicodeSymbolP[A: P]     = P("\\u" ~/ Pass ~~ (char | digit).repX(0, "", 4))
  def notEndOfString[A: P]     = CharPred(_ != '\"')
  def specialSymbols[A: P]     = P("\\" ~~ AnyChar)
  def comment[A: P]: P[Unit]   = P("#" ~~ CharPred(_ != '\n').repX).rep.map(_ => ())
  def directive[A: P]: P[Unit] = P("{-#" ~ CharPred(el => el != '\n' && el != '#').rep ~ "#-}").rep(0, comment).map(_ => ())

  def unusedText[A: P] = comment ~ directive ~ comment

  def escapedUnicodeSymbolP[A: P]: P[(Int, String, Int)] = P(Index ~~ (NoCut(unicodeSymbolP) | specialSymbols).! ~~ Index)
  def stringP[A: P]: P[EXPR] =
    P(Index ~~ "\"" ~/ Pass ~~ (escapedUnicodeSymbolP | notEndOfString).!.repX ~~ "\"" ~~ Index)
      .map { case (start, xs, end) =>
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
      .map(posAndVal => CONST_STRING(posAndVal._1, posAndVal._2))

  def correctVarName[A: P]: P[PART[String]] =
    (Index ~~ (char ~~ (digit | char).repX()).! ~~ Index)
      .filter { case (_, x, _) => !keywords.contains(x) }
      .map { case (start, x, end) => PART.VALID(Pos(start, end), x) }

  def checkedUnderscore[A](implicit p: P[A]): P[Unit] =
    ("_" ~~/ !"_".repX(1)).?.opaque("not more than 1 underscore in a row")

  def declNameP[A: P]: P[Unit] = checkedUnderscore ~~ char ~~ ("_".? ~~ (digit | char)).repX() ~~ checkedUnderscore

  def correctLFunName[A: P]: P[PART[String]] =
    (Index ~~ declNameP.! ~~ Index)
      .filter { case (_, x, _) => !keywords.contains(x) }
      .map { case (start, x, end) => PART.VALID(Pos(start, end), x) }

  def genericVarName(namePx: fastparse.P[Any] => P[Unit])(implicit c: fastparse.P[Any]): P[PART[String]] = {
    def nameP(implicit c: fastparse.P[Any]) = namePx(c)
    (Index ~~ nameP.! ~~ Index).map { case (start, x, end) =>
      if (keywords.contains(x)) PART.INVALID(Pos(start, end), s"keywords are restricted: $x")
      else PART.VALID(Pos(start, end), x)
    }
  }

  def anyVarName(implicit c: fastparse.P[Any]): P[PART[String]] = {
    def nameP(implicit c: fastparse.P[Any]): P[Unit] = declNameP
    genericVarName(nameP(_))
  }

  def invalid[A: P]: P[INVALID] = {
    import fastparse.NoWhitespace.*
    P(Index ~~ CharPred(_ != '\n').rep(1) ~~ Index)
      .map { case (start, end) =>
        INVALID(Pos(start, end), "can't parse the expression")
      }
  }

  def border[A: P]: P[Unit] = CharIn(" \t\n\r({")

  def numberP[A: P]: P[CONST_LONG] =
    P(Index ~~ (CharIn("+\\-").? ~~ digit.repX(1)).! ~~ ("_" ~~ digit.repX(1).!).repX(0) ~~ Index)
      .map({ case (start, x1, x2, end) => CONST_LONG(Pos(start, end), x2.foldLeft(x1)(_ ++ _).toLong) })

  def trueP[A: P]: P[TRUE]        = P(Index ~~ "true".! ~~ !(char | digit) ~~ Index).map { case (start, _, end) => TRUE(Pos(start, end)) }
  def falseP[A: P]: P[FALSE]      = P(Index ~~ "false".! ~~ !(char | digit) ~~ Index).map { case (start, _, end) => FALSE(Pos(start, end)) }
  def curlyBracesP[A: P]: P[EXPR] = P("{" ~ baseExpr ~ "}")

  def lfunP[A: P]: P[REF] = P(correctLFunName).map { x =>
    REF(Pos(x.position.start, x.position.end), x)
  }

  def ifP[A: P]: P[IF] = {
    def optionalPart(keyword: String, branch: String): P[EXPR] = (Index ~ (keyword ~/ Index ~ baseExpr.?).?).map { case (ifTruePos, ifTrueRaw) =>
      ifTrueRaw
        .map { case (pos, expr) => expr.getOrElse(INVALID(Pos(pos, pos), s"expected a $branch branch's expression")) }
        .getOrElse(INVALID(Pos(ifTruePos, ifTruePos), s"expected a $branch branch"))
    }

    def thenPart[AA: P] = optionalPart("then", "true")
    def elsePart[AA: P] = optionalPart("else", "false")

    P(Index ~~ "if" ~~ &(border) ~/ Index ~ baseExpr.? ~ thenPart ~ elsePart ~~ Index).map { case (start, condPos, condRaw, ifTrue, ifFalse, end) =>
      val cond = condRaw.getOrElse(INVALID(Pos(condPos, condPos), "expected a condition"))
      IF(Pos(start, end), cond, ifTrue, ifFalse)
    } |
      P(Index ~~ "then" ~~ &(border) ~/ Index ~ baseExpr.? ~ elsePart ~~ Index).map { case (start, ifTrueExprPos, ifTrueRaw, ifFalse, end) =>
        val ifTrue = ifTrueRaw.getOrElse(INVALID(Pos(ifTrueExprPos, ifTrueExprPos), "expected a true branch's expression"))
        IF(Pos(start, end), INVALID(Pos(start, start), "expected a condition"), ifTrue, ifFalse)
      } |
      P(Index ~~ "else" ~~ &(border) ~/ Index ~ baseExpr.? ~~ Index).map { case (start, ifFalseExprPos, ifFalseRaw, end) =>
        val ifFalse = ifFalseRaw.getOrElse(INVALID(Pos(ifFalseExprPos, ifFalseExprPos), "expected a false branch's expression"))
        IF(Pos(start, end), INVALID(Pos(start, start), "expected a condition"), INVALID(Pos(start, start), "expected a true branch"), ifFalse)
      }
  }

  def functionCallArgs[A: P]: P[Seq[EXPR]] = comment ~ baseExpr.rep(0, comment ~ "," ~ comment) ~ comment

  def maybeFunctionCallP[A: P]: P[EXPR] = (Index ~~ lfunP ~~ P("(" ~/ functionCallArgs ~ ")").? ~~ Index).map {
    case (start, REF(_, functionName, _, _), Some(args), accessEnd) => FUNCTION_CALL(Pos(start, accessEnd), functionName, args.toList)
    case (_, id, None, _)                                           => id
  }

  def foldMacroP[A: P]: P[EXPR] =
    (Index ~~ P("FOLD<") ~~ Index ~~ digit.repX(1).! ~~ Index ~~ ">(" ~/ baseExpr ~ "," ~ baseExpr ~ "," ~ lfunP ~ ")" ~~ Index)
      .map { case (start, limStart, limit, limEnd, list, acc, f, end) =>
        val lim = limit.toInt
        if (lim < 1)
          INVALID(Pos(limStart, limEnd), "FOLD limit should be natural")
        else if (lim > MaxListLengthV4)
          INVALID(Pos(limStart, limEnd), s"List size limit in FOLD is too big, $lim must be less or equal $MaxListLengthV4")
        else
          FOLD(Pos(start, end), lim, list, acc, f)
      }

  def list[A: P]: P[EXPR] = (Index ~~ P("[") ~ functionCallArgs ~ P("]") ~~ Index).map { case (s, e, f) =>
    val pos = Pos(s, f)
    e.foldRight(REF(pos, PART.VALID(pos, "nil")): EXPR) { (v, l) =>
      FUNCTION_CALL(pos, PART.VALID(pos, "cons"), List(v, l))
    }
  }

  def bracedArgs[A: P]: P[Seq[EXPR]] =
    comment ~ baseExpr.rep(
      sep = comment ~ "," ~ comment,
      max = ContractLimits.MaxTupleSize
    ) ~ comment

  def bracesOrTuple[A: P]: P[EXPR] = (Index ~~ P("(") ~ bracedArgs ~ P(")") ~~ Index).map {
    case (_, Seq(expr), _) => expr
    case (s, elements, f) =>
      FUNCTION_CALL(
        Pos(s, f),
        PART.VALID(Pos(s, f), s"${compiler.TuplePrefix}${elements.length}"),
        elements.toList
      )
  }

  def extractableAtom[A: P]: P[EXPR] = P(
    curlyBracesP | bracesOrTuple |
      byteVectorP | stringP | numberP | trueP | falseP | list |
      maybeFunctionCallP
  )

  sealed trait Accessor
  case class Method(name: PART[String], args: Seq[EXPR])     extends Accessor
  case class Getter(name: PART[String])                      extends Accessor
  case class ListIndex(index: EXPR)                          extends Accessor
  case class GenericMethod(name: PART[String], `type`: Type) extends Accessor

  object GenericMethod {
    val ExactAs = "exactAs"
    val As      = "as"

    val KnownMethods: Set[String] = Set(ExactAs, As)
  }

  def singleTypeP[A: P]: P[Single] = (anyVarName ~~ ("[" ~~ Index ~ unionTypeP ~ Index ~~ "]").?).map { case (t, param) =>
    Single(t, param.map { case (start, param, end) => VALID(Pos(start, end), param) })
  }

  def unionTypeP[A: P]: P[Type] =
    (Index ~ P("Any") ~ Index).map { case (start, end) => AnyType(Pos(start, end)) } | P(singleTypeP | tupleTypeP)
      .rep(1, comment ~ "|" ~ comment)
      .map(Union.apply)
  def tupleTypeP[A: P]: P[Tuple] =
    ("(" ~
      P(unionTypeP).rep(
        ContractLimits.MinTupleSize,
        comment ~ "," ~ comment,
        ContractLimits.MaxTupleSize
      )
      ~ ")")
      .map(Tuple)

  def funcP(implicit c: fastparse.P[Any]): P[FUNC] = {
    def funcName       = anyVarName
    def argWithType    = anyVarName ~/ ":" ~ unionTypeP ~ comment
    def args(min: Int) = "(" ~ comment ~ argWithType.rep(min, "," ~ comment) ~ ")" ~ comment
    def funcBody       = P(singleBaseExpr | ("{" ~ comment ~ baseExpr ~ "}"))
    def correctFunc    = Index ~~ "func" ~~ &(spaces) ~ funcName ~ comment ~/ args(min = 0) ~ "=" ~ funcBody ~~ Index
    def noKeyword      = (NoCut(funcName) ~ comment ~ NoCut(args(min = 1)) ~/ "=".? ~ funcBody.?) ~~ Fail
    def noKeywordP     = noKeyword.asInstanceOf[P[Nothing]].opaque("'func' keyword")
    (noKeywordP | correctFunc)
      .map { case (start, name, args, expr, end) => FUNC(Pos(start, end), expr, name, args) }
  }

  def annotationP[A: P]: P[ANNOTATION] = (Index ~~ "@" ~ anyVarName ~ comment ~ "(" ~ comment ~ anyVarName.rep(0, ",") ~ comment ~ ")" ~~ Index).map {
    case (start, name: PART[String], args: Seq[PART[String]], end) => ANNOTATION(Pos(start, end), name, args)
  }

  def annotatedFunc[A: P]: P[ANNOTATEDFUNC] = (Index ~~ annotationP.rep(1) ~ comment ~ funcP ~~ Index).map { case (start, as, f, end) =>
    ANNOTATEDFUNC(Pos(start, end), as, f)
  }

  def matchCaseP(implicit c: fastparse.P[Any]): P[MATCH_CASE] = {
    def checkForGenericAndGetLastPos(t: Type): Either[INVALID, Option[Pos]] =
      t match {
        case Single(VALID(position, Type.ListTypeName), Some(VALID(_, AnyType(_)))) => Right(Some(position))
        case Single(name, parameter) =>
          parameter
            .toLeft(Some(name.position))
            .leftMap {
              case VALID(position, _)              => INVALID(position, s"Unexpected generic match type: only List[Any] is allowed")
              case PART.INVALID(position, message) => INVALID(position, message)
            }
        case Union(types) =>
          types
            .traverse(checkForGenericAndGetLastPos)
            .map(_.lastOption.flatten)
        case Tuple(types) =>
          types
            .traverse(checkForGenericAndGetLastPos)
            .map(_.lastOption.flatten)
        case AnyType(pos) => Right(Some(pos))
      }

    def restMatchCaseInvalidP(implicit c: fastparse.P[Any]): P[String] = P((!P("=>") ~~ AnyChar.!).repX.map(_.mkString))
    def varDefP(implicit c: fastparse.P[Any]): P[Option[PART[String]]] = (NoCut(anyVarName) ~~ !("'" | "(")).map(Some(_)) | P("_").!.map(_ => None)

    def typesDefP(implicit c: fastparse.P[Any]) =
      (
        ":" ~ comment ~
          (unionTypeP | (Index ~~ restMatchCaseInvalidP ~~ Index).map { case (start, _, end) =>
            Single(PART.INVALID(Pos(start, end), "the type for variable should be specified: `case varName: Type => expr`"), None)
          })
      ).?.map(_.getOrElse(Union(Seq())))

    def pattern(implicit c: fastparse.P[Any]): P[Pattern] =
      (varDefP ~ comment ~ typesDefP).map { case (v, t) => TypedVar(v, t) } |
        (Index ~ "(" ~ pattern.rep(min = 2, sep = ",") ~ ")" ~ Index).map(p => TuplePat(p._2, Pos(p._1, p._3))) |
        (Index ~ anyVarName ~ "(" ~ (anyVarName ~ "=" ~ pattern).rep(sep = ",") ~ ")" ~ Index)
          .map(p => ObjPat(p._3.map(kp => (PART.toOption(kp._1).get, kp._2)).toMap, Single(p._2, None), Pos(p._1, p._4))) |
        (Index ~ baseExpr.rep(min = 1, sep = "|") ~ Index).map(p => ConstsPat(p._2, Pos(p._1, p._3)))

    def checkPattern(p: Pattern): Either[INVALID, Option[Pos]] =
      p match {
        case TypedVar(_, t)    => checkForGenericAndGetLastPos(t)
        case ConstsPat(_, pos) => Right(Some(pos))
        case TuplePat(ps, pos) =>
          ps.toList traverse checkPattern map { _ =>
            Some(pos)
          }
        case ObjPat(ps, _, pos) =>
          ps.values.toList traverse checkPattern map { _ =>
            Some(pos)
          }
      }

    P(
      Index ~~ "case" ~~ &(border) ~ comment ~/ (
        pattern |
          (Index ~~ restMatchCaseInvalidP ~~ Index).map { case (start, _, end) =>
            TypedVar(
              Some(PART.INVALID(Pos(start, end), "invalid syntax, should be: `case varName: Type => expr` or `case _ => expr`")),
              Union(Seq())
            )
          }
      ) ~ comment ~ "=>" ~/ baseExpr.? ~~ Index
    ).map { case (caseStart, p, e, end) =>
      checkPattern(p)
        .fold(
          error => MATCH_CASE(error.position, pattern = p, expr = error),
          { pos =>
            val cPos      = Pos(caseStart, end)
            val exprStart = pos.fold(caseStart)(_.end)
            MATCH_CASE(
              cPos,
              pattern = p,
              expr = e.getOrElse(INVALID(Pos(exprStart, end), "expected expression"))
            )
          }
        )
    }
  }

  def matchP[A: P]: P[EXPR] =
    P(Index ~~ "match" ~~ &(border) ~/ baseExpr ~ "{" ~ comment ~ matchCaseP.rep(0, comment) ~ comment ~ "}" ~~ Index)
      .map {
        case (start, _, Nil, end)   => INVALID(Pos(start, end), "pattern matching requires case branches")
        case (start, e, cases, end) => MATCH(Pos(start, end), e, cases.toList)
      }

  def accessOrName(implicit c: fastparse.P[Any]): P[PART[String]] = {
    def nameP(implicit c: fastparse.P[Any]) = (char | "_") ~~ ("_".? ~~ (digit | char)).repX()
    genericVarName(nameP(_))
  }

  def genericMethodName(implicit c: fastparse.P[Any]): P[PART[String]] =
    accessOrName.filter {
      case VALID(_, name) if GenericMethod.KnownMethods.contains(name) => true
      case _                                                           => false
    }

  def accessP[A: P]: P[(Int, Accessor, Int)] = P(
    (("" ~ comment ~ Index ~ "." ~/ comment ~ functionCallOrGetter) ~~ Index) | (Index ~~ "[" ~/ baseExpr.map(ListIndex) ~ "]" ~~ Index)
  )

  def functionCallOrGetter[A: P]: P[Accessor] =
    (genericMethodName ~~/ ("[" ~ unionTypeP ~ "]")).map { case (name, tpe) =>
      GenericMethod(name, tpe)
    } | (accessOrName.map(Getter) ~/ comment ~~ ("(" ~/ comment ~ functionCallArgs ~/ comment ~ ")").?).map { case (g @ Getter(name), args) =>
      args.fold(g: Accessor)(Method(name, _))
    }

  def maybeAccessP[A: P]: P[EXPR] =
    P(Index ~~ extractableAtom ~~ Index ~~ NoCut(accessP).repX)
      .map { case (start, obj, _, accessors) =>
        accessors.foldLeft(obj) { case (e, (accessStart, a, accessEnd)) =>
          a match {
            case Getter(n)           => GETTER(Pos(start, accessEnd), e, n)
            case Method(n, args)     => FUNCTION_CALL(Pos(start, accessEnd), n, e :: args.toList)
            case ListIndex(index)    => FUNCTION_CALL(Pos(start, accessEnd), PART.VALID(Pos(accessStart, accessEnd), "getElement"), List(e, index))
            case GenericMethod(n, t) => GENERIC_FUNCTION_CALL(Pos(start, accessEnd), e, n, t)
          }
        }
      }

  def byteVectorP[A: P]: P[EXPR] =
    P(Index ~~ "base" ~~ ("58" | "64" | "16").! ~~ "'" ~/ Pass ~~ CharPred(_ != '\'').repX.! ~~ "'" ~~ Index)
      .map { case (start, base, xs, end) =>
        val innerStart = start + 8
        val innerEnd   = end - 1
        val decoded = base match {
          case "16" => Global.base16Decode(xs, checkLength = false)
          case "58" => Global.base58Decode(xs)
          case "64" => Global.base64Decode(xs)
        }
        decoded match {
          case Left(err) => CONST_BYTESTR(Pos(start, end), PART.INVALID(Pos(innerStart, innerEnd), err))
          case Right(r)  => CONST_BYTESTR(Pos(start, end), PART.VALID(Pos(innerStart, innerEnd), ByteStr(r)))
        }
      }

  private def destructuredTupleValuesP[A: P]: P[Seq[(Int, Option[PART[String]])]] =
    P("(") ~
      (Index ~ anyVarName.?).rep(
        ContractLimits.MinTupleSize,
        comment ~ "," ~ comment,
        ContractLimits.MaxTupleSize
      ) ~
      P(")")

  private def letNameP[A: P]: P[Seq[(Int, Option[PART[String]])]] =
    (Index ~ anyVarName.?).map(Seq(_))

  def variableDefP[A: P](key: String): P[Seq[LET]] =
    P(
      Index ~~ key ~~ &(
        spaces
      ) ~/ comment ~ (destructuredTupleValuesP | letNameP) ~ comment ~ Index ~ ("=" ~/ Index ~ baseExpr.?).? ~~ Index
    ).map { case (start, names, valuePos, valueRaw, end) =>
      val value = extractValue(valuePos, valueRaw)
      val pos   = Pos(start, end)
      if (names.length == 1)
        names.map { case (nameStart, nameRaw) =>
          val name = extractName(Pos(nameStart, nameStart), nameRaw)
          LET(pos, name, value)
        }
      else {
        val exprRefName = "$t0" + s"${pos.start}${pos.end}"
        val exprRef     = LET(pos, VALID(pos, exprRefName), value)
        val tupleValues =
          names.zipWithIndex
            .map { case ((nameStart, nameRaw), i) =>
              val namePos = Pos(nameStart, nameStart)
              val name    = extractName(namePos, nameRaw)
              val getter = GETTER(
                namePos,
                REF(namePos, VALID(namePos, exprRefName)),
                VALID(namePos, s"_${i + 1}")
              )
              LET(pos, name, getter)
            }
        exprRef +: tupleValues
      }
    }

  // Hack to force parse of "\n". Otherwise it is treated as a separator
  def newLineSep(implicit c: fastparse.P[Any]) = {
    P(CharsWhileIn(" \t\r").repX ~~ "\n").repX(1)
  }

  def strictLetBlockP[A: P]: P[EXPR] = {
    P(
      Index ~~
        variableDefP("strict") ~/
        Pass ~~
        (
          ("" ~ ";") ~/ (baseExpr | invalid).? |
            newLineSep ~/ (baseExpr | invalid).? |
            (Index ~~ CharPred(_ != '\n').repX).map(pos => Some(INVALID(Pos(pos, pos), "expected ';'")))
        ) ~~
        Index
    ).map { case (start, varNames, body, end) =>
      val blockPos = Pos(start, end)
      Macro.unwrapStrict(blockPos, varNames, body.getOrElse(INVALID(Pos(end, end), "expected a body")))
    }
  }

  private def extractName(
      namePos: Pos,
      nameRaw: Option[PART[String]]
  ): PART[String] =
    nameRaw.getOrElse(PART.INVALID(namePos, "expected a variable's name"))

  private def extractValue(
      valuePos: Int,
      valueRaw: Option[(Int, Option[EXPR])]
  ): EXPR =
    valueRaw
      .map { case (pos, expr) => expr.getOrElse(INVALID(Pos(pos, pos), "expected a value's expression")) }
      .getOrElse(INVALID(Pos(valuePos, valuePos), "expected a value"))

  def block[A: P]: P[EXPR] = blockOr(INVALID(_, "expected ';'"))

  private def blockOr(otherExpr: Pos => EXPR)(implicit c: fastparse.P[Any]): P[EXPR] = {
    P(
      Index ~~
        declaration.rep(1) ~/
        Pass ~~
        (
          ("" ~ ";") ~/ (baseExpr | invalid).? |
            newLineSep ~/ (baseExpr | invalid).? |
            (Index ~~ CharPred(_ != '\n').repX).map(pos => Some(otherExpr(Pos(pos, pos))))
        ) ~~
        Index
    ).map { case (start, declarations, body, end) =>
      declarations.flatten.reverse
        .foldLeft(body.getOrElse(INVALID(Pos(end, end), "expected a body"))) { (acc, l) =>
          BLOCK(Pos(start, end), l, acc)
        }
    }
  }

  def baseAtom[A: P](epn: fastparse.P[Any] => P[EXPR]) = {
    def ep[AA: P](implicit c: fastparse.P[Any]) = epn(c)
    comment ~ P(foldMacroP | ifP | matchP | ep | maybeAccessP) ~ comment
  }

  def baseExpr[A: P] = P(strictLetBlockP | binaryOp(baseAtom(block(_))(_), opsByPriority))

  def blockOrDecl[A: P]    = baseAtom(blockOr(p => REF(p, VALID(p, "unit")))(_))
  def baseExprOrDecl[A: P] = binaryOp(baseAtom(blockOrDecl(_))(_), opsByPriority)

  def singleBaseAtom[A: P] =
    comment ~
      P(foldMacroP | ifP | matchP | maybeAccessP) ~
      comment

  def singleBaseExpr[A: P] = P(binaryOp(singleBaseAtom(_), opsByPriority))

  def declaration[A: P] = P(variableDefP("let") | funcP.map(Seq(_)))

  def revp[A, B](l: A, s: Seq[(B, A)], o: Seq[(A, B)] = Seq.empty): (Seq[(A, B)], A) = {
    s.foldLeft((o, l)) { (acc, op) =>
      (acc, op) match { case ((o, l), (b, a)) => ((l, b) +: o) -> a }
    }
  }

  def binaryOp(atomA: fastparse.P[Any] => P[EXPR], rest: List[Either[List[BinaryOperation], List[BinaryOperation]]])(implicit
      c: fastparse.P[Any]
  ): P[EXPR] = {
    def atom(implicit c: fastparse.P[Any]) = atomA(c)
    rest match {
      case Nil => unaryOp(atom(_), unaryOps)
      case Left(kinds) :: restOps =>
        def operand(implicit c: fastparse.P[Any]) = binaryOp(atom(_), restOps)
        val kindc = kinds
          .map(o => { implicit c: fastparse.P[Any] =>
            o.parser
          })
          .reduce((plc, prc) => {
            def pl(implicit c: fastparse.P[Any]) = plc(c)
            def pr(implicit c: fastparse.P[Any]) = prc(c);
            { implicit c: fastparse.P[Any] =>
              P(pl | pr)
            }
          })
        def kind(implicit c: fastparse.P[Any]) = kindc(c)
        P(Index ~~ operand ~ P(kind ~ (NoCut(operand) | Index.map(i => INVALID(Pos(i, i), "expected a second operator")))).rep).map {
          case (start, left: EXPR, r: Seq[(BinaryOperation, EXPR)]) =>
            r.foldLeft(left) { case (acc, (currKind, currOperand)) => currKind.expr(start, currOperand.position.end, acc, currOperand) }
        }
      case Right(kinds) :: restOps =>
        def operand(implicit c: fastparse.P[Any]) = binaryOp(atom(_), restOps)
        val kindc = kinds
          .map(o => { implicit c: fastparse.P[Any] =>
            o.parser
          })
          .reduce((plc, prc) => {
            def pl(implicit c: fastparse.P[Any]) = plc(c)
            def pr(implicit c: fastparse.P[Any]) = prc(c);
            { implicit c: fastparse.P[Any] =>
              P(pl | pr)
            }
          })
        def kind(implicit c: fastparse.P[Any]) = kindc(c)
        P(Index ~~ operand ~ P(kind ~ (NoCut(operand) | Index.map(i => INVALID(Pos(i, i), "expected a second operator")))).rep).map {
          case (start, left: EXPR, r: Seq[(BinaryOperation, EXPR)]) =>
            val (ops, s) = revp(left, r)
            ops.foldLeft(s) { case (acc, (currOperand, currKind)) => currKind.expr(start, currOperand.position.end, currOperand, acc) }
        }
    }
  }

  def unaryOp(atom: fastparse.P[Any] => P[EXPR], ops: List[UnaryOperation])(implicit c: fastparse.P[Any]): P[EXPR] =
    ops.foldRight(atom) { case (op, accc) =>
      def acc(implicit c: fastparse.P[Any]) = accc(c);
      { implicit c: fastparse.P[Any] =>
        (Index ~~ op.parser.map(_ => ()) ~ P(unaryOp(atom, ops)) ~~ Index).map { case (start, expr, end) =>
          op.expr(start, end, expr)
        } | acc
      }
    }(c)

  def parseExpr(str: String): Parsed[EXPR] = {
    def expr[A: P] = P(Start ~ unusedText ~ (baseExpr | invalid) ~ End)
    parse(str, expr(_), verboseFailures = true)
  }

  def parseExprOrDecl(str: String): Parsed[EXPR] = {
    def e[A: P] = P(Start ~ unusedText ~ (baseExprOrDecl | invalid) ~ End)
    parse(str, e(_), verboseFailures = true)
  }

  def parseContract(str: String): Parsed[DAPP] = {
    def contract[A: P] =
      P(Start ~ unusedText ~ declaration.rep ~ comment ~ annotatedFunc.rep ~ declaration.rep ~ End ~~ Index)
        .map { case (ds, fs, t, end) =>
          (DAPP(Pos(0, end), ds.flatten.toList, fs.toList), t)
        }

    parse(str, contract(_), verboseFailures = true) match {
      case Parsed.Success((s, t), _) if (t.nonEmpty) =>
        def contract[A: P] = P(Start ~ unusedText ~ declaration.rep ~ comment ~ annotatedFunc.rep ~ !declaration.rep(1) ~ End ~~ Index)
        parse(str, contract(_)) match {
          case Parsed.Failure(_, o, e) =>
            Parsed.Failure(s"Local functions should be defined before @Callable one: ${str.substring(o)}", o, e)
          case _ => throw new Exception("Parser error")
        }
      case Parsed.Success((s, _), v) => Parsed.Success(s, v)
      case f: Parsed.Failure         => Parsed.Failure(f.label, f.index, f.extra)
    }
  }

  type RemovedCharPos = Pos

  def parseExpressionWithErrorRecovery(scriptStr: String): Either[Throwable, (SCRIPT, Option[RemovedCharPos])] = {

    def parse(str: String): Either[Parsed.Failure, SCRIPT] =
      parseExpr(str) match {
        case Parsed.Success(resExpr, _) => Right(SCRIPT(resExpr.position, resExpr))
        case f: Parsed.Failure          => Left(f)
      }

    parseWithError[SCRIPT](
      new StringBuilder(scriptStr),
      parse,
      SCRIPT(Pos(0, scriptStr.length - 1), INVALID(Pos(0, scriptStr.length - 1), "Parsing failed. Unknown error."))
    ).map { exprAndErrorIndexes =>
      val removedCharPosOpt = if (exprAndErrorIndexes._2.isEmpty) None else Some(Pos(exprAndErrorIndexes._2.min, exprAndErrorIndexes._2.max))
      (exprAndErrorIndexes._1, removedCharPosOpt)
    }
  }

  def parseDAPPWithErrorRecovery(scriptStr: String): Either[Throwable, (DAPP, Option[RemovedCharPos])] = {

    def parse(str: String): Either[Parsed.Failure, DAPP] =
      parseContract(str) match {
        case Parsed.Success(resDAPP, _) => Right(resDAPP)
        case f: Parsed.Failure          => Left(f)
      }

    parseWithError[DAPP](
      new StringBuilder(scriptStr),
      parse,
      DAPP(Pos(0, scriptStr.length - 1), List.empty, List.empty)
    ).map { dAppAndErrorIndexes =>
      val removedCharPosOpt = if (dAppAndErrorIndexes._2.isEmpty) None else Some(Pos(dAppAndErrorIndexes._2.min, dAppAndErrorIndexes._2.max))
      (dAppAndErrorIndexes._1, removedCharPosOpt)
    }
  }

  @tailrec
  private def clearChar(source: StringBuilder, pos: Int): Int = {
    if (pos >= 0) {
      if (" \n\r".contains(source.charAt(pos))) {
        clearChar(source, pos - 1)
      } else {
        source.setCharAt(pos, ' ')
        pos
      }
    } else {
      0
    }
  }

  private def parseWithError[T](
      source: StringBuilder,
      parse: String => Either[Parsed.Failure, T],
      defaultResult: T
  ): Either[Throwable, (T, Iterable[Int])] = {
    parse(source.toString())
      .map(dApp => (dApp, Nil))
      .left
      .flatMap {
        case ex: Parsed.Failure =>
          val errorLastPos       = ex.index
          val lastRemovedCharPos = clearChar(source, errorLastPos - 1)
          val posList            = Set(errorLastPos, lastRemovedCharPos)
          if (lastRemovedCharPos > 0) {
            parseWithError(source, parse, defaultResult)
              .map(dAppAndErrorIndexes => (dAppAndErrorIndexes._1, posList ++ dAppAndErrorIndexes._2.toList))
          } else {
            Right((defaultResult, posList))
          }
        case _ => Left(new Exception("Unknown parsing error."))
      }
  }

  def toString(input: String, f: Failure): String = {
    val found = input.drop(f.index).takeWhile(!_.isWhitespace)
    val betterFound = if (f.label.exists(_.isWhitespace)) {
      val beforeFound = input.take(input.indexOf(found))
      beforeFound.drop(beforeFound.lastIndexWhere(_.isWhitespace) + 1) ++ found
    } else found
    val start = input.lastIndexWhere(_.isWhitespace, input.lastIndexWhere(_.isWhitespace, f.index) - 1) + 1
    val end   = f.index + found.length - 1
    s"Parse error: expected ${f.label}, found \"$betterFound\" in $start-$end"
  }
}
