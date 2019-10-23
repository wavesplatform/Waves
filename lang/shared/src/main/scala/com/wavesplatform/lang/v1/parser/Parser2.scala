package com.wavesplatform.lang.v1.parser

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import com.wavesplatform.lang.v1.parser.Expressions._
import com.wavesplatform.lang.v1.parser.UnaryOperation._
import org.parboiled2._

abstract class Accessor                                                  extends Positioned
case class MethodAcc(position: Pos, name: PART[String], args: Seq[EXPR]) extends Accessor
case class GetterAcc(position: Pos, name: PART[String])                  extends Accessor
case class ListIndexAcc(position: Pos, index: EXPR)                      extends Accessor

case class BinaryOpWithExpr(op: BinaryOperation, expr: EXPR)
case class IdAndTypes(id: PART[String], types: Seq[(PART[String], Option[PART[String]])])

class Parser2(val input: ParserInput) extends Parser {

  private val Global = com.wavesplatform.lang.hacks.Global // Hack for IDEA

  def DAppRoot: Rule1[DAPP] = rule {
    push(cursor) ~ WS ~ zeroOrMore(Directive ~ WS) ~ zeroOrMore(WS ~ Decl) ~ zeroOrMore(WS ~ AnnotatedFunc) ~ WS ~ push(cursor) ~ EOI ~> parseDAppRoot _
  }

  def ScriptRoot: Rule1[SCRIPT] = rule {
    push(cursor) ~ WS ~ zeroOrMore(Directive ~ WS) ~ zeroOrMore(WS ~ Decl) ~ WS ~ Expr ~ push(cursor) ~> parseScriptRoot _
  }

  def Directive: Rule0 = rule { "{-#" ~ WS ~ oneOrMore(noneOf("\n#")) ~ WS ~ "#-}" }

  def Decl: Rule1[Declaration] = rule { Func | Let }

  def AnnotatedFunc: Rule1[ANNOTATEDFUNC] = rule { Annotation ~ WS ~ Func ~ push(cursor) ~> parseAnnotatedFunc _ }
  def Annotation: Rule1[ANNOTATION] = rule {
    push(cursor) ~ "@" ~ IdentifierAtom ~ WS ~ "(" ~ WS ~ zeroOrMore(IdentifierAtom)
      .separatedBy(WS ~ "," ~ WS) ~ WS ~ ")" ~ push(cursor) ~> parseAnnotation _
  }

  def Func: Rule1[FUNC] = rule {
    push(cursor) ~ "func" ~ WS ~ IdentifierAtom ~ WS ~ "(" ~ WS ~ zeroOrMore(FuncArg)
      .separatedBy(WS ~ "," ~ WS) ~ WS ~ ")" ~ WS ~ "=" ~ WS ~ Expr ~ push(cursor) ~> parseFunc _
  }
  def FuncArg: Rule1[IdAndTypes] = rule { IdentifierAtom ~ WS ~ ":" ~ WS ~ GenericTypesAtom ~> IdAndTypes }

  def Let: Rule1[LET] = rule { push(cursor) ~ "let" ~ WS ~ IdentifierAtom ~ WS ~ "=" ~ WS ~ Expr ~ push(cursor) ~> parseLet _ }

  def Block: Rule1[EXPR] = rule { push(cursor) ~ "{" ~ zeroOrMore(WS ~ Decl) ~ WS ~ Expr ~ WS ~ "}" ~ push(cursor) ~> parseBlock _ }
  def BlockWithoutPar: Rule1[EXPR] = rule { push(cursor) ~ zeroOrMore(WS ~ Decl) ~ WS ~ Expr ~ push(cursor) ~> parseBlock _ }

  def Expr: Rule1[EXPR] = rule { OrOpAtom }

  def OrOpAtom: Rule1[EXPR] = rule {
    push(cursor) ~ AndOpAtom ~ zeroOrMore(WS ~ OROP ~ WS ~ AndOpAtom ~> BinaryOpWithExpr) ~ push(cursor) ~> parseBinaryOperationAtom _
  }
  def AndOpAtom: Rule1[EXPR] = rule {
    push(cursor) ~ CompareGroupOpAtom ~ zeroOrMore(WS ~ ANDOP ~ WS ~ CompareGroupOpAtom ~> BinaryOpWithExpr) ~ push(cursor) ~> parseBinaryOperationAtom _
  }
  def CompareGroupOpAtom: Rule1[EXPR] = rule {
    push(cursor) ~ EqualityGroupOpAtom ~ zeroOrMore(WS ~ COMPARE_GROUP_OP ~ WS ~ EqualityGroupOpAtom ~> BinaryOpWithExpr) ~ push(cursor) ~> parseBinaryOperationAtom _
  }
  def EqualityGroupOpAtom: Rule1[EXPR] = rule {
    push(cursor) ~ ConsOpAtom ~ zeroOrMore(WS ~ EQUALITY_GROUP_OP ~ WS ~ ConsOpAtom ~> BinaryOpWithExpr) ~ push(cursor) ~> parseBinaryOperationAtom _
  }
  def ConsOpAtom: Rule1[EXPR] = rule {
    push(cursor) ~ SumGroupOpAtom ~ zeroOrMore(WS ~ CONSOP ~ WS ~ SumGroupOpAtom ~> BinaryOpWithExpr) ~ push(cursor) ~> parseBinaryOperationAtom _
  }
  def SumGroupOpAtom: Rule1[EXPR] = rule {
    push(cursor) ~ MultGroupOpAtom ~ zeroOrMore(WS ~ SUM_GROUP_OP ~ WS ~ MultGroupOpAtom ~> BinaryOpWithExpr) ~ push(cursor) ~> parseBinaryOperationAtom _
  }
  def MultGroupOpAtom: Rule1[EXPR] = rule {
    push(cursor) ~ AtomExpr ~ zeroOrMore(WS ~ MULT_GROUP_OP ~ WS ~ AtomExpr ~> BinaryOpWithExpr) ~ push(cursor) ~> parseBinaryOperationAtom _
  }
  def AtomExpr: Rule1[EXPR] = rule {
    push(cursor) ~ optional(UNARY_OP) ~ WS ~ (Fold | GettableExpr | If | Match | ConstAtom) ~ push(cursor) ~> parseAtomExpr _
  }

  def Fold: Rule1[EXPR] = rule {
    push(cursor) ~ "FOLD" ~ WS ~ "<" ~ WS ~ capture(Digits) ~ WS ~ ">" ~ WS ~ "(" ~ WS ~ Expr ~ WS ~ "," ~ WS ~ Expr ~ WS ~ "," ~ WS ~ ReferenceAtom ~ WS ~ ")" ~ push(
      cursor
    ) ~> parseFoldExpr _
  }

  def GettableExpr: Rule1[EXPR] = rule {
    (ParExpr | Block | FunctionCall | ReferenceAtom) ~ zeroOrMore(WS ~ (ListAccess | ("." ~ WS ~ (FunctionCallAccess | IdentifierAtomAccess)))) ~ push(
      cursor
    ) ~> parseGettableExpr _
  }
  def FunctionCallAccess: Rule1[Accessor]   = rule { FunctionCall ~ push(cursor) ~> parseFunctionCallAccess _ }
  def IdentifierAtomAccess: Rule1[Accessor] = rule { IdentifierAtom ~ push(cursor) ~> parseIdentifierAtomAccess _ }
  def ListAccess: Rule1[Accessor]           = rule { push(cursor) ~ "[" ~ WS ~ (Expr | ReferenceAtom) ~ WS ~ "]" ~ push(cursor) ~> parseListAccess _ }

  def ParExpr: Rule1[EXPR] = rule { "(" ~ WS ~ Expr ~ WS ~ ")" }

  def FunctionCall: Rule1[FUNCTION_CALL] = rule {
    IdentifierAtom ~ WS ~ "(" ~ WS ~ zeroOrMore(Expr).separatedBy(WS ~ "," ~ WS) ~ WS ~ ")" ~ push(cursor) ~> parseFunctionCall _
  }

  def ListAtom: Rule1[EXPR] = rule {
    push(cursor) ~ "[" ~ WS ~ zeroOrMore(Expr).separatedBy(WS ~ "," ~ WS) ~ WS ~ "]" ~ push(cursor) ~> parseListAtom _
  } //~ optional(WS ~ ListAccess)

  def If: Rule1[EXPR] = rule { push(cursor) ~ "if" ~ WS ~ Expr ~ WS ~ "then" ~ WS ~ Expr ~ WS ~ "else" ~ WS ~ Expr ~ push(cursor) ~> parseIf _ }

  def Match: Rule1[EXPR] = rule {
    push(cursor) ~ "match" ~ WS ~ Expr ~ WS ~ "{" ~ oneOrMore(WS ~ MatchCase) ~ WS ~ "}" ~ push(cursor) ~> parseMatch _
  }
  def MatchCase: Rule1[MATCH_CASE] = rule {
    push(cursor) ~ "case" ~ WS ~ ((IdentifierAtom ~ WS ~ ":" ~ WS ~ TypesAtom) | DefaultMatchCase) ~ WS ~ "=>" ~ WS ~ BlockWithoutPar ~ push(cursor) ~> parseMatchCase _
  }
  def DefaultMatchCase: Rule2[PART[String], Seq[PART[String]]] = rule { "_" ~ push(PART.VALID(Pos(0, 0), "_")) ~ push(Seq.empty[PART[String]]) }

  def OROP: Rule1[BinaryOperation]  = rule { "||" ~ push(OR_OP) }
  def ANDOP: Rule1[BinaryOperation] = rule { "&&" ~ push(AND_OP) }

  def COMPARE_GROUP_OP: Rule1[BinaryOperation] = rule { GTOP | GEOP | LTOP | LEOP }
  def GTOP: Rule1[BinaryOperation]             = rule { ">" ~ noneOf("=") ~ push(GT_OP) }
  def GEOP: Rule1[BinaryOperation]             = rule { ">=" ~ push(GE_OP) }
  def LTOP: Rule1[BinaryOperation]             = rule { "<" ~ noneOf("=") ~ push(LT_OP) }
  def LEOP: Rule1[BinaryOperation]             = rule { "<=" ~ push(LE_OP) }

  def EQUALITY_GROUP_OP: Rule1[BinaryOperation] = rule { EQOP | NEOP }
  def EQOP: Rule1[BinaryOperation]              = rule { "==" ~ push(EQ_OP) }
  def NEOP: Rule1[BinaryOperation]              = rule { "!=" ~ push(NE_OP) }

  def CONSOP: Rule1[BinaryOperation] = rule { "::" ~ push(CONS_OP) }

  def SUM_GROUP_OP: Rule1[BinaryOperation] = rule { SUMOP | SUBOP }
  def SUMOP: Rule1[BinaryOperation]        = rule { "+" ~ push(SUM_OP) }
  def SUBOP: Rule1[BinaryOperation]        = rule { "-" ~ push(SUB_OP) }

  def MULT_GROUP_OP: Rule1[BinaryOperation] = rule { MULOP | DIVOP | MODOP }
  def MULOP: Rule1[BinaryOperation]         = rule { "*" ~ push(MUL_OP) }
  def DIVOP: Rule1[BinaryOperation]         = rule { "/" ~ push(DIV_OP) }
  def MODOP: Rule1[BinaryOperation]         = rule { "%" ~ push(MOD_OP) }

  def UNARY_OP: Rule1[UnaryOperation]   = rule { POSITIVEOP | NEGATIVEOP | NOTOP }
  def POSITIVEOP: Rule1[UnaryOperation] = rule { "+" ~ push(POSITIVE_OP) }
  def NEGATIVEOP: Rule1[UnaryOperation] = rule { "-" ~ push(NEGATIVE_OP) }
  def NOTOP: Rule1[UnaryOperation]      = rule { "!" ~ push(NOT_OP) }

  def ConstAtom: Rule1[EXPR] = rule { IntegerAtom | StringAtom | ByteVectorAtom | BooleanAtom | ListAtom }

  def IdentifierAtom: Rule1[PART[String]] = rule {
    push(cursor) ~ capture(!ReservedWords ~ Char ~ zeroOrMore(Char | Digit)) ~ push(cursor) ~> parseIdentifierAtom _
  }
  def ReferenceAtom: Rule1[EXPR] = rule {
    push(cursor) ~ capture(!ReservedWords ~ Char ~ zeroOrMore(Char | Digit)) ~ push(cursor) ~> parseReferenceAtom _
  }

  def GenericTypesAtom: Rule1[Seq[(PART[String], Option[PART[String]])]] = rule { oneOrMore(OneGenericTypeAtom).separatedBy(WS ~ "|" ~ WS) }
  def TypesAtom: Rule1[Seq[PART[String]]]                                = rule { oneOrMore(OneTypeAtom).separatedBy(WS ~ "|" ~ WS) }
  def OneGenericTypeAtom: Rule1[(PART[String], Option[PART[String]])] = rule {
    capture(Char ~ zeroOrMore(Char | Digit)) ~ optional(WS ~ "[" ~ WS ~ OneTypeAtom ~ WS ~ "]" ~ WS) ~> parseGenericTypeAtom _
  }
  def OneTypeAtom: Rule1[PART[String]] = rule { capture(Char ~ zeroOrMore(Char | Digit)) ~> parseOneTypeAtom _ }

  def ByteVectorAtom: Rule1[EXPR] = rule {
    push(cursor) ~ "base" ~ capture(("58" | "64" | "16")) ~ "'" ~ capture(zeroOrMore(ByteBaseChar)) ~ push(cursor) ~> parseByteVectorAtom _ ~ "'"
  }
  def ByteBaseChar: Rule0 = rule { LowerChar | UpperChar | Digit | "+" | "/" | "=" }

  def BooleanAtom: Rule1[EXPR] = rule { "true" ~ push(parseTrueAtom(cursor - 4)) | "false" ~ push(parseFalseAtom(cursor - 5)) }

  def StringAtom: Rule1[EXPR] = rule { "\"" ~ push(cursor) ~ capture(zeroOrMore(noneOf("\"\\"))) ~ push(cursor) ~> parseStringAtom _ ~ "\"" }
  def Char: Rule0             = rule { LowerChar | UpperChar }
  def UpperChar: Rule0        = rule { CharPredicate.UpperAlpha }
  def LowerChar: Rule0        = rule { CharPredicate.LowerAlpha }

  // as in old parser: "+ 1" error            rule{group(optional(anyOf("+-")) ~ WS ~ oneOrMore(Digit)) ~> parseIntegerAtom}
  def IntegerAtom: Rule1[EXPR] = rule { push(cursor) ~ capture(Digits) ~ push(cursor) ~> parseIntegerAtom _ }
  def Digits: Rule0            = rule { oneOrMore(CharPredicate.Digit) }
  def Digit: Rule0             = rule { CharPredicate.Digit }

  def Comment: Rule0 = rule { noneOf("{") ~ noneOf("-") ~ "#" ~ noneOf("-") ~ noneOf("}") ~ zeroOrMore(noneOf("\n")) ~ "\n" }
  def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
  def WS: Rule0         = WhiteSpace

  def ReservedWords: Rule0 = rule { "let" | "base16" | "base58" | "base64" | "true" | "false" | "if" | "then" | "else" | "match" | "case" | "func" }

  def parseDAppRoot(startPos: Int, decList: Seq[Declaration], annFuncList: Seq[ANNOTATEDFUNC], endPos: Int): DAPP = {
    DAPP(Pos(startPos, endPos), decList.toList, annFuncList.toList)
  }

  def parseScriptRoot(startPos: Int, decList: Seq[Declaration], expr: EXPR, endPos: Int): SCRIPT = {
    val resExpr = decList.foldRight(expr) { (dec, combExpr) =>
      BLOCK(
        Pos(dec.position.start, expr.position.end),
        dec,
        combExpr
      )
    }
    SCRIPT(Pos(startPos, endPos), resExpr)
  }

  def parseBlock(startPos: Int, decList: Seq[Declaration], expr: EXPR, endPos: Int): EXPR = {
    decList.foldRight(expr) { (dec, resultBlock) =>
      BLOCK(dec.position, dec, resultBlock)
    }
  }

  def parseAnnotatedFunc(annotation: ANNOTATION, func: FUNC, endPos: Int): ANNOTATEDFUNC = {
    ANNOTATEDFUNC(Pos(annotation.position.start, endPos), List(annotation), func)
  }

  def parseAnnotation(startPos: Int, name: PART[String], args: Seq[PART[String]], endPos: Int): ANNOTATION = {
    ANNOTATION(Pos(startPos, endPos), name, args)
  }

  def parseFunc(startPos: Int, name: PART[String], argAndTypesList: Seq[IdAndTypes], expr: EXPR, endPos: Int): FUNC = {
    FUNC(
      Pos(startPos, endPos),
      name,
      argAndTypesList.map(el => (el.id, el.types)): FuncArgs,
      expr
    )
  }

  def parseLet(startPos: Int, name: PART[String], value: EXPR, endPos: Int): LET = {
    LET(Pos(startPos, endPos), name, value, Seq.empty)
  }

  // TODO Check right convertion
  def parseFoldExpr(startPos: Int, limitNumStr: String, list: EXPR, acc: EXPR, f: EXPR, endPos: Int): EXPR = {
    Macro.unwrapFold(Pos(startPos, endPos), limitNumStr.toInt, list, acc, f.asInstanceOf[REF])
  }

  // TODO проверить правильность позиции
  def parseGettableExpr(expr: EXPR, accessors: Seq[Accessor], endPos: Int): EXPR = {
    val pos = Pos(expr.position.start, endPos)
    val res = accessors.foldLeft(expr) { (resExpr, accessor) =>
      accessor match {
        case GetterAcc(pos, name)       => GETTER(Pos(resExpr.position.start, pos.end), resExpr, name)
        case MethodAcc(pos, name, args) => FUNCTION_CALL(Pos(resExpr.position.start, pos.end), name, (resExpr :: args.toList))
        case ListIndexAcc(pos, index) =>
          FUNCTION_CALL(Pos(resExpr.position.start, pos.end), PART.VALID(Pos(0, 0), "getElement"), List(resExpr, index))
      }
    }
    res
  }

  // TODO проверить правильность позиции
  def parseFunctionCallAccess(funcCall: FUNCTION_CALL, endPos: Int): Accessor = {
    val pos = Pos(funcCall.position.start, endPos)
    MethodAcc(pos, funcCall.name, funcCall.args)
  }

  // TODO проверить правильность позиции
  def parseIdentifierAtomAccess(id: PART[String], endPos: Int): Accessor = {
    GetterAcc(Pos(id.position.start, endPos), id)
  }

  def parseFunctionCall(id: PART[String], args: Seq[EXPR], endPos: Int): FUNCTION_CALL = {
    FUNCTION_CALL(Pos(id.position.start, endPos), id, args.toList)
  }

  // TODO проверить правильность позиции
  def parseListAccess(startPos: Int, accessObj: EXPR, endPos: Int): Accessor = {
    val pos = Pos(startPos, endPos)
    accessObj match {
      case ref: REF   => GetterAcc(pos, ref.key)
      case expr: EXPR => ListIndexAcc(pos, expr)
    }
  }

  def parseListAtom(startPos: Int, elements: Seq[EXPR], endPos: Int): EXPR = {
    val pos = Pos(startPos, endPos)
    elements.foldRight(REF(pos, PART.VALID(Pos(0, 0), "nil")): EXPR) { (resultExpr, element) =>
      FUNCTION_CALL(pos, PART.VALID(Pos(0, 0), "cons"), List(resultExpr, element))
    }
  }

  def parseIf(startPos: Int, cond: EXPR, ifTrue: EXPR, ifFalse: EXPR, endPos: Int): EXPR = {
    IF(Pos(startPos, endPos), cond, ifTrue, ifFalse)
  }

  def parseMatch(startPos: Int, expr: EXPR, cases: Seq[MATCH_CASE], endPos: Int): EXPR = {
    Expressions.MATCH(Pos(startPos, endPos), expr, cases)
  }

  def parseMatchCase(startPos: Int, id: PART[String], types: Seq[PART[String]], expr: EXPR, endPos: Int): MATCH_CASE = {
    val newVarName = id match {
      case PART.VALID(pos, "_") => None
      case _                    => Some(id)
    }
    MATCH_CASE(Pos(startPos, endPos), newVarName, types, expr)
  }

  def parseBinaryOperationAtom(startPos: Int, leftExpr: EXPR, opAndExprList: Seq[BinaryOpWithExpr], endPos: Int): EXPR = {
    opAndExprList.foldLeft(leftExpr) { (exprLeft: EXPR, opAndExprRight: BinaryOpWithExpr) =>
      {
        val pos = Pos(exprLeft.position.start, opAndExprRight.expr.position.end)
        opAndExprRight.op match {
          case LT_OP   => BINARY_OP(pos, opAndExprRight.expr, GT_OP, exprLeft)
          case LE_OP   => BINARY_OP(pos, opAndExprRight.expr, GE_OP, exprLeft)
          case CONS_OP => FUNCTION_CALL(pos, PART.VALID(pos, "cons"), List(exprLeft, opAndExprRight.expr))
          case _       => BINARY_OP(pos, exprLeft, opAndExprRight.op, opAndExprRight.expr)
        }
      }
    }
  }

  def parseAtomExpr(startPos: Int, unOperationOpt: Option[UnaryOperation], expr: EXPR, endPos: Int): EXPR = {
    unOperationOpt match {
      case Some(POSITIVE_OP) | None => expr
      case Some(op)                 => FUNCTION_CALL(Pos(startPos, endPos), PART.VALID(Pos(0, 0), op.func), List(expr))
    }
  }

  def parseIdentifierAtom(startPos: Int, typeName: String, endPos: Int): PART[String] = {
    PART.VALID(Pos(0, 0), typeName)
  }

  def parseReferenceAtom(startPos: Int, typeName: String, endPos: Int): EXPR = {
    REF(Pos(startPos, endPos), PART.VALID(Pos(0, 0), typeName))
  }

  def parseTypesAtom(argTypeList: List[PART[String]]): List[PART[String]] = {
    argTypeList
  }

  def parseGenericTypeAtom(genericTypeName: String, typeName: Option[PART[String]]): (PART[String], Option[PART[String]]) = {
    (PART.VALID(Pos(0, 0), genericTypeName), typeName)
  }

  def parseOneTypeAtom(typeName: String): PART[String] = {
    PART.VALID(Pos(0, 0), typeName)
  }

  // TODO ошибка парсинга байт строки
  def parseByteVectorAtom(startPos: Int, base: String, byteStr: String, endPos: Int): EXPR = {
    val decoded = base match {
      case "16" => Global.base16Decode(byteStr)
      case "58" => Global.base58Decode(byteStr)
      case "64" => Global.base64Decode(byteStr)
      case _ => Left("Wrong input around 'base...' construction")
    }
    val result = decoded match {
      case Left(err) => CONST_BYTESTR(Pos(startPos, endPos), PART.INVALID(Pos(startPos, endPos), err.toString))
      case Right(r) => CONST_BYTESTR(Pos(startPos, endPos), PART.VALID(Pos(0, 0), ByteStr(r)))
    }
    result
  }

  def parseTrueAtom(startPos: Int): EXPR = {
    TRUE(Pos(startPos, startPos + 4))
  }

  def parseFalseAtom(startPos: Int): EXPR = {
    FALSE(Pos(startPos, startPos + 4))
  }

  def parseStringAtom(startPos: Int, chars: String, endPos: Int): EXPR = {
    CONST_STRING(Pos(startPos, endPos), PART.VALID(Pos(0, 0), chars))
  }

  def parseIntegerAtom(startPos: Int, numberStr: String, endPos: Int): EXPR = {
    CONST_LONG(Pos(startPos, endPos), numberStr.toLong)
  }
}

object Parser2 {

  type RemovedCharPos = Pos

  final case class ParsingError(start: Int, end: Int, message: String)

  def parseExpression(scriptStr: String): Either[Throwable, (SCRIPT, Option[RemovedCharPos])] = {

    def parse(str: String): Either[Throwable, SCRIPT] = new Parser2(str).ScriptRoot.run().toEither

    parseWithError[SCRIPT](
      new StringBuilder(scriptStr),
      parse
    ).map {
      exprAndErrorIndexes =>
        val removedCharPosOpt = if (exprAndErrorIndexes._2.isEmpty) None else Some(Pos(exprAndErrorIndexes._2.min, exprAndErrorIndexes._2.max))
        (exprAndErrorIndexes._1, removedCharPosOpt)
    }
  }

  def parseDAPP(scriptStr: String): Either[Throwable, (DAPP, Option[RemovedCharPos])] = {

    def parse(str: String): Either[Throwable, DAPP] = new Parser2(str).DAppRoot.run().toEither

    parseWithError[DAPP](new StringBuilder(scriptStr), parse
    ).map {
      dAppAndErrorIndexes =>
        val removedCharPosOpt = if (dAppAndErrorIndexes._2.isEmpty) None else Some(Pos(dAppAndErrorIndexes._2.min, dAppAndErrorIndexes._2.max))
        (dAppAndErrorIndexes._1, removedCharPosOpt)
    }
  }

  private def clearChar(source: StringBuilder, pos: Int): Unit = {
    if (pos >= 0) {
      if (" \n\r".contains(source.charAt(pos))) {
        clearChar(source, pos - 1)
      } else {
        source.setCharAt(pos, ' ')
      }
    }
  }

  private def parseWithError[T](
      source: StringBuilder,
      parse: String => Either[Throwable, T]
  ): Either[Throwable, (T, Iterable[Int])] = {
    parse(source.toString())
      .map(dApp => (dApp, Nil))
      .left
      .flatMap {
        case ex: ParseError =>
          val errorLastPos = ex.position.index
          if (errorLastPos > 0) {
            clearChar(source, errorLastPos - 1)
            parseWithError(source, parse).map(dAppAndErrorIndexes => (dAppAndErrorIndexes._1, errorLastPos :: dAppAndErrorIndexes._2.toList))
          } else {
            Left(ex)
          }
        case ex: Throwable => Left(ex)
    }
  }
}
