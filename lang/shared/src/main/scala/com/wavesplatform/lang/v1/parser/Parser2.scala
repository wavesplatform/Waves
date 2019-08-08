package com.wavesplatform.lang.v1.parser

import org.parboiled.scala._
import org.parboiled.support.IndexRange
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.parser.Expressions._


abstract class Accessor extends Positioned
case class MethodAcc(position: Pos, name: PART[String], args: Seq[EXPR]) extends Accessor
case class GetterAcc(position: Pos, name: PART[String])                  extends Accessor
case class ListIndexAcc(position: Pos, index: EXPR)                      extends Accessor



sealed trait UnaryOp {
  def name: String
}
case class POSITIVEOP() extends UnaryOp {
  def name = "+"
}
case class NEGATIVEOP() extends UnaryOp {
  def name = "-"
}
case class NOTOP() extends UnaryOp {
  def name = "!"
}

// TODO fix type
trait BinaryOp
case class OROP() extends BinaryOp
case class ANDOP() extends BinaryOp
case class EQOP() extends BinaryOp
case class NEOP() extends BinaryOp
case class GEOP() extends BinaryOp
case class GTOP() extends BinaryOp
case class SUMOP() extends BinaryOp
case class SUBOP() extends BinaryOp
case class MULOP() extends BinaryOp
case class DIVOP() extends BinaryOp
case class MODOP() extends BinaryOp
case class LEOP() extends BinaryOp
case class LTOP() extends BinaryOp
case class CONSOP() extends BinaryOp


class Parser2() extends Parser {

  private val Global = com.wavesplatform.lang.hacks.Global // Hack for IDEA



  def DAppRoot: Rule1[DAPP] = rule{WS ~>> IDX ~ zeroOrMore(WS ~ Decl) ~ zeroOrMore(WS ~ AnnotatedFunc) ~ WS ~ EOI ~>> IDX ~~> parseDAppRoot _}

  def ScriptRoot: Rule1[EXPR] = rule{WS ~>> IDX ~ zeroOrMore(WS ~ Decl) ~ Expr ~ WS ~ EOI ~>> IDX ~~> parseScriptRoot _}

  def Decl: Rule1[Declaration] = rule{Func | Let}

  def AnnotatedFunc: Rule1[ANNOTATEDFUNC] = rule{Annotation ~ WS ~ Func ~>> IDX ~~> parseAnnotatedFunc _}
  def Annotation: Rule1[ANNOTATION] = rule{"@" ~>> IDX ~ IdentifierAtom ~ WS ~ "(" ~ WS ~ zeroOrMore(IdentifierAtom, WS ~ "," ~ WS) ~ WS ~ ")" ~>> IDX ~~> parseAnnotation _}

  def Func: Rule1[FUNC] = rule{"func" ~>> IDX ~ WS ~ IdentifierAtom ~ WS ~ "(" ~ WS ~ zeroOrMore(FuncArg, WS ~ "," ~ WS) ~ WS ~ ")" ~ WS ~ "=" ~ WS ~ Expr ~>> IDX ~~> parseFunc _}
  def FuncArg = rule{IdentifierAtom ~ WS ~ ":" ~ WS ~ TypesAtom}

  def Let: Rule1[LET] = rule{"let" ~>> IDX ~ WS ~ IdentifierAtom ~ WS ~ "=" ~ WS ~ Expr ~>> IDX ~~> parseLet _}

  def Block: Rule1[EXPR] = rule{"{" ~>> IDX ~ zeroOrMore(WS ~ Decl) ~ WS ~ Expr ~ WS ~ "}" ~>> IDX ~~> parseBlock _}

  def Expr: Rule1[EXPR] = rule{OrOpAtom}

  def OrOpAtom: Rule1[EXPR] = rule{AndOpAtom ~>> IDX ~ zeroOrMore(WS ~ OR_OP ~ WS ~ AndOpAtom) ~>> IDX ~~> parseBinaryOperationAtom _}
  def AndOpAtom: Rule1[EXPR] = rule{CompareGroupOpAtom ~>> IDX ~ zeroOrMore(WS ~ AND_OP ~ WS ~ CompareGroupOpAtom) ~>> IDX ~~> parseBinaryOperationAtom _}
  def CompareGroupOpAtom: Rule1[EXPR] = rule{EqualityGroupOpAtom ~>> IDX ~ zeroOrMore(WS ~ COMPARE_GROUP_OP ~ WS ~ EqualityGroupOpAtom) ~>> IDX ~~> parseBinaryOperationAtom _}
  def EqualityGroupOpAtom: Rule1[EXPR] = rule{ConsOpAtom ~>> IDX ~ zeroOrMore(WS ~ EQUALITY_GROUP_OP ~ WS ~ ConsOpAtom) ~>> IDX ~~> parseBinaryOperationAtom _}
  def ConsOpAtom: Rule1[EXPR] = rule{SumGroupOpAtom ~>> IDX ~ zeroOrMore(WS ~ CONS_OP ~ WS ~ SumGroupOpAtom) ~>> IDX ~~> parseBinaryOperationAtom _}
  def SumGroupOpAtom: Rule1[EXPR] = rule{MultGroupOpAtom ~>> IDX ~ zeroOrMore(WS ~ SUM_GROUP_OP ~ WS ~ MultGroupOpAtom) ~>> IDX ~~> parseBinaryOperationAtom _}
  def MultGroupOpAtom: Rule1[EXPR] = rule{AtomExpr ~>> IDX ~ zeroOrMore(WS ~ MULT_GROUP_OP ~ WS ~ AtomExpr) ~>> IDX ~~> parseBinaryOperationAtom _}
  def AtomExpr: Rule1[EXPR] = rule{optional(UNARY_OP) ~>> IDX ~ WS ~ (GettableExpr | If | Match | ConstAtom) ~>> IDX ~~> parseAtomExpr _}

  def GettableExpr: Rule1[EXPR] = rule{(ParExpr | Block | FunctionCall | ReferenceAtom) ~ zeroOrMore(WS ~ (ListAccess | ("." ~ WS ~ (FunctionCallAccess | IdentifierAtomAccess)))) ~>> IDX ~~> parseGettableExpr _}
  def FunctionCallAccess: Rule1[Accessor] = rule{FunctionCall ~>> IDX ~~> parseFunctionCallAccess _}
  def IdentifierAtomAccess: Rule1[Accessor] = rule{IdentifierAtom ~>> IDX ~~> parseIdentifierAtomAccess _}
  def ListAccess: Rule1[Accessor] = rule{"[" ~>> IDX ~ WS ~ (Expr | ReferenceAtom) ~ WS ~ "]" ~>> IDX ~~> parseListAccess _}

  def ParExpr: Rule1[EXPR] = rule{"(" ~ WS ~ Expr ~ WS ~ ")"}

  def FunctionCall: Rule1[FUNCTION_CALL] = rule{IdentifierAtom ~ WS ~ "(" ~ WS  ~ zeroOrMore(Expr, WS ~ "," ~ WS) ~ WS ~ ")" ~>> IDX ~~> parseFunctionCall _}

  def ListAtom: Rule1[EXPR] = rule{"[" ~>> IDX ~ WS ~ zeroOrMore(Expr, WS ~ "," ~ WS) ~ WS ~ "]" ~>> IDX ~~> parseListAtom _}                //~ optional(WS ~ ListAccess)

  def If: Rule1[EXPR] = rule{"if" ~>> IDX ~ WS ~ Expr ~ WS ~ "then" ~ WS ~ Expr ~ WS ~ "else" ~ WS ~ Expr ~>> IDX ~~> parseIf _}

  def Match: Rule1[EXPR] = rule{"match" ~>> IDX ~ WS ~ Expr ~ WS ~ "{" ~ oneOrMore(WS ~ MatchCase) ~ WS ~ "}" ~>> IDX ~~> parseMatch _}
  def MatchCase = rule{"case" ~>> IDX ~ WS ~ ((IdentifierAtom ~ WS ~ ":" ~ WS ~ TypesAtom) | DefaultMatchCase) ~ WS ~ "=>" ~ WS ~ Expr ~>> IDX ~~> parseMatchCase _}
  def DefaultMatchCase = rule{"_" ~ push(PART.VALID(Pos(0, 0), "_")) ~ push(List.empty[PART[String]])}

  def OR_OP = rule{"||" ~ push(OROP())}
  def AND_OP = rule{"&&"  ~ push(ANDOP())}

  def COMPARE_GROUP_OP = rule{GT_OP | GE_OP | LT_OP | LE_OP}
  def GT_OP = rule{">" ~ push(GTOP())}
  def GE_OP = rule{">=" ~ push(GEOP())}
  def LT_OP = rule{"<" ~ push(LTOP())}
  def LE_OP = rule{"<=" ~ push(LEOP())}

  def EQUALITY_GROUP_OP = rule{EQ_OP | NE_OP}
  def EQ_OP = rule{"==" ~ push(EQOP())}
  def NE_OP = rule{"!=" ~ push(NEOP())}

  def CONS_OP = rule{"::" ~ push(CONSOP())}

  def SUM_GROUP_OP = rule{SUM_OP | SUB_OP}
  def SUM_OP = rule{"+" ~ push(SUMOP())}
  def SUB_OP = rule{"-" ~ push(SUBOP())}

  def MULT_GROUP_OP = rule{MUL_OP | DIV_OP | MOD_OP}
  def MUL_OP = rule{"*" ~ push(MULOP())}
  def DIV_OP = rule{"/" ~ push(DIVOP())}
  def MOD_OP = rule{"%" ~ push(MODOP())}

  def UNARY_OP = rule{POSITIVE_OP | NEGATIVE_OP | NOT_OP}
  def POSITIVE_OP = rule{"+" ~ push(POSITIVEOP())}
  def NEGATIVE_OP = rule{"-" ~ push(NEGATIVEOP())}
  def NOT_OP = rule{"!" ~ push(NOTOP())}

  def ConstAtom = rule{IntegerAtom | StringAtom | ByteVectorAtom | BooleanAtom | ListAtom}

  def IdentifierAtom = rule{group(!ReservedWords ~ Char ~ zeroOrMore(Char | Digit)) ~> (str => str) ~>> IDX ~~> parseIdentifierAtom _}
  def ReferenceAtom = rule{group(!ReservedWords ~ Char ~ zeroOrMore(Char | Digit)) ~> (str => str) ~>> IDX ~~> parseReferenceAtom _}

  def TypesAtom = rule{oneOrMore(OneTypeAtom, WS ~ "|" ~ WS)}
  def OneTypeAtom = rule{group(Char ~ zeroOrMore(Char | Digit)) ~> parseOneTypeAtom}

  def ByteVectorAtom = rule{"base" ~>> IDX ~ ("58" | "64" | "16") ~> (str => str) ~ "'" ~ zeroOrMore(ByteBaseChar) ~> (str => str) ~>> IDX ~~> parseByteVectorAtom _ ~ "'"}
  def ByteBaseChar: Rule0 = rule{LowerChar | UpperChar | Digit | "+" | "/" | "="}

  def BooleanAtom = rule{"true" ~>> IDX ~~> parseTrueAtom _ | "false" ~>> IDX ~~> parseFalseAtom _}

  def StringAtom = rule {"\"" ~ zeroOrMore(!anyOf("\"\\") ~ ANY) ~> (str => str) ~>> IDX ~~> parseStringAtom _ ~ "\""}
  //def String = rule{zeroOrMore(!anyOf("\"\\") ~ ANY) ~> (str => str) ~>> pushIndexRange _}
  def Char: Rule0 = rule{LowerChar | UpperChar}
  def UpperChar: Rule0 = rule{"A" - "Z"}
  def LowerChar: Rule0 = rule{"a" - "z"}

  // as in old parser: "+ 1" error            rule{group(optional(anyOf("+-")) ~ WS ~ oneOrMore(Digit)) ~> parseIntegerAtom}
  def IntegerAtom = rule{oneOrMore(Digit) ~> (str => str) ~>> IDX ~~> parseIntegerAtom _}
  def Digits: Rule0 = rule { oneOrMore(Digit) }
  def Digit: Rule0 = rule{"0" - "9"}

  def WhiteSpace: Rule0 = rule{zeroOrMore(anyOf(" \n\r\t\f"))}
  def WS: Rule0 = WhiteSpace

  def ReservedWords: Rule0 = rule{"let" | "base58" | "base64" | "true" | "false" | "if" | "then" | "else" | "match" | "case" | "func"}






  def parseDAppRoot(startPos: Pos, decList: List[Declaration], annFuncList: List[ANNOTATEDFUNC], endPos: Pos): DAPP = {
    DAPP(Pos(startPos.start, endPos.end), decList, annFuncList)
  }

  def parseScriptRoot(startPos: Pos, decList: List[Declaration], expr: EXPR, endPos: Pos): EXPR = {
    decList.foldRight(expr) {
      (dec, resExpr) =>
        BLOCK(dec.position, dec, resExpr)
    }
  }

  def parseBlock(startPos: Pos, decList: List[Declaration], expr: EXPR, endPos: Pos): EXPR = {
    decList.foldRight(expr) {
      (dec, resultBlock) =>
        BLOCK(dec.position, dec, resultBlock)
    }
  }

  def parseAnnotatedFunc (annotation: ANNOTATION, func: FUNC, pos: Pos): ANNOTATEDFUNC = {
    ANNOTATEDFUNC(Pos(annotation.position.start, pos.end), List(annotation), func)
  }

  def parseAnnotation (startPos: Pos, name: PART[String], args: List[PART[String]], endPos: Pos): ANNOTATION = {
    ANNOTATION(Pos(startPos.start, endPos.end), name, args)
  }

  def parseFunc(startPos: Pos, name: PART[String], argAndTypesList: List[(PART[String], List[PART[String]])], expr: EXPR, endPos: Pos): FUNC = {
    FUNC(Pos(startPos.start, endPos.end), name, argAndTypesList, expr)
  }

  def parseLet(startPos: Pos, name: PART[String], value: EXPR, endPos: Pos): LET = {
    LET(Pos(startPos.start, endPos.end), name, value, Seq.empty)
  }

  def parseGettableExpr(expr: EXPR, accessors: List[Accessor], endPos: Pos): EXPR = {
    val pos = Pos(expr.position.start, endPos.end)
    val res = accessors.foldLeft(expr) {
      (resExpr, accessor) =>
        accessor match {
          case GetterAcc(pos, name) => GETTER(Pos(resExpr.position.start, pos.end), resExpr, name)
          case MethodAcc(pos, name, args) => FUNCTION_CALL(Pos(resExpr.position.start, pos.end), name, (resExpr :: args.toList))
          case ListIndexAcc(pos, index) => FUNCTION_CALL(Pos(resExpr.position.start, pos.end), PART.VALID(Pos(0, 0), "getElement"), List(resExpr, index))
        }
    }
    res
  }

  // TODO проверить правильность позиции
  def parseIdentifierAtomAccess(id: PART[String], pos: Pos): Accessor = {
    GetterAcc(pos, id)
  }

  // TODO проверить правильность позиции
  def parseFunctionCallAccess(funcCall: FUNCTION_CALL, endPos: Pos): Accessor = {
    val pos = Pos(funcCall.position.start, endPos.end)
    MethodAcc(pos, funcCall.name, funcCall.args)
  }

  def parseFunctionCall(id: PART[String], args: List[EXPR], pos: Pos): FUNCTION_CALL = {
    FUNCTION_CALL(pos, id, args)
  }

  // TODO проверить правильность позиции
  def parseListAccess(startPos: Pos, accessObj: EXPR, endPos: Pos): Accessor = {
    val pos = Pos(startPos.start, endPos.end)
    accessObj match {
      case ref: REF => GetterAcc(pos, ref.key)
      case expr: EXPR => ListIndexAcc(pos, expr)
    }
  }

  def parseListAtom(startPos: Pos, elements: List[EXPR], endPos: Pos): EXPR = {
    val pos = Pos(startPos.start, endPos.end)
    elements.foldRight(REF(pos, PART.VALID(Pos(0, 0), "nil")): EXPR) {
      (resultExpr, element) =>
        FUNCTION_CALL(pos, PART.VALID(Pos(0, 0), "cons"), List(resultExpr, element))
    }
  }

  def parseIf(startPos: Pos, cond: EXPR, ifTrue: EXPR, ifFalse: EXPR, endPos: Pos): EXPR = {
    IF(Pos(startPos.start, endPos.end), cond, ifTrue, ifFalse)
  }

  def parseMatch(startPos: Pos, expr: EXPR, cases: Seq[MATCH_CASE], endPos: Pos): EXPR = {
    MATCH(Pos(startPos.start, endPos.end), expr, cases)
  }

  def parseMatchCase(startPos: Pos, id: PART[String], types: List[PART[String]], expr: EXPR, endPos: Pos): MATCH_CASE = {
    val newVarName = id match {
      case PART.VALID(pos, "_") => None
      case _ => Some(id)
    }
    MATCH_CASE(Pos(startPos.start, endPos.end), newVarName, types, expr)
  }

  def parseBinaryOperationAtom(leftExpr: EXPR, startPos: Pos, opAndExprList: List[(BinaryOp, EXPR)], endPos: Pos): EXPR = {
    opAndExprList.foldLeft(leftExpr){
      (exprLeft: EXPR, opAndExprRight: (BinaryOp, EXPR)) => {
        val pos = Pos(exprLeft.position.start, opAndExprRight._2.position.end)
        opAndExprRight._1 match {
          case LTOP() => BINARY_OP(pos, opAndExprRight._2, GTOP(), exprLeft)
          case LEOP() => BINARY_OP(pos, opAndExprRight._2, GEOP(), exprLeft)
          case CONSOP() => FUNCTION_CALL(pos, PART.VALID(pos, "cons"), List(exprLeft, opAndExprRight._2))
          case _ => BINARY_OP(pos, exprLeft, opAndExprRight._1, opAndExprRight._2)
        }
      }
    }
  }

  def parseAtomExpr(unOperationOpt: Option[UnaryOp], startPos: Pos, expr: EXPR, endPos: Pos): EXPR = {
    unOperationOpt.map{
      op =>
        FUNCTION_CALL(Pos(startPos.start, endPos.end),PART.VALID(Pos(0, 0), op.name), List(expr))
    }.getOrElse(expr)
  }

  def parseIdentifierAtom(typeName: String, pos: Pos): PART[String] = {
    PART.VALID(Pos(0, 0), typeName)
  }

  def parseReferenceAtom(typeName: String, pos: Pos): EXPR = {
    REF(pos, PART.VALID(Pos(0, 0), typeName))
  }

  def parseTypesAtom(argTypeList: List[PART[String]]): List[PART[String]] = {
    argTypeList
  }

  def parseOneTypeAtom(typeName: String): PART[String] = {
    PART.VALID(Pos(0, 0), typeName)
  }

  // TODO ошибка парсинга байт строки
  def parseByteVectorAtom(startPos: Pos, base: String, byteStr: String, endPos: Pos): EXPR = {
    val decoded = base match {
      case "16" => Global.base16Decode(byteStr)
      case "58" => Global.base58Decode(byteStr)
      case "64" => Global.base64Decode(byteStr)
    }
    val result = decoded match {
      //case Left(err) => CONST_BYTESTR(Pos(startPos.start, endPos.end), ByteStr(Array[Byte]()))
      case Right(r)  => CONST_BYTESTR(Pos(startPos.start, endPos.end), PART.VALID(Pos(0, 0), ByteStr(r)))
    }
    result
  }

  def parseTrueAtom(pos: Pos): EXPR = {
    TRUE(pos)
  }

  def parseFalseAtom(pos: Pos): EXPR = {
    FALSE(pos)
  }

  def parseStringAtom(chars: String, pos: Pos): EXPR = {
    CONST_STRING(pos, PART.VALID(Pos(0, 0), chars))
  }

  def parseIntegerAtom(numberStr: String, pos: Pos): EXPR = {
    CONST_LONG(pos, numberStr.toLong)
  }

  def IDX(idxRange: IndexRange): Pos = {
    Pos(idxRange.start, idxRange.end)
  }
}

object Parser2 {

  private val parser = new Parser2 { override val buildParseTree = true }

  def parseExpression(scriptStr: String) = {
    RecoveringParseRunner(parser.ScriptRoot).run(scriptStr).result
  }

  def parseDAPP(scriptStr: String) = {
    RecoveringParseRunner(parser.DAppRoot).run(scriptStr).result
  }
}
