package com.wavesplatform

import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.{CASETYPEREF, FINAL, LIST, NOTHING, TYPE, UNION}
import com.wavesplatform.lang.v1.compiler.{CompilationError, CompilerContext}
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.lang.v1.parser.Expressions.{PART, Type}

import scala.annotation.nowarn
import scala.scalajs.js
import scala.scalajs.js.Any
import scala.scalajs.js.Dynamic.literal as jObj
import scala.scalajs.js.JSConverters.*

object JsApiUtils {

  def serPart[T](f: T => js.Any)(part: PART[T]): js.Object = {
    val partValue = Expressions.PART.toOption(part).fold(null: Any)(f)
    jObj(
      "value"    -> partValue,
      "posStart" -> part.position.start,
      "posEnd"   -> part.position.end
    )
  }

  def serPartStr(part: PART[String]): js.Object = {
    val partValue = Expressions.PART.toOption(part).getOrElse("")
    jObj(
      "value"    -> partValue,
      "posStart" -> part.position.start,
      "posEnd"   -> part.position.end
    )
  }

  def dAppToJs(ast: Expressions.DAPP): js.Object = {

    def serAnnFunc(annFunc: Expressions.ANNOTATEDFUNC): js.Object = {

      def serAnnotation(ann: Expressions.ANNOTATION): js.Object = {
        jObj(
          "type"     -> "ANNOTATION",
          "posStart" -> annFunc.position.start,
          "posEnd"   -> annFunc.position.end,
          "name"     -> serPartStr(ann.name),
          "argList"  -> ann.args.toJSArray.map(serPartStr)
        )
      }

      jObj(
        "type"     -> "ANNOTATEDFUNC",
        "posStart" -> annFunc.position.start,
        "posEnd"   -> annFunc.position.end,
        "annList"  -> annFunc.anns.toJSArray.map(serAnnotation),
        "func"     -> serDec(annFunc.f)
      )
    }

    jObj(
      "type"        -> "DAPP",
      "posStart"    -> ast.position.start,
      "posEnd"      -> ast.position.end,
      "decList"     -> ast.decs.toJSArray.map(serDec),
      "annFuncList" -> ast.fs.toJSArray.map(serAnnFunc)
    )
  }

  def expressionScriptToJs(ast: Expressions.SCRIPT): js.Object =
    jObj(
      "type"     -> "SCRIPT",
      "posStart" -> ast.position.start,
      "posEnd"   -> ast.position.end,
      "expr"     -> serExpr(ast.expr)
    )

  def serExpr(expr: Expressions.EXPR): js.Object = {

    def serType(t: FINAL): js.Object = {
      t match {
        case ut: UNION =>
          jObj(
            "unionTypes" -> ut.typeList.toJSArray.map(serType(_))
          )
        case lt: LIST =>
          jObj(
            "listOf" -> serType(lt.innerType)
          )
        case someT =>
          jObj(
            "type" -> someT.toString()
          )
      }
    }

    val commonDataObj = jObj(
      "type"       -> expr.getName,
      "posStart"   -> expr.position.start,
      "posEnd"     -> expr.position.end,
      "resultType" -> serType(expr.resultType.getOrElse(NOTHING)),
      "ctx"        -> serCtx(expr.ctxOpt.getOrElse(CompilerContext.empty))
    )

    expr match {
      case _: Expressions.CONST_LONG    => commonDataObj
      case _: Expressions.CONST_BYTESTR => commonDataObj
      case _: Expressions.CONST_STRING  => commonDataObj
      case _: Expressions.TRUE          => commonDataObj
      case _: Expressions.FALSE         => commonDataObj

      case x: Expressions.REF =>
        val additionalDataObj = jObj("name" -> Expressions.PART.toOption[String](x.key).getOrElse("").toString)
        js.Object.assign(additionalDataObj, commonDataObj)

      case Expressions.GETTER(_, ref, field, _, _, _) =>
        val additionalDataObj = jObj(
          "ref"   -> serExpr(ref),
          "field" -> serPartStr(field)
        )
        js.Object.assign(additionalDataObj, commonDataObj)

      case Expressions.BLOCK(_, dec, body, _, _) =>
        val additionalDataObj = jObj(
          "dec"  -> serDec(dec),
          "body" -> serExpr(body)
        )
        js.Object.assign(additionalDataObj, commonDataObj)

      case Expressions.IF(_, cond, ifTrue, ifFalse, _, _) =>
        val additionalDataObj = jObj(
          "cond"    -> serExpr(cond),
          "ifTrue"  -> serExpr(ifTrue),
          "ifFalse" -> serExpr(ifFalse)
        )
        js.Object.assign(additionalDataObj, commonDataObj)

      case Expressions.FUNCTION_CALL(_, name, args, _, _) =>
        val additionalDataObj = jObj(
          "name" -> serPartStr(name),
          "args" -> args.toJSArray.map(serExpr)
        )
        js.Object.assign(additionalDataObj, commonDataObj)

      case Expressions.FOLD(_, limit, value, acc, func, _, _) =>
        val additionalDataObj = jObj(
          "name" -> s"FOLD<$limit>",
          "args" -> js.Array(serExpr(value), serExpr(acc), func.key.toString: js.Any)
        )
        js.Object.assign(additionalDataObj, commonDataObj)

      case Expressions.MATCH(_, expr, cases, _, ctxOpt) =>
        val additionalDataObj = jObj(
          "expr"  -> serExpr(expr),
          "cases" -> cases.toJSArray.map(serMatchCase(_, ctxOpt.getOrElse(CompilerContext.empty)))
        )
        js.Object.assign(additionalDataObj, commonDataObj)

      case t => jObj("[not_supported]stringRepr" -> t.toString)
    }
  }

  def serMatchCase(c: Expressions.MATCH_CASE, simpleCtx: CompilerContext): js.Object = {
    val vars = c.pattern.subpatterns.collect { case (Expressions.TypedVar(Some(newVarName), caseType), _) =>
      (serPartStr(newVarName), serType(caseType))
    }
    jObj(
      "type"       -> "MATCH_CASE",
      "posStart"   -> c.position.start,
      "posEnd"     -> c.position.end,
      "varName"    -> vars.headOption.map(_._1).orUndefined,
      "varTypes"   -> vars.headOption.map(_._2).orUndefined,
      "resultType" -> c.resultType.getOrElse(NOTHING).toString,
      "expr"       -> serExpr(c.expr),
      "ctx"        -> serCtx(simpleCtx)
    )
  }

  def serCtx(ctx: CompilerContext): js.Object = {
    val r = js.Array[js.Dynamic]()
    def addInfo(decl: (String, CompilerContext.PositionedInfo)): Unit =
      if (decl._2.pos.start != -1)
        r += jObj(
          "name"     -> decl._1,
          "posStart" -> decl._2.pos.start,
          "posEnd"   -> decl._2.pos.end
        )
    ctx.varDefs.foreach(addInfo)
    ctx.functionDefs.foreach(addInfo)
    r
  }

  def serType(t: Type): js.Object =
    t match {
      case Expressions.AnyType(_) =>
        jObj(
          "typeName" -> "Any"
        )
      case Expressions.Single(name, parameter) =>
        jObj(
          "typeName"  -> serPartStr(name),
          "typeParam" -> parameter.map(serPart(serType)).orUndefined
        )
      case Expressions.Union(types) =>
        jObj(
          "isUnion"  -> "true",
          "typeList" -> types.toJSArray.map(serType)
        )
      case Expressions.Tuple(types) =>
        jObj(
          "isTuple"  -> "true",
          "typeList" -> types.toJSArray.map(serType)
        )
    }

  def serDec(dec: Expressions.Declaration): js.Object = {
    def serFuncArg(argName: PART[String], argType: Type): js.Object =
      jObj(
        "argName" -> serPartStr(argName),
        "type"    -> serType(argType)
      )

    dec match {
      case Expressions.LET(p, name, expr, _, _) =>
        jObj(
          "type"     -> "LET",
          "posStart" -> p.start,
          "posEnd"   -> p.end,
          "name"     -> serPartStr(name),
          "expr"     -> serExpr(expr)
        )
      case Expressions.FUNC(p, expr, name, args) =>
        jObj(
          "type"     -> "FUNC",
          "posStart" -> p.start,
          "posEnd"   -> p.end,
          "name"     -> serPartStr(name),
          "argList"  -> args.toJSArray.map(arg => serFuncArg(arg._1, arg._2)),
          "expr"     -> serExpr(expr)
        )
      case t => jObj("[not_supported]stringRepr" -> t.toString)
    }
  }

  def compilationErrToJs(err: CompilationError): js.Object = {
    jObj("posStart" -> err.start, "posEnd" -> err.end, "msg" -> err.message)
  }

  @nowarn("cat=deprecation")
  def toJs(ast: EXPR): js.Object = {
    def r(expr: EXPR): js.Object = {
      expr match {
        case CONST_LONG(t)      => jObj("type" -> "LONG", "value" -> t.toDouble)
        case GETTER(ref, field) => jObj("type" -> "GETTER", "ref" -> r(ref), "field" -> field)
        case CONST_BYTESTR(bs)  => jObj("type" -> "BYTESTR", "value" -> bs.arr.toJSArray)
        case CONST_STRING(s)    => jObj("type" -> "STRING", "value" -> s)
        case LET_BLOCK(let, body) =>
          jObj("type" -> "BLOCK", "let" -> jObj("name" -> let.name, "value" -> r(let.value)), "body" -> r(body))
        case BLOCK(LET(name, value), body) =>
          jObj("type" -> "BLOCK", "let" -> jObj("name" -> name, "value" -> r(value)), "body" -> r(body))
        case BLOCK(FUNC(name, args, body), nextBody) =>
          jObj(
            "type" -> "BLOCK",
            "func" -> jObj("name" -> name, "args" -> args.toJSArray, "body" -> r(body)),
            "body" -> r(nextBody)
          )
        case IF(cond, ifTrue, ifFalse) =>
          jObj("type" -> "IF", "condition" -> r(cond), "true" -> r(ifTrue), "false" -> r(ifFalse))
        case REF(key)         => jObj("type" -> "REF", "key" -> key)
        case CONST_BOOLEAN(b) => jObj("type" -> "BOOL", "value" -> b)
        case FUNCTION_CALL(function, args) =>
          jObj(
            "type" -> "CALL",
            "name" -> (function match {
              case Native(name)          => name.toString()
              case User(internalName, _) => internalName
            }),
            "args" -> args.toJSArray.map(r)
          )
        case t => jObj("[not_supported]stringRepr" -> t.toString)
      }
    }

    r(ast)
  }

  def toJs(c: DApp): js.Object = {
    toJs(TRUE) // later
  }

  def typeRepr(t: TYPE): js.Any = t match {
    case UNION(l, _) => l.toJSArray.map(typeRepr)
    case CASETYPEREF(name, fields, false) =>
      jObj("typeName" -> name, "fields" -> fields.toJSArray.map(f => jObj("name" -> f._1, "type" -> typeRepr(f._2))))
    case LIST(t) => jObj("listOf" -> typeRepr(t))
    case t       => t.toString
  }
}
