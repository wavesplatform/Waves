package com.wavesplatform

import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.CompilationError
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.{CASETYPEREF, FINAL, LIST, NOTHING, TYPE, UNION}
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.lang.v1.parser.Expressions.{PART, Pos, Type}

import scala.scalajs.js
import scala.scalajs.js.Any
import scala.scalajs.js.Dynamic.{literal => jObj}
import scala.scalajs.js.JSConverters._

object JsApiUtils {

  def serPart[T](f: T => js.Any)(part: PART[T]): js.Object = {
    val partValue = Expressions.PART.toOption(part).fold(null: Any)(f)
    jObj.applyDynamic("apply")(
      "value"    -> partValue,
      "posStart" -> part.position.start,
      "posEnd"   -> part.position.end
    )
  }

  def serPartStr(part: PART[String]): js.Object = {
    val partValue = Expressions.PART.toOption(part).getOrElse("").toString
    jObj.applyDynamic("apply")(
      "value"    -> partValue,
      "posStart" -> part.position.start,
      "posEnd"   -> part.position.end
    )
  }

  def dAppToJs(ast: Expressions.DAPP): js.Object = {

    def serAnnFunc(annFunc: Expressions.ANNOTATEDFUNC): js.Object = {

      def serAnnotation(ann: Expressions.ANNOTATION): js.Object = {
        jObj.applyDynamic("apply")(
          "type"     -> "ANNOTATION",
          "posStart" -> annFunc.position.start,
          "posEnd"   -> annFunc.position.end,
          "name"     -> serPartStr(ann.name),
          "argList"  -> ann.args.map(serPartStr).toJSArray
        )
      }

      jObj.applyDynamic("apply")(
        "type"     -> "ANNOTATEDFUNC",
        "posStart" -> annFunc.position.start,
        "posEnd"   -> annFunc.position.end,
        "annList"  -> annFunc.anns.map(serAnnotation).toJSArray,
        "func"     -> serDec(annFunc.f)
      )
    }

    jObj.applyDynamic("apply")(
      "type"        -> "DAPP",
      "posStart"    -> ast.position.start,
      "posEnd"      -> ast.position.end,
      "decList"     -> ast.decs.map(serDec).toJSArray,
      "annFuncList" -> ast.fs.map(serAnnFunc).toJSArray
    )
  }

  def expressionScriptToJs(ast: Expressions.SCRIPT): js.Object = {
    jObj.applyDynamic("apply")(
      "type"     -> "SCRIPT",
      "posStart" -> ast.position.start,
      "posEnd"   -> ast.position.end,
      "expr"     -> serExpr(ast.expr)
    )
  }

  def serExpr(expr: Expressions.EXPR): js.Object = {

    def serType(t: FINAL): js.Object = {
      t match {
        case ut: UNION =>
          jObj.applyDynamic("apply")(
            "unionTypes" -> ut.typeList.map(serType(_)).toJSArray
          )
        case lt: LIST =>
          jObj.applyDynamic("apply")(
            "listOf" -> serType(lt.innerType)
          )
        case someT =>
          jObj.applyDynamic("apply")(
            "type" -> someT.toString()
          )
      }
    }

    val commonDataObj = jObj.applyDynamic("apply")(
      "type"       -> expr.getName,
      "posStart"   -> expr.position.start,
      "posEnd"     -> expr.position.end,
      "resultType" -> serType(expr.resultType.getOrElse(NOTHING)),
      "ctx"        -> serCtx(expr.ctxOpt.getOrElse(Map.empty))
    )

    expr match {
      case x: Expressions.CONST_LONG    => commonDataObj
      case x: Expressions.CONST_BYTESTR => commonDataObj
      case x: Expressions.CONST_STRING  => commonDataObj
      case x: Expressions.TRUE          => commonDataObj
      case x: Expressions.FALSE         => commonDataObj

      case x: Expressions.REF => {
        val additionalDataObj = jObj.applyDynamic("apply")("name" -> Expressions.PART.toOption[String](x.key).getOrElse("").toString)
        mergeJSObjects(commonDataObj, additionalDataObj)
      }

      case Expressions.GETTER(_, ref, field, _, _, _) => {
        val additionalDataObj = jObj.applyDynamic("apply")(
          "ref"   -> serExpr(ref),
          "field" -> serPartStr(field)
        )
        mergeJSObjects(commonDataObj, additionalDataObj)
      }

      case Expressions.BLOCK(_, dec, body, _, _) => {
        val additionalDataObj = jObj.applyDynamic("apply")(
          "dec"  -> serDec(dec),
          "body" -> serExpr(body)
        )
        mergeJSObjects(commonDataObj, additionalDataObj)
      }

      case Expressions.IF(_, cond, ifTrue, ifFalse, _, _) => {
        val additionalDataObj = jObj.applyDynamic("apply")(
          "cond"    -> serExpr(cond),
          "ifTrue"  -> serExpr(ifTrue),
          "ifFalse" -> serExpr(ifFalse)
        )
        mergeJSObjects(commonDataObj, additionalDataObj)
      }

      case Expressions.FUNCTION_CALL(_, name, args, _, _) => {
        val additionalDataObj = jObj.applyDynamic("apply")(
          "name" -> serPartStr(name),
          "args" -> args.map(serExpr).toJSArray
        )
        mergeJSObjects(commonDataObj, additionalDataObj)
      }

      case Expressions.FOLD(_, limit, value, acc, func, _, _) =>
        val additionalDataObj = jObj.applyDynamic("apply")(
          "name" -> s"FOLD<$limit>",
          "args" -> js.Array(serExpr(value), serExpr(acc), func.key.toString: js.Any)
        )
        mergeJSObjects(commonDataObj, additionalDataObj)

      case Expressions.MATCH(_, expr, cases, _, ctxOpt) => {
        val additionalDataObj = jObj.applyDynamic("apply")(
          "expr"  -> serExpr(expr),
          "cases" -> cases.map(serMatchCase(_, ctxOpt.getOrElse(Map.empty))).toJSArray
        )
        mergeJSObjects(commonDataObj, additionalDataObj)
      }

      case t => jObj.applyDynamic("apply")("[not_supported]stringRepr" -> t.toString)
    }
  }

  def mergeJSObjects(objs: js.Dynamic*): js.Object = {
    val result = js.Dictionary.empty[Any]
    for (source <- objs) {
      for ((key, value) <- source.asInstanceOf[js.Dictionary[Any]])
        result(key) = value
    }
    result.asInstanceOf[js.Object]
  }

  def serMatchCase(c: Expressions.MATCH_CASE, simpleCtx: Map[String, Pos]): js.Object = {
    val vars = c.pattern.subpatterns.collect { case (Expressions.TypedVar(Some(newVarName), caseType), _) =>
      (serPartStr(newVarName), serType(caseType))
    }
    jObj.applyDynamic("apply")(
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

  def serCtx(simpleCtx: Map[String, Pos]): js.Object = {
    simpleCtx.map { ctxEl =>
      jObj.applyDynamic("apply")(
        "name"     -> ctxEl._1,
        "posStart" -> ctxEl._2.start,
        "posEnd"   -> ctxEl._2.end
      )
    }.toJSArray
  }

  def serType(t: Type): js.Object =
    t match {
      case Expressions.AnyType(_) =>
        jObj.applyDynamic("apply")(
          "typeName" -> "Any"
        )
      case Expressions.Single(name, parameter) =>
        jObj.applyDynamic("apply")(
          "typeName"  -> serPartStr(name),
          "typeParam" -> parameter.map(serPart(serType)).orUndefined
        )
      case Expressions.Union(types) =>
        jObj.applyDynamic("apply")(
          "isUnion"  -> "true",
          "typeList" -> types.map(serType).toJSArray
        )
      case Expressions.Tuple(types) =>
        jObj.applyDynamic("apply")(
          "isTuple"  -> "true",
          "typeList" -> types.map(serType).toJSArray
        )
    }

  def serDec(dec: Expressions.Declaration): js.Object = {
    def serFuncArg(argName: PART[String], argType: Type): js.Object =
      jObj.applyDynamic("apply")(
        "argName" -> serPartStr(argName),
        "type"    -> serType(argType)
      )

    dec match {
      case Expressions.LET(p, name, expr, _, _) =>
        jObj.applyDynamic("apply")(
          "type"     -> "LET",
          "posStart" -> p.start,
          "posEnd"   -> p.end,
          "name"     -> serPartStr(name),
          "expr"     -> serExpr(expr)
        )
      case Expressions.FUNC(p, expr, name, args) =>
        jObj.applyDynamic("apply")(
          "type"     -> "FUNC",
          "posStart" -> p.start,
          "posEnd"   -> p.end,
          "name"     -> serPartStr(name),
          "argList"  -> args.map(arg => serFuncArg(arg._1, arg._2)).toJSArray,
          "expr"     -> serExpr(expr)
        )
      case t => jObj.applyDynamic("apply")("[not_supported]stringRepr" -> t.toString)
    }
  }

  def compilationErrToJs(err: CompilationError): js.Object = {
    jObj.applyDynamic("apply")("posStart" -> err.start, "posEnd" -> err.end, "msg" -> err.message)
  }

  def toJs(ast: EXPR): js.Object = {
    def r(expr: EXPR): js.Object = {
      expr match {
        case CONST_LONG(t)      => jObj.applyDynamic("apply")("type" -> "LONG", "value" -> t.toDouble)
        case GETTER(ref, field) => jObj.applyDynamic("apply")("type" -> "GETTER", "ref" -> r(ref), "field" -> field)
        case CONST_BYTESTR(bs)  => jObj.applyDynamic("apply")("type" -> "BYTESTR", "value" -> bs.arr.toJSArray)
        case CONST_STRING(s)    => jObj.applyDynamic("apply")("type" -> "STRING", "value" -> s)
        case LET_BLOCK(let, body) =>
          jObj.applyDynamic("apply")("type" -> "BLOCK", "let" -> jObj("name" -> let.name, "value" -> r(let.value)), "body" -> r(body))
        case IF(cond, ifTrue, ifFalse) =>
          jObj.applyDynamic("apply")("type" -> "IF", "condition" -> r(cond), "true" -> r(ifTrue), "false" -> r(ifFalse))
        case REF(key)         => jObj.applyDynamic("apply")("type" -> "REF", "key" -> key)
        case CONST_BOOLEAN(b) => jObj.applyDynamic("apply")("type" -> "BOOL", "value" -> b)
        case FUNCTION_CALL(function, args) =>
          jObj.applyDynamic("apply")(
            "type" -> "CALL",
            "name" -> (function match {
              case Native(name)          => name.toString()
              case User(internalName, _) => internalName
            }),
            "args" -> args.map(r).toJSArray
          )
        case t => jObj.applyDynamic("apply")("[not_supported]stringRepr" -> t.toString)
      }
    }

    r(ast)
  }

  def toJs(c: DApp): js.Object = {
    toJs(TRUE) // later
  }

  def typeRepr(t: TYPE): js.Any = t match {
    case UNION(l, _) => l.map(typeRepr).toJSArray
    case CASETYPEREF(name, fields, false) =>
      js.Dynamic.literal("typeName" -> name, "fields" -> fields.map(f => js.Dynamic.literal("name" -> f._1, "type" -> typeRepr(f._2))).toJSArray)
    case LIST(t) => js.Dynamic.literal("listOf" -> typeRepr(t))
    case t       => t.toString
  }
}
