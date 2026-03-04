package com.wavesplatform

import JsApiUtils.*
import com.wavesplatform.lang.*
import com.wavesplatform.lang.directives.Directive.extractDirectives
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveParser, DirectiveSet}
import com.wavesplatform.lang.v1.ContractLimits

import com.wavesplatform.common.utils.Base64

import scala.scalajs.js
import scala.scalajs.js.Dynamic.literal as jObj
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.{Any, Dictionary}

object JsAPI {

  @JSExportTopLevel("getTypes", moduleID = "ride")
  def getTypes(ver: Int = 2, isTokenContext: Boolean = false, isContract: Boolean = false): js.Array[js.Object & js.Dynamic] =
    API
      .allTypes(ver, isTokenContext, isContract)
      .map(v => js.Dynamic.literal("name" -> v.name, "type" -> typeRepr(v)))
      .toJSArray

  @JSExportTopLevel("getVarsDoc", moduleID = "ride")
  def getVarsDoc(ver: Int = 2, isTokenContext: Boolean = false, isContract: Boolean = false): js.Array[js.Object & js.Dynamic] =
    API
      .allVars(ver, isTokenContext, isContract)
      .map { case (name, ft) =>
        js.Dynamic.literal(
          "name" -> name,
          "type" -> typeRepr(ft),
          "doc"  -> DocSource.varData((name, ver))
        )
      }
      .toJSArray

  @JSExportTopLevel("getFunctionsDoc", moduleID = "ride")
  def getFunctionsDoc(ver: Int = 2, isTokenContext: Boolean = false, isContract: Boolean = false): js.Array[js.Object & js.Dynamic] =
    API
      .allFunctions(ver, isTokenContext, isContract)
      .map { case (name, args, signature) =>
        val (funcDoc, paramsDoc, _) = DocSource.funcData((name, signature.args.map(_._2.toString).toList, ver))

        js.Dynamic.literal(
          "name"       -> name,
          "doc"        -> funcDoc,
          "resultType" -> typeRepr(signature.result),
          "args" -> (args zip signature.args zip paramsDoc).map { arg =>
            js.Dynamic.literal("name" -> arg._1._1, "type" -> typeRepr(arg._1._2._2), "doc" -> arg._2)
          }.toJSArray
        )
      }
      .toJSArray

  @JSExportTopLevel("contractLimits", moduleID = "ride")
  val contractLimits: js.Dynamic = {
    import ContractLimits.*
    js.Dynamic.literal(
      "MaxComplexityByVersion"                -> ((ver: Int) => MaxComplexityByVersion(DirectiveDictionary[StdLibVersion].idMap(ver))),
      "MaxAssetVerifierComplexityByVersion"   -> ((ver: Int) => MaxComplexityByVersion(DirectiveDictionary[StdLibVersion].idMap(ver))),
      "MaxAccountVerifierComplexityByVersion" -> ((ver: Int) => MaxAccountVerifierComplexityByVersion(DirectiveDictionary[StdLibVersion].idMap(ver))),
      "MaxCallableComplexityByVersion"        -> ((ver: Int) => MaxCallableComplexityByVersion(DirectiveDictionary[StdLibVersion].idMap(ver))),
      "MaxExprSizeInBytes"                    -> MaxExprSizeInBytes,
      "MaxContractSizeInBytes"                -> MaxContractSizeInBytesV6,
      "MaxInvokeScriptArgs"                   -> MaxInvokeScriptArgs,
      "MaxInvokeScriptSizeInBytes"            -> MaxInvokeScriptSizeInBytes,
      "MaxWriteSetSizeInBytes"                -> MaxWriteSetSizeInBytes,
      "MaxPaymentAmount"                      -> MaxCallableActionsAmountBeforeV6(V4),
      "MaxAttachedPaymentAmount"              -> MaxAttachedPaymentAmount
    )
  }

  @JSExportTopLevel("scriptInfo", moduleID = "ride")
  def scriptInfo(input: String): js.Dynamic = {
    val info = DirectiveParser(input)
      .flatMap(v => extractDirectives(v))
      .map { case DirectiveSet(ver, scriptType, contentType, imports) =>
        js.Dynamic.literal(
          "stdLibVersion" -> ver.id,
          "contentType"   -> contentType.id,
          "scriptType"    -> scriptType.id,
          "imports"       -> imports.fileNames.toJSArray
        )
      }
    info.fold(
      err => js.Dynamic.literal("error" -> err),
      identity
    )
  }

  @JSExportTopLevel("parseAndCompile", moduleID = "ride")
  def parseAndCompile(
      input: String,
      estimatorVersion: Int,
      needCompaction: Boolean = false,
      removeUnusedCode: Boolean = false,
      libraries: Dictionary[String] = Dictionary.empty
  ): js.Dynamic =
    API
      .parseAndCompile(input, estimatorVersion, needCompaction, removeUnusedCode, libraries.toMap)
      .fold(
        e => js.Dynamic.literal("error" -> e),
        {
          case CompileAndParseResult.Expression(bytes, complexity, expr, errors) =>
            js.Dynamic.literal(
              "result"     -> Global.toBuffer(bytes),
              "base64"     -> Base64.encode(bytes),
              "size"       -> bytes.length,
              "complexity" -> complexity.toDouble,
              "exprAst"    -> expressionScriptToJs(expr),
              "errorList"  -> errors.map(compilationErrToJs).toJSArray
            )
          case CompileAndParseResult.Contract(bytes, verifierComplexity, callableComplexities, expr, errors) =>
            js.Dynamic.literal(
              "result"           -> Global.toBuffer(bytes),
              "base64"           -> Base64.encode(bytes),
              "size"             -> bytes.length,
              "complexity"       -> verifierComplexity.toDouble,
              "complexityByFunc" -> callableComplexities.view.mapValues(_.toDouble).toMap.toJSDictionary,
              "dAppAst"          -> dAppToJs(expr),
              "errorList"        -> errors.map(compilationErrToJs).toJSArray
            )
          case CompileAndParseResult.Library(bytes, complexity, expr) =>
            js.Dynamic.literal(
              "result"     -> Global.toBuffer(bytes),
              "base64"     -> Base64.encode(bytes),
              "size"       -> bytes.length,
              "ast"        -> toJs(expr),
              "complexity" -> complexity.toDouble
            )
        }
      )

  @JSExportTopLevel("compile", moduleID = "ride")
  def compile(
      input: String,
      estimatorVersion: Int,
      needCompaction: Boolean = false,
      removeUnusedCode: Boolean = false,
      libraries: Dictionary[String] = Dictionary.empty
  ): js.Dynamic =
    (for {
      estimator <- API.estimatorByVersion(estimatorVersion)
      result    <- API.compile(input, estimator, needCompaction, removeUnusedCode, libraries.toMap)
    } yield result)
      .fold(
        e => js.Dynamic.literal("error" -> e),
        {
          case CompileResult.Expression(_, bytes, complexity, expr, error, _) =>
            val resultFields: Seq[(String, Any)] = Seq(
              "result"     -> Global.toBuffer(bytes),
              "base64"     -> Base64.encode(bytes),
              "size"       -> bytes.length,
              "ast"        -> toJs(expr),
              "complexity" -> complexity.toDouble
            )
            val errorFieldOpt: Seq[(String, Any)] =
              error
                .fold(
                  error => Seq("error" -> error),
                  _ => Seq()
                )
            js.Dynamic.literal.applyDynamic("apply")(resultFields ++ errorFieldOpt*)
          case CompileResult.Library(_, bytes, complexity, expr) =>
            js.Dynamic.literal(
              "result"     -> Global.toBuffer(bytes),
              "base64"     -> Base64.encode(bytes),
              "size"       -> bytes.length,
              "ast"        -> toJs(expr),
              "complexity" -> complexity.toDouble
            )
          case CompileResult.DApp(_, di, meta, error) =>
            val mappedMeta =
              meta.argsWithFuncName.map { case (func, argsWithName) =>
                func -> argsWithName.map { case (arg, argType) => arg -> argType.name }.toJSArray
              }.toJSDictionary

            val compactNameToOriginalName: Map[String, String] =
              di.dApp.meta.compactNameAndOriginalNamePairList.map(pair => pair.compactName -> pair.originalName).toMap

            val resultFields: Seq[(String, Any)] = Seq(
              "result"               -> Global.toBuffer(di.bytes),
              "base64"               -> Base64.encode(di.bytes),
              "size"                 -> di.bytes.length,
              "ast"                  -> toJs(),
              "meta"                 -> mappedMeta,
              "complexity"           -> di.maxComplexity._2.toDouble,
              "verifierComplexity"   -> di.verifierComplexity.toDouble,
              "callableComplexities" -> di.callableComplexities.view.mapValues(_.toDouble).toMap.toJSDictionary,
              "userFunctionComplexities" -> di.userFunctionComplexities.map { case (name, complexity) =>
                compactNameToOriginalName.getOrElse(name, name) -> complexity.toDouble
              }.toJSDictionary,
              "globalVariableComplexities" -> di.globalVariableComplexities.map { case (name, complexity) =>
                compactNameToOriginalName.getOrElse(name, name) -> complexity.toDouble
              }.toJSDictionary
            )
            val errorFieldOpt: Seq[(String, Any)] =
              error
                .fold(
                  error => Seq("error" -> error),
                  _ => Seq()
                )
            js.Dynamic.literal.applyDynamic("apply")((resultFields ++ errorFieldOpt)*)
        }
      )

  @JSExportTopLevel("decompile", moduleID = "ride")
  def decompile(input: String): js.Dynamic =
    Global
      .decompile(input)
      .fold(
        err => jObj("error" -> err.m),
        scriptText => jObj("result" -> scriptText)
      )

  @JSExportTopLevel("nodeVersion", moduleID = "ride")
  def nodeVersion(): js.Dynamic = js.Dynamic.literal("version" -> Version.VersionString)

  @JSExportTopLevel("version", moduleID = "ride")
  val version: String = Version.VersionString

  @JSExportTopLevel("flattenCompilationResult", moduleID = "ride")
  def flattenCompilationResult(compiled: js.Dynamic): js.Dynamic =
    if (!js.isUndefined(compiled.error))
      js.Dynamic.literal("error" -> compiled.error)
    else {
      val flat = js.Object.assign(js.Dynamic.literal(), compiled.result.asInstanceOf[js.Object]).asInstanceOf[js.Dynamic]
      flat.error = js.undefined
      flat
    }
}
