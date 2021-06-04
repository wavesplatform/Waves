import JsApiUtils._
import cats.kernel.Monoid
import com.wavesplatform.DocSource
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.Directive.extractDirectives
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveParser, DirectiveSet}
import com.wavesplatform.lang.script.ScriptPreprocessor
import com.wavesplatform.lang.v1.BaseGlobal.DAppInfo
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.repl.Repl
import com.wavesplatform.lang.v1.repl.node.http.NodeConnectionSettings
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.{CTX, ContractLimits}
import com.wavesplatform.lang.{Global, Version}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.Dynamic.{literal => jObj}
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.{Any, Dictionary, Promise, UndefOr}

object JsAPI {

  private def wavesContext(v: StdLibVersion, isTokenContext: Boolean, isContract: Boolean) =
    WavesContext.build(
      Global,
      DirectiveSet(v, ScriptType.isAssetScript(isTokenContext), if (isContract) DAppType else Expression)
        .explicitGet()
    )

  private def cryptoContext(version: StdLibVersion) = CryptoContext.build(Global, version).withEnvironment[Environment]
  private def pureContext(version: StdLibVersion)   = PureContext.build(version, fixUnicodeFunctions = true).withEnvironment[Environment]

  private val fullDAppContext: Map[StdLibVersion, CTX[Environment]] =
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V3)
      .map(v => (v, buildContractContext(v)))
      .toMap

  private def buildScriptContext(v: StdLibVersion, isTokenContext: Boolean, isContract: Boolean): CTX[Environment] =
    Monoid.combineAll(Seq(pureContext(v), cryptoContext(v), wavesContext(v, isTokenContext, isContract)))

  private def buildContractContext(v: StdLibVersion): CTX[Environment] =
    Monoid.combineAll(Seq(pureContext(v), cryptoContext(v), wavesContext(v, false, true)))

  @JSExportTopLevel("getTypes")
  def getTypes(ver: Int = 2, isTokenContext: Boolean = false, isContract: Boolean = false): js.Array[js.Object with js.Dynamic] =
    buildScriptContext(DirectiveDictionary[StdLibVersion].idMap(ver), isTokenContext, isContract).types
      .map(v => js.Dynamic.literal("name" -> v.name, "type" -> typeRepr(v)))
      .toJSArray

  @JSExportTopLevel("getVarsDoc")
  def getVarsDoc(ver: Int = 2, isTokenContext: Boolean = false, isContract: Boolean = false): js.Array[js.Object with js.Dynamic] =
    buildScriptContext(DirectiveDictionary[StdLibVersion].idMap(ver), isTokenContext, isContract).vars
      .filterNot(_._1.startsWith("_"))
      .map(
        v =>
          js.Dynamic.literal(
            "name" -> v._1,
            "type" -> typeRepr(v._2._1),
            "doc"  -> DocSource.varData((v._1, ver))
          )
      )
      .toJSArray

  @JSExportTopLevel("getFunctionsDoc")
  def getFunctionsDoc(ver: Int = 2, isTokenContext: Boolean = false, isContract: Boolean = false): js.Array[js.Object with js.Dynamic] =
    buildScriptContext(DirectiveDictionary[StdLibVersion].idMap(ver), isTokenContext, isContract).functions
      .filterNot(_.name.startsWith("_"))
      .map(f => {
        val (funcDoc, paramsDoc, _) = DocSource.funcData((f.name, f.signature.args.map(_._2.toString).toList, ver))
        js.Dynamic.literal(
          "name"       -> f.name,
          "doc"        -> funcDoc,
          "resultType" -> typeRepr(f.signature.result),
          "args" -> (f.args zip f.signature.args zip paramsDoc).map { arg =>
            js.Dynamic.literal("name" -> arg._1._1, "type" -> typeRepr(arg._1._2._2), "doc" -> arg._2)
          }.toJSArray
        )
      })
      .toJSArray

  @JSExportTopLevel("contractLimits")
  def contractLimits(): js.Dynamic = {
    import ContractLimits._
    js.Dynamic.literal(
      "MaxComplexityByVersion"                -> ((ver: Int) => MaxComplexityByVersion(DirectiveDictionary[StdLibVersion].idMap(ver))),
      "MaxAssetVerifierComplexityByVersion"   -> ((ver: Int) => MaxComplexityByVersion(DirectiveDictionary[StdLibVersion].idMap(ver))),
      "MaxAccountVerifierComplexityByVersion" -> ((ver: Int) => MaxAccountVerifierComplexityByVersion(DirectiveDictionary[StdLibVersion].idMap(ver))),
      "MaxCallableComplexityByVersion"        -> ((ver: Int) => MaxCallableComplexityByVersion(DirectiveDictionary[StdLibVersion].idMap(ver))),
      "MaxExprSizeInBytes"                    -> MaxExprSizeInBytes,
      "MaxContractSizeInBytes"                -> MaxContractSizeInBytes,
      "MaxInvokeScriptArgs"                   -> MaxInvokeScriptArgs,
      "MaxInvokeScriptSizeInBytes"            -> MaxInvokeScriptSizeInBytes,
      "MaxWriteSetSizeInBytes"                -> MaxWriteSetSizeInBytes,
      "MaxPaymentAmount"                      -> MaxCallableActionsAmount(V4),
      "MaxAttachedPaymentAmount"              -> MaxAttachedPaymentAmount
    )
  }

  @JSExportTopLevel("scriptInfo")
  def scriptInfo(input: String): js.Dynamic = {
    val info = DirectiveParser(input)
      .flatMap(v => extractDirectives(v))
      .map {
        case DirectiveSet(ver, scriptType, contentType, imports) =>
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

  @JSExportTopLevel("parseAndCompile")
  def parseAndCompile(
      input: String,
      estimatorVersion: Int,
      needCompaction: Boolean = false,
      removeUnusedCode: Boolean = false,
      libraries: Dictionary[String] = Dictionary.empty
  ): js.Dynamic = {
    val r = for {
      estimatorVer <- Either.cond(
        estimatorVersion > 0 && estimatorVersion <= ScriptEstimator.all.length,
        estimatorVersion,
        s"Version of estimator must be not greater than ${ScriptEstimator.all.length}"
      )
      directives  <- DirectiveParser(input)
      ds          <- extractDirectives(directives)
      linkedInput <- ScriptPreprocessor(input, libraries.toMap, ds.imports)
      compiled    <- parseAndCompileScript(ds, linkedInput, ScriptEstimator.all.toIndexedSeq(estimatorVer - 1), needCompaction, removeUnusedCode)
    } yield compiled
    r.fold(
      e => js.Dynamic.literal("error" -> e),
      identity
    )
  }

  private def parseAndCompileScript(
      ds: DirectiveSet,
      input: String,
      estimator: ScriptEstimator,
      needCompaction: Boolean,
      removeUnusedCode: Boolean
  ) = {
    val stdLibVer = ds.stdLibVersion
    val isAsset   = ds.scriptType == Asset
    ds.contentType match {
      case Expression =>
        val ctx = buildScriptContext(stdLibVer, isAsset, ds.contentType == DAppType)
        Global
          .parseAndCompileExpression(input, ctx.compilerContext, Global.LetBlockVersions.contains(stdLibVer), stdLibVer, estimator)
          .map {
            case (bytes, complexity, exprScript, compErrorList) =>
              js.Dynamic.literal(
                "result"     -> Global.toBuffer(bytes),
                "complexity" -> complexity,
                "exprAst"    -> expressionScriptToJs(exprScript),
                "errorList"  -> compErrorList.map(compilationErrToJs).toJSArray
              )
          }
      case Library =>
        val ctx = buildScriptContext(stdLibVer, isAsset, ds.contentType == DAppType)
        Global
          .compileDecls(input, ctx.compilerContext, stdLibVer, estimator)
          .map {
            case (bytes, ast, complexity) =>
              js.Dynamic.literal(
                "result"     -> Global.toBuffer(bytes),
                "ast"        -> toJs(ast),
                "complexity" -> complexity
              )
          }
      case DAppType =>
        Global
          .parseAndCompileContract(input, fullDAppContext(ds.stdLibVersion).compilerContext, stdLibVer, estimator, needCompaction, removeUnusedCode)
          .map {
            case (bytes, complexityWithMap, exprDApp, compErrorList) =>
              js.Dynamic.literal(
                "result"           -> Global.toBuffer(bytes),
                "complexity"       -> complexityWithMap._1,
                "complexityByFunc" -> complexityWithMap._2.view.mapValues(c => c: Any).toMap.toJSDictionary,
                "dAppAst"          -> dAppToJs(exprDApp),
                "errorList"        -> compErrorList.map(compilationErrToJs).toJSArray
              )
          }
    }
  }

  @JSExportTopLevel("compile")
  def compile(
      input: String,
      estimatorVersion: Int,
      needCompaction: Boolean = false,
      removeUnusedCode: Boolean = false,
      libraries: Dictionary[String] = Dictionary.empty
  ): js.Dynamic = {
    val r = for {
      estimatorVer <- Either.cond(
        estimatorVersion > 0 && estimatorVersion <= ScriptEstimator.all.length,
        estimatorVersion,
        s"Version of estimator must be not greater than ${ScriptEstimator.all.length}"
      )
      directives  <- DirectiveParser(input)
      ds          <- extractDirectives(directives)
      linkedInput <- ScriptPreprocessor(input, libraries.toMap, ds.imports)
      compiled    <- compileScript(ds, linkedInput, ScriptEstimator.all.toIndexedSeq(estimatorVer - 1), needCompaction, removeUnusedCode)
    } yield compiled
    r.fold(
      e => js.Dynamic.literal("error" -> e),
      identity
    )
  }

  private def compileScript(
      ds: DirectiveSet,
      input: String,
      estimator: ScriptEstimator,
      needCompaction: Boolean,
      removeUnusedCode: Boolean
  ): Either[String, js.Object with js.Dynamic] = {
    val version = ds.stdLibVersion
    val isAsset = ds.scriptType == Asset
    ds.contentType match {
      case Expression =>
        val ctx = buildScriptContext(version, isAsset, ds.contentType == DAppType)
        Global
          .compileExpression(input, ctx.compilerContext, version, estimator)
          .map {
            case (bytes, expr, complexity) =>
              val resultFields: Seq[(String, Any)] = Seq(
                "result"     -> Global.toBuffer(bytes),
                "ast"        -> toJs(expr),
                "complexity" -> complexity
              )
              val errorFieldOpt: Seq[(String, Any)] =
                Global
                  .checkExpr(expr, complexity, version, isAsset, estimator)
                  .fold(
                    error => Seq("error" -> error),
                    _ => Seq()
                  )
              js.Dynamic.literal.applyDynamic("apply")(resultFields ++ errorFieldOpt: _*)
          }
      case Library =>
        val ctx = buildScriptContext(version, isAsset, ds.contentType == DAppType)
        Global
          .compileDecls(input, ctx.compilerContext, version, estimator)
          .map {
            case (bytes, expr, complexity) =>
              js.Dynamic.literal(
                "result"     -> Global.toBuffer(bytes),
                "ast"        -> toJs(expr),
                "complexity" -> complexity
              )
          }
      case DAppType =>
        // Just ignore stdlib version here
        Global
          .compileContract(input, fullDAppContext(ds.stdLibVersion).compilerContext, version, estimator, needCompaction, removeUnusedCode)
          .map {
            case DAppInfo(
                bytes,
                dApp,
                maxComplexityFunc @ (_, maxComplexity),
                annotatedComplexities,
                verifierComplexity,
                callableComplexities,
                userFunctionComplexities,
                globalVariableComplexities
                ) =>
              val resultFields: Seq[(String, Any)] = Seq(
                "result"                     -> Global.toBuffer(bytes),
                "ast"                        -> toJs(dApp),
                "complexity"                 -> maxComplexity,
                "verifierComplexity"         -> verifierComplexity,
                "callableComplexities"       -> callableComplexities.view.mapValues(c => c: Any).toMap.toJSDictionary,
                "userFunctionComplexities"   -> userFunctionComplexities.view.mapValues(c => c: Any).toMap.toJSDictionary,
                "globalVariableComplexities" -> globalVariableComplexities.view.mapValues(c => c: Any).toMap.toJSDictionary
              )
              val errorFieldOpt: Seq[(String, Any)] = {
                Global
                  .checkContract(version, dApp, maxComplexityFunc, annotatedComplexities, estimator)
                  .fold(
                    error => Seq("error" -> error),
                    _ => Seq()
                  )
              }
              js.Dynamic.literal.applyDynamic("apply")(resultFields ++ errorFieldOpt: _*)
          }
    }
  }

  @JSExportTopLevel("decompile")
  def decompile(input: String): js.Dynamic =
    Global
      .decompile(input)
      .fold(
        err => jObj("error"         -> err.m),
        scriptText => jObj("result" -> scriptText)
      )

  @JSExportTopLevel("nodeVersion")
  def nodeVersion(): js.Dynamic = js.Dynamic.literal("version" -> Version.VersionString)

  @JSExportTopLevel("repl")
  def repl(
    settings: UndefOr[NodeConnectionSettings],
    libraries: js.Array[String] = js.Array()
  ): js.Dynamic = asJs(Repl(settings.toOption, libraries.toList))

  private def asJs(repl: Repl): js.Dynamic =
    jObj(
      "evaluate"    -> (repl.execute _ andThen mapResult),
      "info"        -> repl.info _,
      "totalInfo"   -> (() => repl.totalInfo),
      "clear"       -> repl.clear _,
      "reconfigure" -> (repl.reconfigure _ andThen asJs)
    )

  private def mapResult(eval: Future[Either[String, String]]): Promise[js.Object with js.Dynamic] =
    eval
      .map(
        _.fold(
          e => jObj("error"  -> e),
          r => jObj("result" -> r)
        )
      )
      .toJSPromise

}
