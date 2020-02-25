import JsApiUtils._
import cats.kernel.Monoid
import com.wavesplatform.DocSource
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.contract.meta.RecKeyValueFolder
import com.wavesplatform.lang.directives.Directive.extractDirectives
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveParser, DirectiveSet}
import com.wavesplatform.lang.script.ScriptPreprocessor
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
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
      DirectiveSet(v, ScriptType.isAssetScript(isTokenContext), if (isContract) DAppType else Expression)
        .explicitGet()
    )

  private def cryptoContext(version: StdLibVersion) = CryptoContext.build(Global, version).withEnvironment[Environment]
  private def pureContext(version: StdLibVersion)   = PureContext.build(Global, version).withEnvironment[Environment]
  private val letBLockVersions: Set[StdLibVersion]  = Set(V1, V2)

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
      .map(f => {
        val (funcDoc, paramsDoc) = DocSource.funcData((f.name, f.signature.args.map(_._2.toString).toList, ver))
        js.Dynamic.literal(
          "name"       -> f.name,
          "doc"        -> funcDoc,
          "resultType" -> typeRepr(f.signature.result),
          "args" -> (f.args, f.signature.args, paramsDoc).zipped.toList.map { arg =>
            js.Dynamic.literal("name" -> arg._1, "type" -> typeRepr(arg._2._2), "doc" -> arg._3)
          }.toJSArray
        )
      })
      .toJSArray

  @JSExportTopLevel("contractLimits")
  def contractLimits(): js.Dynamic = {
    js.Dynamic.literal(
      "MaxComplexityByVersion"     -> ((ver: Int) => ContractLimits.MaxComplexityByVersion(DirectiveDictionary[StdLibVersion].idMap(ver))),
      "MaxExprSizeInBytes"         -> ContractLimits.MaxExprSizeInBytes,
      "MaxContractSizeInBytes"     -> ContractLimits.MaxContractSizeInBytes,
      "MaxInvokeScriptArgs"        -> ContractLimits.MaxInvokeScriptArgs,
      "MaxInvokeScriptSizeInBytes" -> ContractLimits.MaxInvokeScriptSizeInBytes,
      "MaxWriteSetSizeInBytes"     -> ContractLimits.MaxWriteSetSizeInBytes,
      "MaxPaymentAmount"           -> ContractLimits.MaxCallableActionsAmount,
      "MaxAttachedPaymentAmount"   -> ContractLimits.MaxAttachedPaymentAmount
    )
  }

  @JSExportTopLevel("scriptInfo")
  def scriptInfo(input: String): js.Dynamic = {
    val info = DirectiveParser(input)
      .flatMap(extractDirectives)
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
      libraries: Dictionary[String] = Dictionary.empty
  ): js.Dynamic = {
    val r = for {
      directives  <- DirectiveParser(input)
      ds          <- extractDirectives(directives)
      linkedInput <- ScriptPreprocessor(input, libraries.toMap, ds.imports)
      compiled    <- parseAndCompileScript(ds, linkedInput)
    } yield compiled
    r.fold(
      e => js.Dynamic.literal("error" -> e),
      identity
    )
  }

  private def parseAndCompileScript(ds: DirectiveSet, input: String) = {
    val stdLibVer = ds.stdLibVersion
    ds.contentType match {
      case Expression =>
        val ctx = buildScriptContext(stdLibVer, ds.scriptType == Asset, ds.contentType == DAppType)
        Global
          .parseAndCompileExpression(input, ctx.compilerContext, letBLockVersions.contains(stdLibVer), stdLibVer, estimator)
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
        val ctx = buildScriptContext(stdLibVer, ds.scriptType == Asset, ds.contentType == DAppType)
        Global
          .compileDecls(input, ctx.compilerContext, letBLockVersions.contains(stdLibVer), stdLibVer, estimator)
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
          .parseAndCompileContract(input, fullDAppContext(ds.stdLibVersion).compilerContext, stdLibVer, estimator)
          .map {
            case (bytes, complexityWithMap, exprDApp, compErrorList) =>
              js.Dynamic.literal(
                "result"           -> Global.toBuffer(bytes),
                "complexity"       -> complexityWithMap._1,
                "complexityByFunc" -> complexityWithMap._2.toJSDictionary,
                "dAppAst"          -> dAppToJs(exprDApp),
                "errorList"        -> compErrorList.map(compilationErrToJs).toJSArray
              )
          }
    }
  }

  @JSExportTopLevel("compile")
  def compile(
      input: String,
      libraries: Dictionary[String] = Dictionary.empty
  ): js.Dynamic = {
    val r = for {
      directives  <- DirectiveParser(input)
      ds          <- extractDirectives(directives)
      linkedInput <- ScriptPreprocessor(input, libraries.toMap, ds.imports)
      compiled    <- compileScript(ds, linkedInput)
    } yield compiled
    r.fold(
      e => js.Dynamic.literal("error" -> e),
      identity
    )
  }

  val estimator = ScriptEstimatorV2

  private def compileScript(ds: DirectiveSet, input: String) = {
    val ver = ds.stdLibVersion
    ds.contentType match {
      case Expression =>
        val ctx = buildScriptContext(ver, ds.scriptType == Asset, ds.contentType == DAppType)
        Global
          .compileExpression(input, ctx.compilerContext, letBLockVersions.contains(ver), ver, estimator)
          .map {
            case (bytes, ast, complexity) =>
              js.Dynamic.literal(
                "result"     -> Global.toBuffer(bytes),
                "ast"        -> toJs(ast),
                "complexity" -> complexity
              )
          }
      case Library =>
        val ctx = buildScriptContext(ver, ds.scriptType == Asset, ds.contentType == DAppType)
        Global
          .compileDecls(input, ctx.compilerContext, letBLockVersions.contains(ver), ver, estimator)
          .map {
            case (bytes, ast, complexity) =>
              js.Dynamic.literal(
                "result"     -> Global.toBuffer(bytes),
                "ast"        -> toJs(ast),
                "complexity" -> complexity
              )
          }
      case DAppType =>
        // Just ignore stdlib version here
        Global
          .compileContract(input, fullDAppContext(ds.stdLibVersion).compilerContext, ver, estimator)
          .map {
            case (bytes, ast, complexity, complexityByFunc) =>
              js.Dynamic.literal(
                "result"           -> Global.toBuffer(bytes),
                "ast"              -> toJs(ast),
                "complexity"       -> complexity,
                "complexityByFunc" -> complexityByFunc.toJSDictionary
              )
          }
    }
  }

  @JSExportTopLevel("decompile")
  def decompile(input: String): js.Dynamic =
    Global
      .decompile(input)
      .fold(
        err => js.Dynamic.literal("error" -> err.m), {
          case (scriptText, meta) =>
            jObj(
              "result" -> scriptText,
              "meta"   -> metaConverter.foldRoot(meta)
            )
        }
      )

  lazy val metaConverter: RecKeyValueFolder[Any, js.Object with js.Dynamic] =
    RecKeyValueFolder(
      Any.fromString,
      _.toJSArray,
      js.Dynamic.literal.applyDynamic("apply")(_: _*)
    )

  @JSExportTopLevel("nodeVersion")
  def nodeVersion(): js.Dynamic = js.Dynamic.literal("version" -> Version.VersionString)

  @JSExportTopLevel("repl")
  def repl(settings: UndefOr[NodeConnectionSettings]): js.Dynamic = asJs(Repl(settings.toOption))

  private def asJs(repl: Repl): js.Dynamic =
    jObj(
      "evaluate"    -> (repl.execute _ andThen mapResult),
      "info"        -> repl.info _,
      "totalInfo"   -> repl.totalInfo _,
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
