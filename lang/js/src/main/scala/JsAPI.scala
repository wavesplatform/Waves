import cats.kernel.Monoid
import com.wavesplatform.DocSource
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.meta.RecKeyValueFolder
import com.wavesplatform.lang.directives.Directive.extractDirectives
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveParser, DirectiveSet}
import com.wavesplatform.lang.script.ScriptPreprocessor
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.repl.Repl
import com.wavesplatform.lang.v1.repl.node.http.NodeConnectionSettings
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.{CTX, ContractLimits}
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.lang.{Global, Version}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.Dynamic.{literal => jObj}
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.{Any, Dictionary, Promise, UndefOr}

object JsAPI {
  private def toJs(ast: EXPR): js.Object = {
    def r(expr: EXPR): js.Object = {
      expr match {
        case CONST_LONG(t)      => jObj.applyDynamic("apply")("type" -> "LONG", "value"    -> t)
        case GETTER(ref, field) => jObj.applyDynamic("apply")("type" -> "GETTER", "ref"    -> r(ref), "field" -> field)
        case CONST_BYTESTR(bs)  => jObj.applyDynamic("apply")("type" -> "BYTESTR", "value" -> bs.arr.toJSArray)
        case CONST_STRING(s)    => jObj.applyDynamic("apply")("type" -> "STRING", "value"  -> s)
        case LET_BLOCK(let, body) =>
          jObj.applyDynamic("apply")("type" -> "BLOCK", "let" -> jObj("name" -> let.name, "value" -> r(let.value)), "body" -> r(body))
        case IF(cond, ifTrue, ifFalse) =>
          jObj.applyDynamic("apply")("type" -> "IF", "condition" -> r(cond), "true" -> r(ifTrue), "false" -> r(ifFalse))
        case REF(key)         => jObj.applyDynamic("apply")("type" -> "REF", "key"    -> key)
        case CONST_BOOLEAN(b) => jObj.applyDynamic("apply")("type" -> "BOOL", "value" -> b)
        case FUNCTION_CALL(function, args) =>
          jObj.applyDynamic("apply")("type" -> "CALL", "name" -> (function match {
            case Native(name)          => name.toString()
            case User(internalName, _) => internalName
          }), "args" -> args.map(r).toJSArray)
        case t => jObj.applyDynamic("apply")("[not_supported]stringRepr" -> t.toString)
      }
    }

    r(ast)
  }

  private def toJs(c: DApp): js.Object = {
    toJs(TRUE) // later
  }

  private def wavesContext(v: StdLibVersion, isTokenContext: Boolean, isContract: Boolean) =
    WavesContext.build(
      DirectiveSet(v, ScriptType.isAssetScript(isTokenContext), if (isContract) DAppType else Expression)
        .explicitGet()
    )

  private def cryptoContext(version: StdLibVersion) = CryptoContext.build(Global, version).withEnvironment[Environment]
  private def pureContext(version: StdLibVersion)   = PureContext.build(Global, version).withEnvironment[Environment]

  private def typeRepr(t: TYPE): js.Any = t match {
    case UNION(l, _) => l.map(typeRepr).toJSArray
    case CASETYPEREF(name, fields) =>
      js.Dynamic.literal("typeName" -> name, "fields" -> fields.map(f => js.Dynamic.literal("name" -> f._1, "type" -> typeRepr(f._2))).toJSArray)
    case LIST(t) => js.Dynamic.literal("listOf" -> typeRepr(t))
    case t       => t.toString
  }

  private val fullContractContext: CTX[Environment] =
    buildContractContext(V3)

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
      "MaxPaymentAmount"           -> ContractLimits.MaxPaymentAmount
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

  private def compileScript(ds: DirectiveSet, input: String): Either[String, js.Object with js.Dynamic] = {
    val version = ds.stdLibVersion
    ds.contentType match {
      case Expression =>
        val ctx = buildScriptContext(version, ds.scriptType == Asset, ds.contentType == DAppType)
        Global.compileExpression(input, ctx.compilerContext, version, estimator)
          .map {
            case (bytes, expr, complexity) =>
              val resultFields: Seq[(String, Any)] = Seq(
                "result"     -> Global.toBuffer(bytes),
                "ast"        -> toJs(expr),
                "complexity" -> complexity
              )
              val errorFieldOpt: Seq[(String, Any)] =
                Global.checkExpr(expr, complexity, version)
                  .fold(
                    error => Seq("error" -> error),
                    _     => Seq()
                  )
              js.Dynamic.literal.applyDynamic("apply")(resultFields ++ errorFieldOpt: _*)
          }
      case Library =>
        val ctx = buildScriptContext(version, ds.scriptType == Asset, ds.contentType == DAppType)
        Global.compileDecls(input, ctx.compilerContext, version, estimator)
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
        Global.compileContract(input, fullContractContext.compilerContext, version, estimator)
          .map {
            case (bytes, dApp, maxComplexityFunc @ (_, maxComplexity), complexityByFunc) =>
              val resultFields: Seq[(String, Any)] = Seq(
                "result"           -> Global.toBuffer(bytes),
                "ast"              -> toJs(dApp),
                "complexity"       -> maxComplexity,
                "complexityByFunc" -> complexityByFunc.toJSDictionary
              )
              val errorFieldOpt: Seq[(String, Any)] =
                Global.checkContract(dApp, maxComplexityFunc, version)
                  .fold(
                    error => Seq("error" -> error),
                    _     => Seq()
                  )
              js.Dynamic.literal.applyDynamic("apply")(resultFields ++ errorFieldOpt: _*)

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
