import cats.kernel.Monoid
import com.wavesplatform.lang.Version
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.Directive.extractDirectives
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveParser, DirectiveSet}
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import com.wavesplatform.lang.v1.{CTX, ContractLimits}

import scala.scalajs.js
import scala.scalajs.js.Dynamic.{literal => jObj}
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExportTopLevel

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
            case Native(name) => name.toString()
            case User(name)   => name
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
        .explicitGet(),
      new Environment {
        override def height: Long                                                                                    = 0
        override def chainId: Byte                                                                                   = 1: Byte
        override def inputEntity: Environment.InputEntity                                                            = null
        override def transactionById(id: Array[Byte]): Option[Tx]                                                    = ???
        override def transferTransactionById(id: Array[Byte]): Option[Tx]                                            = ???
        override def transactionHeightById(id: Array[Byte]): Option[Long]                                            = ???
        override def assetInfoById(id: Array[Byte]): Option[ScriptAssetInfo]                                         = ???
        override def lastBlockOpt(): Option[BlockInfo]                                                               = ???
        override def blockInfoByHeight(height: Int): Option[BlockInfo]                                               = ???
        override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any]                   = ???
        override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = ???
        override def resolveAlias(name: String): Either[String, Recipient.Address]                                   = ???
        override def tthis: Recipient.Address                                                                        = ???
      }
    )

  private def cryptoContext(version: StdLibVersion) = CryptoContext.build(Global, version)
  private def pureContext(version: StdLibVersion)   = PureContext.build(Global, version)
  private val letBLockVersions: Set[StdLibVersion]  = Set(V1, V2)

  private def typeRepr(t: TYPE): js.Any = t match {
    case UNION(l, _) => l.map(typeRepr).toJSArray
    case CASETYPEREF(name, fields) =>
      js.Dynamic.literal("typeName" -> name, "fields" -> fields.map(f => js.Dynamic.literal("name" -> f._1, "type" -> typeRepr(f._2))).toJSArray)
    case LIST(t) => js.Dynamic.literal("listOf" -> typeRepr(t))
    case t       => t.toString
  }

  private val fullContractContext: CTX =
    buildContractContext(V3)

  private def buildScriptContext(v: StdLibVersion, isTokenContext: Boolean, isContract: Boolean): CTX = {
    Monoid.combineAll(Seq(pureContext(v), cryptoContext(v), wavesContext(v, isTokenContext, isContract)))
  }

  private def buildContractContext(v: StdLibVersion): CTX = {
    Monoid.combineAll(Seq(pureContext(v), cryptoContext(v), wavesContext(v, false, true)))
  }

  @JSExportTopLevel("getTypes")
  def getTypes(ver: Int = 2, isTokenContext: Boolean = false, isContract: Boolean = false): js.Array[js.Object with js.Dynamic] =
    buildScriptContext(DirectiveDictionary[StdLibVersion].idMap(ver), isTokenContext, isContract).types
      .map(v => js.Dynamic.literal("name" -> v.name, "type" -> typeRepr(v)))
      .toJSArray

  @JSExportTopLevel("getVarsDoc")
  def getVarsDoc(ver: Int = 2, isTokenContext: Boolean = false, isContract: Boolean = false): js.Array[js.Object with js.Dynamic] =
    buildScriptContext(DirectiveDictionary[StdLibVersion].idMap(ver), isTokenContext, isContract).vars
      .map(v => js.Dynamic.literal("name" -> v._1, "type" -> typeRepr(v._2._1._1), "doc" -> v._2._1._2))
      .toJSArray

  @JSExportTopLevel("getFunctionsDoc")
  def getFunctionsDoc(ver: Int = 2, isTokenContext: Boolean = false, isContract: Boolean = false): js.Array[js.Object with js.Dynamic] =
    buildScriptContext(DirectiveDictionary[StdLibVersion].idMap(ver), isTokenContext, isContract).functions
      .map(f =>
        js.Dynamic.literal(
          "name"       -> f.name,
          "doc"        -> f.docString,
          "resultType" -> typeRepr(f.signature.result),
          "args" -> ((f.argsDoc zip f.signature.args) map { arg =>
            js.Dynamic.literal("name" -> arg._1._1, "type" -> typeRepr(arg._2._2), "doc" -> arg._1._2)
          }).toJSArray
      ))
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
        case DirectiveSet(ver, scriptType, contentType) =>
          js.Dynamic.literal("stdLibVersion" -> ver.id, "contentType" -> contentType.id, "scriptType" -> scriptType.id)
      }
    info.fold(
      err => js.Dynamic.literal("error" -> err),
      identity
    )
  }

  @JSExportTopLevel("compile")
  def compile(input: String): js.Dynamic = {
    val compiled = DirectiveParser(input)
      .flatMap(extractDirectives)
      .map {
        case DirectiveSet(ver, scriptType, contentType) =>
          contentType match {
            case Expression =>
              val ctx = buildScriptContext(ver, scriptType == Asset, contentType == DAppType)
              Global
                .compileExpression(input, ctx.compilerContext, letBLockVersions.contains(ver), ver)
                .fold(
                  err => {
                    js.Dynamic.literal("error" -> err)
                  }, {
                    case (bytes, ast, complexity) =>
                      js.Dynamic.literal(
                        "result"     -> Global.toBuffer(bytes),
                        "ast"        -> toJs(ast),
                        "complexity" -> complexity
                      )
                  }
                )
            case DAppType =>
              // Just ignore stdlib version here
              Global
                .compileContract(input, fullContractContext.compilerContext, ver)
                .fold(
                  err => {
                    js.Dynamic.literal("error" -> err)
                  }, {
                    case (bytes, ast, complexity, complexityByFunc) =>
                      js.Dynamic.literal(
                        "result"           -> Global.toBuffer(bytes),
                        "ast"              -> toJs(ast),
                        "complexity"       -> complexity,
                        "complexityByFunc" -> complexityByFunc
                      )
                  }
                )
          }
      }

    compiled.fold(
      err => js.Dynamic.literal("error" -> err),
      identity
    )
  }

  @JSExportTopLevel("decompile")
  def decompile(input: String): js.Dynamic = {
    val decompiled = Global.decompile(input).right.map { scriptText =>
      js.Dynamic.literal("result" -> scriptText)
    }
    decompiled.fold(
      err => js.Dynamic.literal("error" -> err.m),
      identity
    )
  }

  @JSExportTopLevel("nodeVersion")
  def nodeVersion(): js.Dynamic = js.Dynamic.literal("version" -> Version.VersionString)
}
