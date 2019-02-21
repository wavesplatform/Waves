import cats.kernel.Monoid
import com.wavesplatform.lang.StdLibVersion.{StdLibVersion, _}
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.directives.DirectiveParser
import com.wavesplatform.lang.utils.{extractContentType, extractScriptType, extractStdLibVersion}
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.domain.{Recipient, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import com.wavesplatform.lang.{ContentType, Global, ScriptType, utils}

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

  private def toJs(c: Contract): js.Object = {
    toJs(TRUE) // later
  }

  private def wavesContext(v: com.wavesplatform.lang.StdLibVersion.StdLibVersion, isTokenContext: Boolean = false) = WavesContext.build(
    v,
    new Environment {
      override def height: Long                                                                                    = 0
      override def chainId: Byte                                                                                   = 1: Byte
      override def inputEntity: Environment.InputEntity                                                            = null
      override def transactionById(id: Array[Byte]): Option[Tx]                                                    = ???
      override def transactionHeightById(id: Array[Byte]): Option[Long]                                            = ???
      override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any]                   = ???
      override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = ???
      override def resolveAlias(name: String): Either[String, Recipient.Address]                                   = ???
    },
    isTokenContext
  )

  private val cryptoContext = CryptoContext.build(Global)

  private def typeRepr(t: TYPE): js.Any = t match {
    case UNION(l) => l.map(typeRepr).toJSArray
    case CASETYPEREF(name, fields) =>
      js.Dynamic.literal("typeName" -> name, "fields" -> fields.map(f => js.Dynamic.literal("name" -> f._1, "type" -> typeRepr(f._2))).toJSArray)
    case LIST(t) => js.Dynamic.literal("listOf" -> typeRepr(t))
    case t       => t.toString
  }

  private val fullContractContext: CTX =
    buildContractContext(V3)

  private def buildScriptContext(v: StdLibVersion, isTokenContext: Boolean): CTX = {
    Monoid.combineAll(Seq(PureContext.build(v), cryptoContext, wavesContext(v, isTokenContext)))
  }

  private def buildContractContext(v: StdLibVersion): CTX = {
    Monoid.combineAll(Seq(PureContext.build(v), cryptoContext, wavesContext(V3)))
  }

  @JSExportTopLevel("fullContext")
  val fullContext: CTX =
    buildScriptContext(V3, isTokenContext = false)

  @JSExportTopLevel("getTypes")
  def getTypes() = fullContext.types.map(v => js.Dynamic.literal("name" -> v.name, "type" -> typeRepr(v.typeRef))).toJSArray

  @JSExportTopLevel("getVarsDoc")
  def getVarsDoc() = fullContext.vars.map(v => js.Dynamic.literal("name" -> v._1, "type" -> typeRepr(v._2._1._1), "doc" -> v._2._1._2)).toJSArray

  @JSExportTopLevel("getFunctionsDoc")
  def getFunctionnsDoc() =
    fullContext.functions
      .map(
        f =>
          js.Dynamic.literal(
            "name"       -> f.name,
            "doc"        -> f.docString,
            "resultType" -> typeRepr(f.signature.result),
            "args" -> ((f.argsDoc zip f.signature.args) map { arg =>
              js.Dynamic.literal("name" -> arg._1._1, "type" -> typeRepr(arg._2._2), "doc" -> arg._1._2)
            }).toJSArray
        ))
      .toJSArray

  @JSExportTopLevel("compilerContext")
  val compilerContext = fullContext.compilerContext

  @JSExportTopLevel("compile")
  def compile(input: String): js.Dynamic = {
    val (directives, scriptWithoutDirectives) = DirectiveParser.splitToDirectiveAndScript(input)
    val compiled = for {
      ver         <- extractStdLibVersion(directives)
      contentType <- extractContentType(directives)
      scriptType  <- extractScriptType(directives)
    } yield {
      contentType match {
        case ContentType.Expression =>
          val ctx = buildScriptContext(ver, scriptType == ScriptType.Asset)
          Global
            .compileScript(scriptWithoutDirectives, ctx.compilerContext)
            .fold(
              err => {
                js.Dynamic.literal("error" -> err)
              }, {
                case (bytes, ast) =>
                  js.Dynamic.literal("result" -> Global.toBuffer(bytes), "ast" -> toJs(ast))
              }
            )
        case ContentType.Contract =>
          // Just ignore stdlib version here
          Global
            .compileContract(scriptWithoutDirectives, fullContractContext.compilerContext)
            .fold(
              err => {
                js.Dynamic.literal("error" -> err)
              }, {
                case (bytes, ast) =>
                  js.Dynamic.literal("result" -> Global.toBuffer(bytes), "ast" -> toJs(ast))
              }
            )
      }
    }

    compiled.fold(
      err => js.Dynamic.literal("error" -> err),
      identity
    )
  }
}
