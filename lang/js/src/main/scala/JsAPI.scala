import cats.kernel.Monoid
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.contract.{Contract, ContractSerDe}
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.{CTX, Serde}
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, ExpressionCompilerV1}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.traits.domain.{Ord, Recipient, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import fastparse.core.Parsed.{Failure, Success}
import shapeless.{:+:, CNil}

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

  def wavesContext(v: com.wavesplatform.lang.Version.Version) = WavesContext.build(
    v,
    new Environment {
      override def height: Long                                                                                    = 0
      override def chainId: Byte                                                                                   = 1: Byte
      override def inputEntity: Tx :+: Ord :+: CNil                                                                = null
      override def transactionById(id: Array[Byte]): Option[Tx]                                                    = ???
      override def transactionHeightById(id: Array[Byte]): Option[Long]                                            = ???
      override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any]                   = ???
      override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = ???
      override def resolveAlias(name: String): Either[String, Recipient.Address]                                   = ???
    },
    isTokenContext = false
  )

  val v1                   = com.wavesplatform.lang.Version.ExprV1
  val v3                   = com.wavesplatform.lang.Version.ContractV
  val exprWavesContext     = wavesContext(v1)
  val contractWavesContext = wavesContext(v3)

  val cryptoContext = CryptoContext.build(Global)

  def typeRepr(t: TYPE): js.Any = t match {
    case UNION(l) => l.map(typeRepr).toJSArray
    case CASETYPEREF(name, fields) =>
      js.Dynamic.literal("typeName" -> name, "fields" -> fields.map(f => js.Dynamic.literal("name" -> f._1, "type" -> typeRepr(f._2))).toJSArray)
    case LIST(t) => js.Dynamic.literal("listOf" -> typeRepr(t))
    case t       => t.toString
  }

  @JSExportTopLevel("fullContext")
  val fullContext: CTX = Monoid.combineAll(Seq(PureContext.build(v1), cryptoContext, exprWavesContext))

  val fullContractContext: CTX = Monoid.combineAll(Seq(PureContext.build(v3), cryptoContext, contractWavesContext))

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

    def hash(m: Array[Byte]) = Global.keccak256(Global.blake2b256(m))

    def serialize(expr: EXPR): Either[String, Array[Byte]] = {
      val s = 1.toByte +: Serde.serialize(expr)
      Right(s ++ hash(s).take(4))
    }

    (Parser.parseScript(input) match {
      case Success(value, _)    => Right[String, Expressions.EXPR](value)
      case Failure(_, _, extra) => Left[String, Expressions.EXPR](extra.traced.trace)
    }).flatMap(ExpressionCompilerV1(fullContractContext.compilerContext, _))
      .flatMap(ast => serialize(ast._1).map(x => (x, ast)))
      .fold(
        err => {
          js.Dynamic.literal("error" -> err)
        }, {
          case (result, ast) =>
            //js.Dynamic.literal("result" -> result)
            js.Dynamic.literal("result" -> Global.toBuffer(result), "ast" -> toJs(ast._1))
        }
      )
  }

  @JSExportTopLevel("compileContract")
  def compileContract(input: String): js.Dynamic = {

    def hash(m: Array[Byte]) = Global.keccak256(Global.blake2b256(m))

    def serialize(expr: Contract): Either[String, Array[Byte]] = {
      val s = 3.toByte +: ContractSerDe.serialize(expr)
      Right(s ++ hash(s).take(4))
    }

    (Parser.parseContract(input) match {
      case Success(value, _)    => Right[String, Expressions.CONTRACT](value)
      case Failure(_, _, extra) => Left[String, Expressions.CONTRACT](extra.traced.trace)
    }).flatMap(ContractCompiler(fullContractContext.compilerContext, _))
      .flatMap(ast => serialize(ast).map(x => (x, ast)))
      .fold(
        err => {
          js.Dynamic.literal("error" -> err)
        }, {
          case (result, ast) =>
            //js.Dynamic.literal("result" -> result)
            js.Dynamic.literal("result" -> Global.toBuffer(result), "ast" -> toJs(ast))
        }
      )
  }
}
