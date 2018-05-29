import cats.kernel.Monoid
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.v1.compiler.{CompilerContext, CompilerV1}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext, WavesContext}
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.traits.{DataType, Environment, Transaction}
import fastparse.core.Parsed.{Failure, Success}
import scodec.Attempt
import scodec.Attempt.Successful

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.Dynamic.{literal => jObj}

object JsAPI {

  private def toJs(ast: EXPR): js.Object = {
    def r(expr: EXPR): js.Object = {
      expr match {
        case CONST_LONG(t)                      => jObj("type" -> "LONG", "value"       -> t)
        case GETTER(ref, field, tpe)            => jObj("type" -> "GETTER", "ref"       -> r(ref), "field" -> field)
        case CONST_BYTEVECTOR(bs)               => jObj("type" -> "BYTEVECTOR", "value" -> bs.toArray.toJSArray)
        case CONST_STRING(s)                    => jObj("type" -> "STRING", "value"     -> s)
        case BLOCK(let, body, tpe)              => jObj("type" -> "BLOCK", "let"        -> jObj("name" -> let.name, "value" -> r(let.value)), "body" -> r(body))
        case IF(cond, ifTrue, ifFalse, tpe)     => jObj("type" -> "IF", "condition"     -> r(cond), "true" -> r(ifTrue), "false" -> r(ifFalse))
        case REF(key, tpe)                      => jObj("type" -> "REF", "key"          -> key)
        case TRUE                               => jObj("type" -> "BOOL", "value"       -> true)
        case FALSE                              => jObj("type" -> "BOOL", "value"       -> false)
        case FUNCTION_CALL(function, args, tpe) => jObj("type" -> "CALL", "name"        -> function.name, "args" -> args.map(r).toJSArray)
      }
    }

    r(ast)
  }

  @JSExportTopLevel("compile")
  def compile(input: String): js.Dynamic = {

    val c = WavesContext.build(new Environment {
      override def height: Int                                                                                       = ???
      override def networkByte: Byte                                                                                 = ???
      override def transaction: Transaction                                                                          = ???
      override def transactionById(id: Array[Byte]): Option[Transaction]                                             = ???
      override def transactionHeightById(id: Array[Byte]): Option[Int]                                               = ???
      override def data(addressBytes: Array[Byte], key: String, dataType: DataType): Option[Any]                     = ???
      override def resolveAddress(addressOrAlias: Array[Byte]): Either[String, Array[Byte]]                          = ???
      override def accountBalanceOf(addressOrAlias: Array[Byte], assetId: Option[Array[Byte]]): Either[String, Long] = ???
    })

    val b = Monoid.combine(c, CryptoContext.build(Global))
    val d = Monoid.combine(b, PureContext.instance)

    def serialize(expr: EXPR): Either[String, Array[Byte]] = {
      Serde.codec
        .encode(expr)
        .map(x => {
          val s = Array(1.toByte) ++ x.toByteArray
          s ++ hash(s).take(4)
        }) match {
        case Successful(value)      => Right[String, Array[Byte]](value)
        case Attempt.Failure(cause) => Left[String, Array[Byte]](cause.message)
      }
    }

    def hash(m: Array[Byte]) = Global.keccak256(Global.blake2b256(m))

    (Parser(input) match {
      case Success(value, _)    => Right[String, Expressions.EXPR](value.head)
      case Failure(_, _, extra) => Left[String, Expressions.EXPR](extra.traced.trace)
    }).flatMap(CompilerV1(CompilerContext.fromEvaluationContext(d), _))
      .flatMap(ast => serialize(ast).map(x => (Global.base58Encode(x), ast)))
      .fold(
        err => {
          js.Dynamic.literal("error" -> err)
        }, {
          case (result, ast) =>
            // js.Dynamic.literal("result" -> result)
            js.Dynamic.literal("result" -> result, "ast" -> toJs(ast))
        }
      )
  }
}
