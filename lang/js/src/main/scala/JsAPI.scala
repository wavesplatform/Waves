import cats.kernel.Monoid
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.traits.{DataType, Environment, Recipient, Tx}
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import fastparse.core.Parsed.{Failure, Success}

import scala.scalajs.js
import scala.scalajs.js.Dynamic.{literal => jObj}
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExportTopLevel

object JsAPI {

  private def toJs(ast: EXPR): js.Object = {
    def r(expr: EXPR): js.Object = {
      expr match {
        case CONST_LONG(t)        => jObj.applyDynamic("apply")("type" -> "LONG", "value"       -> t)
        case GETTER(ref, field)   => jObj.applyDynamic("apply")("type" -> "GETTER", "ref"       -> r(ref), "field" -> field)
        case CONST_BYTEVECTOR(bs) => jObj.applyDynamic("apply")("type" -> "BYTEVECTOR", "value" -> bs.toArray.toJSArray)
        case CONST_STRING(s)      => jObj.applyDynamic("apply")("type" -> "STRING", "value"     -> s)
        case BLOCK(let, body) =>
          jObj.applyDynamic("apply")("type" -> "BLOCK", "let" -> jObj("name" -> let.name, "value" -> r(let.value)), "body" -> r(body))
        case IF(cond, ifTrue, ifFalse) =>
          jObj.applyDynamic("apply")("type" -> "IF", "condition" -> r(cond), "true" -> r(ifTrue), "false" -> r(ifFalse))
        case REF(key) => jObj.applyDynamic("apply")("type" -> "REF", "key"    -> key)
        case TRUE     => jObj.applyDynamic("apply")("type" -> "BOOL", "value" -> true)
        case FALSE    => jObj.applyDynamic("apply")("type" -> "BOOL", "value" -> false)
        case FUNCTION_CALL(function, args) =>
          jObj.applyDynamic("apply")("type" -> "CALL", "name" -> (function match {
            case Native(name) => name.toString()
            case User(name)   => name
          }), "args" -> args.map(r).toJSArray)
      }
    }

    r(ast)
  }

  @JSExportTopLevel("compile")
  def compile(input: String): js.Dynamic = {

    val wavesContext = WavesContext.build(new Environment {
      override def height: Int                                                                                     = ???
      override def networkByte: Byte                                                                               = ???
      override def inputEntity: Tx                                                                                 = ???
      override def transactionById(id: Array[Byte]): Option[Tx]                                                    = ???
      override def transactionHeightById(id: Array[Byte]): Option[Int]                                             = ???
      override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any]                   = ???
      override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = ???
      override def resolveAlias(name: String): Either[String, Recipient.Address]                                   = ???
    })

    //comment
    val cryptoContext = CryptoContext.build(Global)

    val compilerContext = Monoid.combineAll(Seq(PureContext.ctx, cryptoContext, wavesContext)).compilerContext

    def hash(m: Array[Byte]) = Global.keccak256(Global.blake2b256(m))

    def serialize(expr: EXPR): Either[String, Array[Byte]] = {
      val s = 1.toByte +: Serde.serialize(expr)
      Right(s ++ hash(s).take(4))
    }

    (Parser(input) match {
      case Success(value, _)    => Right[String, Expressions.EXPR](value.head)
      case Failure(_, _, extra) => Left[String, Expressions.EXPR](extra.traced.trace)
    }).flatMap(CompilerV1(compilerContext, _))
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
}
