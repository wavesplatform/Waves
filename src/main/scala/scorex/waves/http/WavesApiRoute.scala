package scorex.waves.http

import java.nio.charset.StandardCharsets
import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json.{JsError, JsSuccess, Json}
import scorex.account.Account
import scorex.api.http.{NegativeFee, NoBalance, _}
import scorex.app.Application
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.transaction.LagonakiTransaction.ValidationResult
import scorex.waves.transaction.{ExternalPayment, WavesTransactionModule}

import scala.util.{Failure, Success, Try}

@Path("/waves")
@Api(value = "waves", description = "Waves specific commands.", position = 1)
case class WavesApiRoute(override val application: Application)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  // TODO asInstanceOf
  implicit lazy val transactionModule: WavesTransactionModule = application.transactionModule.asInstanceOf[WavesTransactionModule]

  override lazy val route = pathPrefix("waves") {
    payment ~ address
  }

  @Path("/address")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "publicKey", value = "Public key as a plain string", required = true, paramType = "body", dataType = "String")
  ))
  @ApiOperation(value = "Generate", notes = "Generate a address from public key", httpMethod = "POST")
  def address: Route = {
    path("address") {
      withCors {
        entity(as[String]) { publicKey =>
          val addressFromPubKey = Account.fromPublicKey(Base58.decode(publicKey).get)
          complete(HttpEntity(ContentTypes.`application/json`, Json.obj("address" -> addressFromPubKey).toString))
        }
      }
    }
  }

  @Path("/external-payment")
  @ApiOperation(value = "Send payment", notes = "Publish signed payment to the Blockchain", httpMethod = "POST", produces = "application/json", consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.waves.transaction.ExternalPayment",
      defaultValue = "{\n\t\"timestamp\": 0,\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"senderPublicKey\":\"senderPubKey\",\n\t\"recipient\":\"recipientId\",\n\t\"signature\":\"sig\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def payment: Route = path("external-payment") {
    withCors {
      entity(as[String]) { body =>
        val resp = Try(Json.parse(body)).map { js =>
          js.validate[ExternalPayment] match {
            case err: JsError =>
              WrongJson.json
            case JsSuccess(payment: ExternalPayment, _) =>
              val tx = transactionModule.broadcastPayment(payment)
              tx.validate match {
                case ValidationResult.ValidateOke =>
                  tx.json

                case ValidationResult.InvalidAddress =>
                  InvalidAddress.json

                case ValidationResult.NegativeAmount =>
                  NegativeAmount.json

                case ValidationResult.NegativeFee =>
                  NegativeFee.json

                case ValidationResult.NoBalance =>
                  NoBalance.json
              }
          }
        }.getOrElse(WrongJson.json).toString

        complete(HttpEntity(ContentTypes.`application/json`, resp))
      }
    }
  }

//
//  // Workaround to show datatype of post request without using it in another route
//  // Related: https://github.com/swagger-api/swagger-core/issues/606
//  // Why is this still showing even though it's set to hidden? See https://github.com/martypitt/swagger-springmvc/issues/447
//  @ApiOperation(value = "IGNORE", notes = "", hidden = true, httpMethod = "GET", response = classOf[Payment])
//  protected def paymentModel = Unit

}