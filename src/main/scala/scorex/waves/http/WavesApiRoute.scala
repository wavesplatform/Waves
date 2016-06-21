package scorex.waves.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json.{JsError, JsSuccess, Json}
import scorex.account.Account
import scorex.api.http.{NegativeFee, NoBalance, _}
import scorex.app.Application
import scorex.crypto.encode.Base58
import scorex.transaction.LagonakiTransaction.ValidationResult
import scorex.waves.settings.WavesSettings
import scorex.waves.transaction.{ExternalPayment, WavesTransactionModule}
import scorex.waves.wallet.Wallet

import scala.util.{Failure, Success, Try}

@Path("/waves")
@Api(value = "waves", description = "Waves specific commands.", position = 1)
case class WavesApiRoute(override val application: Application)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  val suspendedSenders = application.settings.asInstanceOf[WavesSettings].suspendedSenders

  // TODO asInstanceOf
  implicit lazy val transactionModule: WavesTransactionModule = application.transactionModule.asInstanceOf[WavesTransactionModule]

  override lazy val route = pathPrefix("waves") {
    payment ~ address ~ signPayment ~broadcastSignedPayment
  }


  @Path("/create-signed-payment")
  @ApiOperation(value = "Sign payment",
    notes = "Sign payment by provided wallet seed",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.waves.http.UnsignedPayment",
      defaultValue = "{\n\t\"timestamp\": 0,\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"recipient\":\"recipientAddress\", \n\t\"senderWalletSeed\":\"seed\",\n\t\"senderAddressNonce\":\"0\"\n}"
    )
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with response or error")
  ))
  def signPayment: Route = path("create-signed-payment") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          Try(Json.parse(body)).map { js =>
            js.validate[UnsignedPayment] match {
              case err: JsError =>
                WrongTransactionJson(err).json
              case JsSuccess(payment: UnsignedPayment, _) =>
                val senderWalletSeed = Base58.decode(payment.senderWalletSeed).getOrElse(Array.empty)
                if (senderWalletSeed.isEmpty)
                  WrongJson.json
                else {
                  val senderAccount =  Wallet.generateNewAccount(senderWalletSeed, payment.senderAddressNonce)
                  val recipientAccount = new Account(payment.recipient)

                  transactionModule.createSignedPayment(senderAccount, recipientAccount,
                    payment.amount, payment.fee, payment.timestamp) match {
                    case Left(tx) => {
                      val signature = Base58.encode(tx.signature)
                      val senderPubKey = Base58.encode(tx.sender.publicKey)
                      val signedTx = SignedPayment(tx.timestamp, tx.amount, tx.fee, tx.recipient.toString,
                        senderPubKey, tx.sender.address, signature)
                      Json.toJson(signedTx)
                    }
                    case Right(e) => e match {
                      case ValidationResult.NoBalance => NoBalance.json
                      case ValidationResult.InvalidAddress => InvalidAddress.json
                    }
                  }
                }
            }
          }.getOrElse(WrongJson.json)
        }
      }
    }
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

  @Deprecated
  @Path("/external-payment")
  @ApiOperation(value = "Broadcast payment", notes = "Publish signed payment to the Blockchain", httpMethod = "POST", produces = "application/json", consumes = "application/json")
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
              Base58.decode(payment.senderPublicKey) match {
                case Success(senderPubKeyBytes) =>
                  val senderAddress = Account.fromPublicKey(senderPubKeyBytes)
                  if (suspendedSenders.contains(senderAddress))
                    InvalidSender.json
                  else
                    broadcastPayment(payment)

                case Failure(e) => InvalidSender.json
              }
          }
        }.getOrElse(WrongJson.json).toString

        complete(HttpEntity(ContentTypes.`application/json`, resp))
      }
    }
  }

  @Path("/broadcast-signed-payment")
  @ApiOperation(value = "Broadcast signed payment", notes = "Publish signed payment to the Blockchain", httpMethod = "POST", produces = "application/json", consumes = "application/json")
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
  def broadcastSignedPayment : Route = path("broadcast-signed-payment") {
    withCors {
      entity(as[String]) { body =>
        val resp = Try(Json.parse(body)).map { js =>
          js.validate[ExternalPayment] match {
            case err: JsError =>
              WrongJson.json
            case JsSuccess(payment: ExternalPayment, _) =>
              Base58.decode(payment.senderPublicKey) match {
                case Success(senderPubKeyBytes) =>
                  val senderAddress = Account.fromPublicKey(senderPubKeyBytes)
                  if (suspendedSenders.contains(senderAddress))
                    InvalidSender.json
                  else
                    broadcastPayment(payment)

                case Failure(e) => InvalidSender.json
              }
          }
        }.getOrElse(WrongJson.json).toString

        complete(HttpEntity(ContentTypes.`application/json`, resp))
      }
    }
  }

  private def broadcastPayment(payment: ExternalPayment) = {
    transactionModule.broadcastPayment(payment) match {
      case Left(tx) =>
        if (!tx.signatureValid) InvalidSignature.json
        else {
          tx.validate match {
            case ValidationResult.ValidateOke =>
              tx.json

            case ValidationResult.InvalidAddress =>
              InvalidAddress.json

            case ValidationResult.NegativeAmount =>
              NegativeAmount.json

            case ValidationResult.NegativeFee =>
              NegativeFee.json
          }
        }
      case Right(e) => e match {
        case ValidationResult.NoBalance => NoBalance.json
        case ValidationResult.InvalidAddress => InvalidAddress.json
      }
    }
  }
}