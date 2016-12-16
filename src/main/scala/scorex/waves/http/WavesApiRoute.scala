package scorex.waves.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.WavesSettings
import io.swagger.annotations._
import play.api.libs.json.{JsError, JsPath, JsSuccess, Json}
import scorex.account.Account
import scorex.api.http.{NegativeFee, NoBalance, _}
import scorex.app.RunnableApplication
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationResult
import scorex.transaction.state.wallet.Payment
import scorex.wallet.Wallet
import scorex.waves.transaction.{ExternalPayment, SignedPayment, WavesTransactionModule}
import scorex.api.http.formats._

import scala.util.{Failure, Success, Try}

@Path("/waves")
@Api(value = "waves")
@Deprecated
case class WavesApiRoute(application: RunnableApplication)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  val settings = application.settings

  lazy val wallet = application.wallet

  // TODO asInstanceOf
  implicit lazy val transactionModule: WavesTransactionModule = application.transactionModule.asInstanceOf[WavesTransactionModule]

  override lazy val route = pathPrefix("waves") {
    externalPayment ~ signPayment ~ broadcastSignedPayment ~ payment ~ createdSignedPayment
  }

  @Deprecated
  @Path("/payment")
  @ApiOperation(value = "Send payment from wallet. Deprecated: use /assets/transfer instead",
    notes = "Send payment from wallet to another wallet. Each call sends new payment. Deprecated: use /assets/transfer instead",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.state.wallet.Payment",
      defaultValue = "{\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"sender\":\"senderId\",\n\t\"recipient\":\"recipientId\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def payment: Route = path("payment") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          walletNotExists(wallet).getOrElse {
            Try(Json.parse(body)).map { js =>
              js.validate[Payment] match {
                case err: JsError =>
                  WrongTransactionJson(err).response
                case JsSuccess(payment: Payment, _) =>
                  val txOpt = transactionModule.createPayment(payment, wallet)
                  txOpt match {
                    case Some(tx) =>
                      tx.validate match {
                        case ValidationResult.ValidateOke =>
                          val signed = SignedPayment(tx.timestamp, tx.amount, tx.fee, tx.recipient,
                            tx.sender, tx.sender.address, Base58.encode(tx.signature))
                          JsonResponse(Json.toJson(signed), StatusCodes.OK)

                        case ValidationResult.InvalidAddress => InvalidAddress.response
                        case ValidationResult.NegativeAmount => NegativeAmount.response
                        case ValidationResult.InsufficientFee => InsufficientFee.response
                        case ValidationResult.NoBalance => NoBalance.response
                      }
                    case None => InvalidSender.response
                  }
              }
            }.getOrElse(WrongJson.response)
          }
        }
      }
    }
  }

  @Deprecated
  @Path("/payment/signature")
  @ApiOperation(value = "Create payment signed by address from wallet",
    notes = "Create unique payment signed by address from wallet. Without broadcasting to network.",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.state.wallet.Payment",
      defaultValue = "{\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"sender\":\"senderId\",\n\t\"recipient\":\"recipientId\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def signPayment: Route = path("payment" / "signature") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          walletNotExists(wallet).getOrElse {
            Try(Json.parse(body)).map { js =>
              js.validate[Payment] match {
                case err: JsError =>
                  WrongTransactionJson(err).response
                case JsSuccess(payment: Payment, _) =>
                  val txOpt = transactionModule.signPayment(payment, wallet)
                  txOpt match {
                    case Some(tx) =>
                      tx.validate match {
                        case ValidationResult.ValidateOke =>
                          val signed = SignedPayment(tx.timestamp, tx.amount, tx.fee, tx.recipient,
                            tx.sender, tx.sender.address, Base58.encode(tx.signature))
                          JsonResponse(Json.toJson(signed), StatusCodes.OK)

                        case ValidationResult.InvalidAddress => InvalidAddress.response
                        case ValidationResult.NegativeAmount => NegativeAmount.response
                        case ValidationResult.InsufficientFee => InsufficientFee.response
                        case ValidationResult.NoBalance => NoBalance.response
                      }
                    case None => InvalidSender.response
                  }
              }
            }.getOrElse(WrongJson.response)
          }
        }
      }
    }
  }

  @Deprecated
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
      dataType = "scorex.waves.http.UnsignedPayment"
    )
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with response or error")
  ))
  def createdSignedPayment: Route = path("create-signed-payment") {
    entity(as[String]) { body =>
      postJsonRoute {
        Try(Json.parse(body)).map { js =>
          js.validate[UnsignedPayment] match {
            case err: JsError =>
              WrongTransactionJson(err).response
            case JsSuccess(payment: UnsignedPayment, _) =>
              val senderWalletSeed = Base58.decode(payment.senderWalletSeed).getOrElse(Array.empty)
              if (senderWalletSeed.isEmpty)
                WrongJson.response
              else {
                val senderAccount = Wallet.generateNewAccount(senderWalletSeed, payment.senderAddressNonce)
                val recipientAccount = new Account(payment.recipient)

                transactionModule.createSignedPayment(senderAccount, recipientAccount,
                  payment.amount, payment.fee, payment.timestamp) match {
                  case Right(tx) =>
                    val signedTx = SignedPayment(tx.timestamp, tx.amount, tx.fee, tx.recipient,
                      tx.sender, tx.sender.address, Base58.encode(tx.signature))
                    JsonResponse(Json.toJson(signedTx), StatusCodes.OK)

                  case Left(e) => e match {
                    case ValidationResult.NoBalance => NoBalance.response
                    case ValidationResult.InvalidAddress => InvalidAddress.response
                  }
                }
              }
          }
        }.getOrElse(WrongJson.response)
      }
    }
  }

  private def toErrorResponce(error: JsError): JsonResponse = {
    val errors = error.errors.map(_._1.toString).toSet
    if (errors.contains("/recipient")) {
      InvalidRecipient.response
    } else if (errors.contains("/sender")) {
      InvalidSender.response
    } else if (errors.contains("/senderPublicKey")) {
      InvalidSender.response
    } else if (errors.contains("/signature")) {
      InvalidSignature.response
    } else {
      WrongJson.response
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
  def externalPayment: Route = path("external-payment") {
    entity(as[String]) { body =>
      postJsonRoute {
        Try {
          val js = Json.parse(body)
          js.validate[ExternalPayment] match {
            case error: JsError =>
              toErrorResponce(error)
            case JsSuccess(payment: ExternalPayment, _) =>
              broadcastPayment(payment)
          }
        }.getOrElse(WrongJson.response)
      }
    }
  }

  @Deprecated()
  @Path("/broadcast-signed-payment")
  @ApiOperation(value = "Broadcast signed payment. Deprecated: use /assets/broadcast/transfer instead",
    notes = "Publish signed payment to the Blockchain. Deprecated: use /assets/broadcast/transfer instead",
    httpMethod = "POST", produces = "application/json", consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.waves.transaction.SignedPayment",
      defaultValue = "{\n\t\"timestamp\": 0,\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"senderPublicKey\":\"senderPubKey\",\n\t\"senderAddress\":\"senderAddress\",\n\t\"recipient\":\"recipientId\",\n\t\"signature\":\"sig\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def broadcastSignedPayment: Route = path("broadcast-signed-payment") {
    entity(as[String]) { body =>
      postJsonRoute {
        Try(Json.parse(body)).map { js =>
          js.validate[SignedPayment] match {
            case error: JsError =>
              toErrorResponce(error)
            case JsSuccess(payment: SignedPayment, _) =>
              broadcastPayment(payment)
          }
        }.getOrElse(WrongJson.response)
      }
    }
  }

  private def broadcastPayment(payment: SignedPayment): JsonResponse = {
    transactionModule.broadcastPayment(payment) match {
      case Right(tx) =>
        if (!tx.signatureValid) InvalidSignature.response
        else {
          tx.validate match {
            case ValidationResult.ValidateOke =>
              JsonResponse(tx.json, StatusCodes.OK)

            case ValidationResult.InvalidAddress => InvalidAddress.response
            case ValidationResult.NegativeAmount => NegativeAmount.response
            case ValidationResult.InsufficientFee => InsufficientFee.response
          }
        }
      case Left(e) => e match {
        case ValidationResult.NoBalance => NoBalance.response
        case ValidationResult.InvalidAddress => InvalidAddress.response
        case ValidationResult.InsufficientFee => InsufficientFee.response
        case ValidationResult.InvalidSignature => InvalidSignature.response
        case _ => Unknown.response
      }
    }
  }

  @Deprecated
  private def broadcastPayment(payment: ExternalPayment): JsonResponse = {
    val senderAccount = payment.senderPublicKey
    val signedPayment = SignedPayment(payment.timestamp, payment.amount, payment.fee, payment.recipient,
      payment.senderPublicKey, senderAccount.address, Base58.encode(payment.signature))

    transactionModule.broadcastPayment(signedPayment) match {
      case Right(tx) =>
        if (!tx.signatureValid) InvalidSignature.response
        else {
          tx.validate match {
            case ValidationResult.ValidateOke =>
              JsonResponse(tx.json, StatusCodes.OK)

            case ValidationResult.InvalidAddress => InvalidAddress.response
            case ValidationResult.NegativeAmount => NegativeAmount.response
            case ValidationResult.InsufficientFee => InsufficientFee.response
          }
        }
      case Left(e) => e match {
        case ValidationResult.NoBalance => NoBalance.response
        case ValidationResult.InvalidAddress => InvalidAddress.response
        case ValidationResult.InsufficientFee => NegativeFee.response
        case ValidationResult.InvalidSignature => InvalidSignature.response
        case _ => Unknown.response
      }
    }
  }
}
