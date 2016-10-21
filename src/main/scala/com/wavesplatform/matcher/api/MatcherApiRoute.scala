package com.wavesplatform.matcher.api

import javax.ws.rs.Path

import scala.concurrent.Future
import scala.util.Try

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCode, StatusCodes}
import akka.http.scaladsl.server.Route

import com.wavesplatform.matcher.model.OrderJson._
import com.wavesplatform.settings.WavesSettings
import io.swagger.annotations._
import play.api.libs.json._
import scorex.api.http._
import scorex.transaction.assets.exchange.{Order, Validation}

case class ValidationErrorJson(err: Validation) extends ApiError {
  override val id: Int = 120
  override val message: String =  s"Validation failed: ${err.messages}"
  override val code: StatusCode = StatusCodes.BadRequest
}

@Path("/matcher")
@Api(value = "matcher", produces = "application/json", consumes = "application/json")
case class MatcherApiRoute(matcher: ActorRef)(implicit val settings: WavesSettings,
                                              implicit val context: ActorRefFactory) extends ApiRoute with OrderService {

  def postJsonRouteAsync(fn: Future[JsonResponse]): Route = {
    onSuccess(fn) {res: JsonResponse =>
      complete(res.code -> HttpEntity(ContentTypes.`application/json`, res.response.toString))
    }
  }

  override lazy val route =
    pathPrefix("matcher") {
      place//~ cancel ~ getUnsigned
    }

  val smallRoute: Route =
    pathPrefix("matcher") {
      get {
        pathSingleSlash {
          complete {
            "Captain on the bridge!"
          }
        } ~
          path("ping") {
            complete("PONG!")
          }
      }
    }
  /*
    @Path("/order/cancel")
    @ApiOperation(value = "Cancel",
      notes = "Calncel your order",
      httpMethod = "POST",
      produces = "application/json",
      consumes = "application/json")
    @ApiImplicitParams(Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.matcher.CancelJS",
        defaultValue = "{\n\t\"spendAddress\":\"spendAddress\",\n\t\"OrderID\":0,\n\t\"signature\":\"signature\"\n}"
      )
    ))
    def cancel: Route = path("order/cancel") {
      withCors {
        entity(as[String]) { body =>
          postJsonRoute {
            Try(Json.parse(body)).map { js =>
              js.validate[CancelJS] match {
                case err: JsError =>
                  WrongTransactionJson(err).response
                case JsSuccess(cancelJS: CancelJS, _) =>
                  cancelJS.cancel match {
                    case Success(cancelOrder) if cancelOrder.isValid =>
                      //TODO signed message what order is cancelled (with remaining amount)
                      JsonResponse(Json.obj("cancelled" -> matcher.cancel(cancelOrder)), StatusCodes.OK)
                    case _ => WrongJson.response
                  }
              }
            }.getOrElse(WrongJson.response)
          }
        }
      }
    }
  */
  /*
  @Path("/order/place")
  @ApiOperation(value = "Place",
    notes = "Place new order",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "com.wavesplatform.matcher.model.Order",
      defaultValue = "{\n\t\"spendAddress\":\"spendAddress\",\n\t\"spendTokenID\":\"spendTokenID\",\n\t\"receiveTokenID\":\"receiveTokenID\",\n\t\"price\":1,\n\t\"amount\":1,\n\t\"signature\":\"signature\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def place: Route = path("order/place") {
    withCors {
      entity(as[String]) { body =>
        postJsonRoute {
          Try(Json.parse(body)).map { js =>
            js.validate[OrderJS] match {
              case err: JsError =>
                WrongTransactionJson(err).response
              case JsSuccess(orderjs: OrderJS, _) =>
                orderjs.order match {
                  case Success(order) if order.isValid =>
                    val resp = matcher.place(order)
                    JsonResponse(Json.obj("accepted" -> resp.json), StatusCodes.OK)
                  case _ => WrongJson.response
                }
            }
          }.getOrElse(WrongJson.response)
        }
      }
    }
  }*/

  @Path("/orders/place")
  @ApiOperation(value = "Place order",
    notes = "Place a new limit order (buy or sell)",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.assets.exchange.Order"
    )
  ))
  def place: Route = path("orders" / "place") {
    entity(as[String]) { body =>
      postJsonRouteAsync {
        Try(Json.parse(body)).map { js =>
          js.validate[Order] match {
            case err: JsError =>
              Future.successful(WrongTransactionJson(err).response)
            case JsSuccess(order: Order, _) =>
              //val validation = OrderValidator(order)
              Future.successful(JsonResponse(order.json, StatusCodes.OK))
              /*if (validation.validate()) {
                (matcher ? order)
                .mapTo[OrderResponse]
                .map { resp =>
                  JsonResponse(resp.json, StatusCodes.OK)
                }
              } else {
                Future.successful(ValidationErrorJson(validation.errors).response)
              }*/
          }
        }.recover {
          case t => println(t)
            Future.successful(WrongJson.response)
        }.get
      }
    }
  }

  /*def place: Route = path("order/place") {
  withCors {
    entity(as[Order]) { order =>
      postJsonRoute {
        if (order.isValid) {
          //val resp = matcher.place(order)
          JsonResponse(Json.obj("accepted" -> "123"), StatusCodes.OK)
        } else WrongJson.response
      }
    }
  }
}*/

  /*
    @Path("/transaction/{address}")
    @ApiOperation(value = "Transactions", notes = "Get transactions to sign", httpMethod = "GET")
    @ApiResponses(Array(
      new ApiResponse(code = 200, message = "Json Waves node version")
    ))
    @ApiImplicitParams(Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
    ))
    def getUnsigned: Route = {
      path("transaction" / Segment) { case address =>
        getJsonRoute {
          JsonResponse(Json.obj("version" -> "123"), StatusCodes.OK)
        }
      }
    }

    @Path("/transaction/sign")
    @ApiOperation(value = "Sign",
      notes = "Sign matched transaction",
      httpMethod = "POST",
      produces = "application/json",
      consumes = "application/json")
    @ApiImplicitParams(Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        defaultValue = "{\n\t\"transactionId\":\"transactionId\",\n\t\"signature\":\"signature\"\n}"
      )
    ))
    @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
    def sign: Route = path("transaction/sign") {
      withCors {
        entity(as[String]) { body =>
          postJsonRoute {
            Try {
              val js = Json.parse(body)
              val signature = Base58.decode((js \ "signature").as[String]).get
              val transactionId = Base58.decode((js \ "transactionId").as[String]).get
              val signedTx = matcher.sign(transactionId, signature)
              signedTx match {
                case Success(tx) =>
                  if (tx.isCompleted) {
                    val ntwMsg = Message(ExchangeTransactionMessageSpec, Right(tx), None)
                    application.networkController ! NetworkController.SendToNetwork(ntwMsg, Broadcast)
                  }
                  JsonResponse(Json.obj("signed" -> true, "broadcasted" -> tx.isCompleted), StatusCodes.OK)
                case Failure(e) =>
                  JsonResponse(Json.obj("signed" -> false, "error" -> e.getMessage), StatusCodes.OK)
              }
            }.getOrElse(WrongJson.response)
          }
        }
      }
    }
  */

}


