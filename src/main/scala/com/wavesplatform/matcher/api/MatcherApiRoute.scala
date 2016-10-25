package com.wavesplatform.matcher.api

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCode, StatusCodes}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import com.wavesplatform.matcher.market.MatcherActor.OrderResponse
import scorex.transaction.assets.exchange.OrderJson._
import com.wavesplatform.settings.WavesSettings
import io.swagger.annotations._
import play.api.libs.json._
import scorex.api.http._
import scorex.app.Application
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.{Order, Validation}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.wallet.Wallet

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

case class ValidationErrorJson(err: Validation) extends ApiError {
  override val id: Int = 120
  override val message: String =  s"Validation failed: ${err.messages}"
  override val code: StatusCode = StatusCodes.BadRequest
}

@Path("/matcher")
@Api(value = "/matcher/")
case class MatcherApiRoute(application: Application, matcher: ActorRef)(implicit val settings: WavesSettings,
                                              implicit val context: ActorRefFactory) extends ApiRoute with OrderService {

  val wallet: Wallet = application.wallet
  val storedState: StoredState = application.blockStorage.state.asInstanceOf[StoredState]

  def postJsonRouteAsync(fn: Future[JsonResponse]): Route = {
    onSuccess(fn) {res: JsonResponse =>
      complete(res.code -> HttpEntity(ContentTypes.`application/json`, res.response.toString))
    }
  }

  override lazy val route =
    pathPrefix("matcher") {
      place ~ matcherPubKey ~ signOrder ~ balance
    }

  @Path("/publicKey")
  @ApiOperation(value = "Matcher Public Key", notes = "Get matcher public key", httpMethod = "GET")
  def matcherPubKey: Route = {
    path("publicKey") {
      getJsonRoute {
        val json = wallet.privateKeyAccount(settings.matcherAccount).map(a => JsString(Base58.encode(a.publicKey))).
          getOrElse(JsString(""))
        JsonResponse(json, StatusCodes.OK)
      }
    }
  }

  @Path("/publicKey/{address}")
  @ApiOperation(value = "Public Key", notes = "Account Public Key", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def balance: Route = {
    path("publicKey" / Segment) { case address =>
      getJsonRoute {
        val json = wallet.privateKeyAccount(address).map(a => JsString(Base58.encode(a.publicKey))).
          getOrElse(JsString(""))
        JsonResponse(json, StatusCodes.OK)
      }
    }
  }

  @Path("/sign")
  @ApiOperation(value = "Sign Order",
    notes = "Create order signed by address from wallet",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Order Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.assets.exchange.Order"
    )
  ))
  def signOrder: Route = path("sign") {
    entity(as[String]) { body =>
      postJsonRouteAsync {
        Try(Json.parse(body)).map { js =>
          js.validate[Order] match {
            case err: JsError =>
              Future.successful(WrongTransactionJson(err).response)
            case JsSuccess(order: Order, _) =>
              Future {
                wallet.privateKeyAccount(order.sender.address).map { sender =>
                  val signed = Order.sign(order, sender)
                  JsonResponse(signed.json, StatusCodes.OK)
                }.getOrElse(InvalidAddress.response)
              }
          }
        }.recover {
          case t => println(t)
            Future.successful(WrongJson.response)
        }.get
      }
    }
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
              val v = validateOrder(order)
              if (v) {
                (matcher ? order)
                  .mapTo[OrderResponse]
                  .map { resp =>
                    JsonResponse(resp.json, StatusCodes.OK)
                  }
              } else {
                Future.successful(ValidationErrorJson(v).response)
              }
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

}