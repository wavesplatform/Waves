package scorex.api.http.assets

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import scorex.api.http._
import scorex.app.Application
import scorex.settings.Settings
import scorex.transaction.{SignedTransaction, SimpleTransactionModule, ValidationResult}

import scala.util.Try

@Path("/assets/broadcast")
@Api(value = "assets")
case class AssetsBroadcastApiRoute(application: Application)(implicit val context: ActorRefFactory) extends ApiRoute
  with CommonTransactionApiFunctions {
  override val settings: Settings = application.settings
  val transactionModule = application.transactionModule.asInstanceOf[SimpleTransactionModule]

  override val route: Route = pathPrefix("assets" / "broadcast") {
    issue ~ reissue ~ transfer
  }

  import BroadcastRequests._
  import BroadcastResponses._

  @Path("/issue")
  @ApiOperation(value = "Broadcast signed Asset issue",
    notes = "Publish signed Asset issue transaction to the Blockchain",
    httpMethod = "POST",
    consumes = "application/json",
    produces = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with signed Issue transaction",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.assets.BroadcastRequests$AssetIssueRequest")))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with signed Asset issue transaction contained Asset ID", response = classOf[AssetIssueResponse]),
    new ApiResponse(code = 400, message = "Json with error description", response = classOf[ApiErrorResponse])))
  def issue: Route = path("issue") {
    entity(as[String]) { body =>
      postJsonRoute {
        Try(Json.parse(body)).map { js =>
          js.validate[AssetIssueRequest] match {
            case JsSuccess(request: AssetIssueRequest, _) =>
              request.toTx.map { tx =>
                broadcast(tx)(t => Json.toJson(AssetIssueResponse(t)))
              }.getOrElse(WrongJson.response)

            case _: JsError => WrongJson.response
          }
        }.getOrElse(WrongJson.response)
      }
    }
  }

  @Path("/reissue")
  @ApiOperation(value = "Broadcast signed Asset reissue",
    notes = "Publish signed Asset reissue transaction to the Blockchain",
    httpMethod = "POST",
    consumes = "application/json",
    produces = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with signed Reissue transaction",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.assets.BroadcastRequests$AssetReissueRequest")))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with signed Asset reissue transaction", response = classOf[AssetReissueResponse]),
    new ApiResponse(code = 400, message = "Json with error description", response = classOf[ApiErrorResponse])))
  def reissue: Route = path("reissue") {
    entity(as[String]) { body =>
      postJsonRoute {
        Try(Json.parse(body)).map { js =>
          js.validate[AssetReissueRequest] match {
            case JsSuccess(request: AssetReissueRequest, _) =>
              request.toTx.map { tx =>
                broadcast(tx)(t => Json.toJson(AssetReissueResponse(t)))
              }.getOrElse(WrongJson.response)

            case _: JsError => WrongJson.response
          }
        }.getOrElse(WrongJson.response)
      }
    }
  }

  @Path("/transfer")
  @ApiOperation(value = "Broadcast signed Asset transfer",
    notes = "Publish signed Asset transfer transaction to the Blockchain",
    httpMethod = "POST",
    consumes = "application/json",
    produces = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with signed Transfer transaction",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.assets.BroadcastRequests$AssetTransferRequest")))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with signed Asset transfer transaction", response = classOf[AssetTransferResponse]),
    new ApiResponse(code = 400, message = "Json with error description", response = classOf[ApiErrorResponse])))
  def transfer: Route = path("transfer") {
    entity(as[String]) { body =>
      postJsonRoute {
        Try(Json.parse(body)).map { js =>
          js.validate[AssetTransferRequest] match {
            case JsSuccess(request: AssetTransferRequest, _) =>
              request.toTx.map { tx =>
                broadcast(tx)(t => Json.toJson(AssetTransferResponse(t)))
              }.getOrElse(WrongJson.response)

            case _: JsError => WrongJson.response
          }
        }.getOrElse(WrongJson.response)
      }
    }
  }

  private def broadcast[T <: SignedTransaction](tx: T)(toJson: T => JsValue): JsonResponse =
    Try(transactionModule.broadcastTransaction(tx)).map {
      case ValidationResult.ValidateOke => JsonResponse(toJson(tx), StatusCodes.OK)
      case error => jsonResponse(error)
    }.getOrElse(WrongJson.response)

}
