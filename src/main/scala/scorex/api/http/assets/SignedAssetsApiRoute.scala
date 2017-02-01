package scorex.api.http.assets

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json._
import scorex.api.http._
import scorex.app.Application
import scorex.settings.Settings
import scorex.transaction.{SignedTransaction, SimpleTransactionModule, StateCheckFailed, ValidationError}

import scala.util.{Failure, Success, Try}

@Path("/assets/broadcast")
@Api(value = "assets")
case class SignedAssetsApiRoute(application: Application)(implicit val context: ActorRefFactory) extends ApiRoute
  with CommonTransactionApiFunctions {
  override val settings: Settings = application.settings
  val transactionModule = application.transactionModule.asInstanceOf[SimpleTransactionModule]

  override val route: Route = pathPrefix("assets" / "broadcast") {
    issue ~ reissue ~ transfer ~ burnRoute ~ batchTransfer
  }

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
      dataType = "scorex.api.http.assets.BroadcastRequests$SignedAssetIssueRequest")))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with signed Asset issue transaction contained Asset ID"),
    new ApiResponse(code = 400, message = "Json with error description", response = classOf[ApiErrorResponse])))
  def issue: Route = path("issue") {
    entity(as[String]) { body =>
      postJsonRoute {
        Try(Json.parse(body)).map { js =>
          js.validate[SignedAssetIssueRequest] match {
            case JsSuccess(request: SignedAssetIssueRequest, _) =>
              request.toTx.map(broadcast).getOrElse(WrongJson.response)

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
      dataType = "scorex.api.http.assets.SignedAssetReissueRequest")))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with signed Asset reissue transaction"),
    new ApiResponse(code = 400, message = "Json with error description", response = classOf[ApiErrorResponse])))
  def reissue: Route = path("reissue") {
    entity(as[String]) { body =>
      postJsonRoute {
        Try(Json.parse(body)).map { js =>
          js.validate[SignedAssetReissueRequest] match {
            case JsSuccess(request: SignedAssetReissueRequest, _) =>
              request.toTx.map(broadcast).getOrElse(WrongJson.response)

            case _: JsError => WrongJson.response
          }
        }.getOrElse(WrongJson.response)
      }
    }
  }

  @Path("/burn")
  @ApiOperation(value = "Broadcast signed Asset burn transaction",
    notes = "Publish signed Asset burn transaction to the Blockchain",
    httpMethod = "POST",
    consumes = "application/json",
    produces = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with signed Burn transaction",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.assets.SignedAssetBurnRequest")))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with signed Asset burn transaction"),
    new ApiResponse(code = 400, message = "Json with error description", response = classOf[ApiErrorResponse])))
  def burnRoute: Route = path("burn") {
    entity(as[String]) { body =>
      postJsonRoute {
        Try(Json.parse(body)).map { js =>
          js.validate[SignedAssetBurnRequest] match {
            case JsSuccess(request: SignedAssetBurnRequest, _) =>
              request.toTx.map (broadcast)
              .getOrElse(WrongJson.response)

            case _: JsError => WrongJson.response
          }
        }.getOrElse(WrongJson.response)
      }
    }
  }

  @Path("/batch_transfer")
  @ApiOperation(value = "Batch transfer operation",
    notes = "Transfer assets to new addresses",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Array json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.assets.SignedAssetBurnRequest",
      allowMultiple = true,
      defaultValue = "[{\n  \"assetId\": \"E9yZC4cVhCDfbjFJCc9CqkAtkoFy5KaCe64iaxHM2adG\",\n  \"senderPublicKey\": \"CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw\",\n  \"recipient\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n  \"fee\": 100000,\n  \"amount\": 5500000000,\n  \"attachment\": \"BJa6cfyGUmzBFTj3vvvaew\",\n  \"timestamp\": 1479222433704, \n  \"signature\": \"2TyN8pNS7mS9gfCbX2ktpkWVYckoAmRmDZzKH3K35DKs6sUoXHArzukV5hvveK9t79uzT3cA8CYZ9z3Utj6CnCEo\"\n, {\n  \"assetId\": \"E9yZC4cVhCDfbjFJCc9CqkAtkoFy5KaCe64iaxHM2adG\",\n  \"senderPublicKey\": \"CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw\",\n  \"recipient\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n  \"fee\": 100000,\n  \"amount\": 5500000000,\n  \"attachment\": \"BJa6cfyGUmzBFTj3vvvaew\",\n  \"timestamp\": 1479222433704, \n  \"signature\": \"2TyN8pNS7mS9gfCbX2ktpkWVYckoAmRmDZzKH3K35DKs6sUoXHArzukV5hvveK9t79uzT3cA8CYZ9z3Utj6CnCEo\"\n}]"
    )
  ))
  def batchTransfer: Route = path("batch_transfer") {
    entity(as[String]) { body =>
      postJsonRoute {
        Try(Json.parse(body)).map { js =>
          js.validate[Array[SignedAssetTransferRequest]] match {
            case err: JsError =>
              WrongTransactionJson(err).response
            case JsSuccess(requests: Array[SignedAssetTransferRequest], _) =>
              val validTransactionsOpt = listTry2TryList(requests.map(_.toTx))
              validTransactionsOpt match {
                case Success(txs) =>
                  broadcastMany(txs)(txs => JsArray(txs.map(_.json)))
                case Failure(e: StateCheckFailed) =>
                  StateCheckFailed.response
                case _ =>
                  WrongJson.response
              }
          }
        }.getOrElse(WrongJson.response)
      }
    }
  }

  private def listTry2TryList[T <: AnyRef](tries: Iterable[Try[T]]): Try[Seq[T]] = {
    tries.foldLeft[Try[Seq[T]]](Success(Seq.empty)) {
      case (foldSeq, resultTry) =>
        for {
          seq <- foldSeq
          t <- resultTry
        } yield {
          seq :+ t
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
      dataType = "scorex.api.http.assets.SignedAssetBurnRequest")))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with signed Asset transfer transaction"),
    new ApiResponse(code = 400, message = "Json with error description", response = classOf[ApiErrorResponse])))
  def transfer: Route = path("transfer") {
    entity(as[String]) { body =>
      postJsonRoute {
        Try(Json.parse(body)).map { js =>
          js.validate[SignedAssetTransferRequest] match {
            case JsSuccess(request: SignedAssetTransferRequest, _) =>
              request.toTx.map(broadcast).getOrElse(WrongJson.response)

            case _: JsError => WrongJson.response
          }
        }.getOrElse(WrongJson.response)
      }
    }
  }

  private def broadcast[T <: SignedTransaction](tx: T) : JsonResponse =
    Try(transactionModule.broadcastTransaction(tx)).map {
      case Right(()) => JsonResponse(tx.json, StatusCodes.OK)
      case Left(error)=> jsonResponse(error)
    }.getOrElse(WrongJson.response)

  private def broadcastMany[T <: SignedTransaction](txs: Seq[T])(toJson: Seq[T] => JsValue): JsonResponse =
    Try(transactionModule.broadcastTransactions(txs)).map {
      case Right(()) => JsonResponse(toJson(txs), StatusCodes.OK)
      case Left(error) => jsonResponse(error)
    }.getOrElse(WrongJson.response)

}
