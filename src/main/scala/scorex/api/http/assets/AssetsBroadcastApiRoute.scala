package scorex.api.http.assets

import javax.ws.rs.Path
import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.RestAPISettings
import io.swagger.annotations._
import scorex.api.http._
import scorex.transaction.{Transaction, TransactionModule, ValidationError}

@Path("/assets/broadcast")
@Api(value = "assets")
case class AssetsBroadcastApiRoute(settings: RestAPISettings, transactionModule: TransactionModule)
  extends ApiRoute {

  override val route: Route = pathPrefix("assets" / "broadcast") {
    issue ~ reissue ~ transfer ~ burnRoute ~ batchTransfer
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
  def issue: Route = (path("issue") & post) {
    json[AssetIssueRequest] { issueReq =>
      doBroadcast(issueReq.toTx)
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
  def reissue: Route = (path("reissue") & post) {
    json[AssetReissueRequest] { reissueReq =>
      doBroadcast(reissueReq.toTx)
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
      dataType = "scorex.api.http.assets.BroadcastRequests$AssetBurnRequest")))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with signed Asset burn transaction", response = classOf[AssetBurnResponse]),
    new ApiResponse(code = 400, message = "Json with error description", response = classOf[ApiErrorResponse])))
  def burnRoute: Route = (path("burn") & post) {
    json[AssetBurnRequest] { burnReq =>
      doBroadcast(burnReq.toTx)
    }
  }

  @Path("/batch-transfer")
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
      dataType = "scorex.api.http.assets.BroadcastRequests$AssetTransferRequest",
      allowMultiple = true,
      defaultValue = "[{\n  \"assetId\": \"E9yZC4cVhCDfbjFJCc9CqkAtkoFy5KaCe64iaxHM2adG\",\n  \"senderPublicKey\": \"CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw\",\n  \"recipient\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n  \"fee\": 100000,\n  \"amount\": 5500000000,\n  \"attachment\": \"BJa6cfyGUmzBFTj3vvvaew\",\n  \"timestamp\": 1479222433704, \n  \"signature\": \"2TyN8pNS7mS9gfCbX2ktpkWVYckoAmRmDZzKH3K35DKs6sUoXHArzukV5hvveK9t79uzT3cA8CYZ9z3Utj6CnCEo\"\n, {\n  \"assetId\": \"E9yZC4cVhCDfbjFJCc9CqkAtkoFy5KaCe64iaxHM2adG\",\n  \"senderPublicKey\": \"CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw\",\n  \"recipient\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n  \"fee\": 100000,\n  \"amount\": 5500000000,\n  \"attachment\": \"BJa6cfyGUmzBFTj3vvvaew\",\n  \"timestamp\": 1479222433704, \n  \"signature\": \"2TyN8pNS7mS9gfCbX2ktpkWVYckoAmRmDZzKH3K35DKs6sUoXHArzukV5hvveK9t79uzT3cA8CYZ9z3Utj6CnCEo\"\n}]"
    )
  ))
  def batchTransfer: Route = (path("batch-transfer") & post) {
    json[Seq[AssetTransferRequest]] { reqs =>
      val tr = reqs.map(r => doBroadcast(r.toTx))
      tr.map(_.fold(_.json, _.json))
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
  def transfer: Route = (path("transfer") & post) {
    json[AssetTransferRequest] { transferReq =>
      doBroadcast(transferReq.toTx)
    }
  }

  private def doBroadcast[A <: Transaction](v: Either[ValidationError, A]) =
    v.left.map(ApiError.fromValidationError).flatMap(broadcast)

  private def broadcast[T <: Transaction](tx: T): Either[ApiError, T] =
    if (transactionModule.onNewOffchainTransaction(tx)) Right(tx) else Left(StateCheckFailed)
}
