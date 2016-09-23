package scorex.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json.{JsError, JsSuccess, Json}
import scorex.account.Account
import scorex.app.RunnableApplication
import scorex.crypto.encode.Base58
import scorex.transaction.{SimpleTransactionModule, AssetAcc}
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.state.wallet.IssueRequest

import scala.util.{Success, Try}


@Path("/assets")
@Api(value = "/assets/", description = "Info about assets")
case class AssetsApiRoute(application: RunnableApplication)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {
  val MaxAddressesPerRequest = 1000

  val settings = application.settings

  private val wallet = application.wallet
  private val state = application.blockStorage.state.asInstanceOf[StoredState]
  private implicit val transactionModule = application.transactionModule.asInstanceOf[SimpleTransactionModule]

  override lazy val route =
    pathPrefix("assets") {
      balance
    }

  @Path("/balance/{address}/{assetId}")
  @ApiOperation(value = "Balance", notes = "Account's balance", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path"),
    new ApiImplicitParam(name = "assetId", value = "Asset id", required = true, dataType = "String", paramType = "path")
  ))
  def balance: Route = {
    path("balance" / Segment / Segment) { case (address, assetId) =>
      getJsonRoute {
        balanceJson(address, assetId)
      }
    }
  }

  @Path("/issue")
  @ApiOperation(value = "Issue", notes = "Issue new asset", httpMethod = "POST")
  def issue: Route = {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          walletNotExists(wallet).getOrElse {
            Try(Json.parse(body)).map { js =>
              js.validate[IssueRequest] match {
                case err: JsError =>
                  WrongJson.response
                case JsSuccess(issue: IssueRequest, _) =>
                  val txOpt: Option[IssueTransaction] = transactionModule.issueAsset(issue, wallet)
                  txOpt match {
                    case Some(tx) =>
                      JsonResponse(tx.json, StatusCodes.OK)
                    case None =>
                      WrongJson.response
                  }
              }
            }.getOrElse(WrongJson.response)
          }
        }
      }
    }
  }


  private def balanceJson(address: String, assetIdStr: String): JsonResponse = {
    val account = new Account(address)
    Base58.decode(assetIdStr) match {
      case Success(assetId) if Account.isValid(account) =>
        val json = Json.obj(
          "address" -> account.address,
          "assetId" -> assetIdStr,
          "balance" -> state.assetBalance(AssetAcc(account, Some(assetId)))
        )
        JsonResponse(json, StatusCodes.OK)
      case _ => InvalidAddress.response
    }
  }

}
