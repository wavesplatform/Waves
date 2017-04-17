package scorex.api.http

import javax.ws.rs.Path
import scala.util.Success
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.RestAPISettings
import io.swagger.annotations._
import play.api.libs.json.{JsArray, JsNumber, Json}
import scorex.account.Account
import scorex.crypto.encode.Base58
import scorex.transaction.{History, LagonakiState, SimpleTransactionModule}

@Path("/transactions")
@Api(value = "/transactions", description = "Information about transactions")
case class TransactionsApiRoute(
    settings: RestAPISettings,
    state: LagonakiState,
    history: History,
    transactionModule: SimpleTransactionModule) extends ApiRoute with CommonApiFunctions {
  val MaxTransactionsPerRequest = 1000

  override lazy val route =
    pathPrefix("transactions") {
      unconfirmed ~ addressLimit ~ info
    }

  //TODO implement general pagination
  @Path("/address/{address}/limit/{limit}")
  @ApiOperation(value = "Address", notes = "Get list of transactions where specified address has been involved", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Wallet address ", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "limit", value = "Specified number of records to be returned", required = true, dataType = "integer", paramType = "path")
  ))
  def addressLimit: Route = (path("address" / Segment / "limit" / IntNumber) & get) { case (address, limit) =>
    if (limit <= MaxTransactionsPerRequest) {
      val account = new Account(address)
      val txJsons = state.accountTransactions(account, limit).map(_.json)
      complete(Json.arr(txJsons))
    } else complete(TooBigArrayAllocation)
  }

  @Path("/info/{signature}")
  @ApiOperation(value = "Info", notes = "Get transaction info", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "signature", value = "transaction signature ", required = true, dataType = "string", paramType = "path")
  ))
  def info: Route = (path("info" / Segment) & get) { encoded =>
    Base58.decode(encoded) match {
      case Success(sig) =>
        state.included(sig) match {
          case Some(h) =>
            val jsonOpt = for {
              b <- history.blockAt(h)
              tx <- b.transactionData.collectFirst { case t if t.id sameElements sig => t }
            } yield tx.json + ("height" -> JsNumber(h))

            jsonOpt match {
              case Some(json) => complete(json)
              case None =>
                complete(StatusCodes.InternalServerError -> Json.obj("status" -> "error", "details" -> "Internal error"))
            }

          case None =>
            complete(StatusCodes.NotFound -> Json.obj("status" -> "error", "details" -> "Transaction is not in blockchain"))
        }
      case _ =>
        complete(StatusCodes.UnprocessableEntity -> Json.obj("status" -> "error", "details" -> "Incorrect signature"))
    }
  }

  @Path("/unconfirmed")
  @ApiOperation(value = "Unconfirmed", notes = "Get list of unconfirmed transactions", httpMethod = "GET")
  def unconfirmed: Route = (path("unconfirmed") & get) {
    complete(JsArray(transactionModule.unconfirmedTxs.map(_.json)))
  }
}
