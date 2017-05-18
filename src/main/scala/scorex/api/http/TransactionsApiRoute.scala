package scorex.api.http

import javax.ws.rs.Path

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.reader.StateReader
import io.swagger.annotations._
import play.api.libs.json._
import scorex.account.Account
import scorex.crypto.encode.Base58
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.{History, Transaction, NewTransactionHandler, UnconfirmedTransactionsStorage}

import scala.util.Success
import scala.util.control.Exception

@Path("/transactions")
@Api(value = "/transactions", description = "Information about transactions")
case class TransactionsApiRoute(
                                 settings: RestAPISettings,
                                 state: StateReader,
                                 history: History,
                                 utxStorage: UnconfirmedTransactionsStorage) extends ApiRoute with CommonApiFunctions {

  import TransactionsApiRoute.MaxTransactionsPerRequest

  override lazy val route =
    pathPrefix("transactions") {
      unconfirmed ~ addressLimit ~ info
    }

  private val invalidLimit = StatusCodes.BadRequest -> Json.obj("message" -> "invalid.limit")
  //TODO implement general pagination
  @Path("/address/{address}/limit/{limit}")
  @ApiOperation(value = "Address", notes = "Get list of transactions where specified address has been involved", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Wallet address ", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "limit", value = "Specified number of records to be returned", required = true, dataType = "integer", paramType = "path")
  ))
  def addressLimit: Route = (pathPrefix("address") & get) {
    pathPrefix(Segment) { address =>
      Account.fromString(address) match {
        case Left(e) => complete(ApiError.fromValidationError(e))
        case Right(a) =>
          pathPrefix("limit") {
            pathEndOrSingleSlash {
              complete(invalidLimit)
            } ~
              path(Segment) { limitStr =>
                Exception.allCatch.opt(limitStr.toInt) match {
                  case Some(limit) if limit > 0 && limit <= MaxTransactionsPerRequest =>
                    complete(JsArray(state.accountTransactions(a, limit).map(txToExtendedJson)))
                  case Some(limit) if limit > MaxTransactionsPerRequest =>
                    complete(TooBigArrayAllocation)
                  case _ =>
                    complete(invalidLimit)
                }
              }
          } ~ complete(StatusCodes.NotFound)
      }
    }
  }

  @Path("/info/{signature}")
  @ApiOperation(value = "Info", notes = "Get transaction info", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "signature", value = "transaction signature ", required = true, dataType = "string", paramType = "path")
  ))
  def info: Route = (pathPrefix("info") & get) {
    pathEndOrSingleSlash {
      complete(InvalidSignature)
    } ~
      path(Segment) { encoded =>
        Base58.decode(encoded) match {
          case Success(sig) =>
            state.included(sig) match {
              case Some(h) =>
                val jsonOpt = for {
                  b <- history.blockAt(h).toRight(s"Block height=$h not found")
                  tx <- b.transactionData.collectFirst { case t if t.id sameElements sig => t }.toRight(s"Tx not found in block")
                } yield txToExtendedJson(tx) + ("height" -> JsNumber(h))

                jsonOpt match {
                  case Right(json) => complete(json)
                  case Left(err) =>
                    complete(StatusCodes.InternalServerError -> Json.obj("status" -> "error", "details" -> s"Internal error: $err"))
                }

              case None =>
                complete(StatusCodes.NotFound -> Json.obj("status" -> "error", "details" -> "Transaction is not in blockchain"))
            }
          case _ => complete(InvalidSignature)
        }
      }
  }

  @Path("/unconfirmed")
  @ApiOperation(value = "Unconfirmed", notes = "Get list of unconfirmed transactions", httpMethod = "GET")
  def unconfirmed: Route = (path("unconfirmed") & get) {
    complete(JsArray(utxStorage.all.map(txToExtendedJson)))
  }

  private def txToExtendedJson(tx: Transaction): JsObject = {
    tx match {
      case leaseCancel: LeaseCancelTransaction =>
        leaseCancel.json ++ Json.obj("lease" -> state.findTransaction[LeaseTransaction](leaseCancel.leaseId).map(_.json).getOrElse[JsValue](JsNull))
      case tx => tx.json
    }
  }
}

object TransactionsApiRoute {
  val MaxTransactionsPerRequest = 1000
}
