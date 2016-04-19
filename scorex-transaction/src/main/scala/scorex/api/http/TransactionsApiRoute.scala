package scorex.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import com.wordnik.swagger.annotations._
import play.api.libs.json.{JsArray, Json}
import scorex.app.Application
import scorex.crypto.encode.Base58
import scorex.transaction.LagonakiState
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import scorex.transaction.state.database.blockchain.StoredBlockchain
import spray.routing.Route

import scala.util.{Success, Try}


@Api(value = "/transactions", description = "Information about transactions")
case class TransactionsApiRoute(override val application: Application)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonApiFunctions {

  private val state: LagonakiState = application.blockStorage.state

  override lazy val route =
    pathPrefix("transactions") {
      unconfirmed ~ address ~ adressLimit ~ info
    }

  //TODO implement general pagination
  @Path("/address/{address}/limit/{limit}")
  @ApiOperation(value = "Address", notes = "Get list of transactions where specified address has been involved", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Wallet address ", required = true, dataType = "String", paramType = "path"),
    new ApiImplicitParam(name = "limit", value = "Specified number of records to be returned", required = true, dataType = "Long", paramType = "path")
  ))
  def adressLimit: Route = {
    path("address" / Segment / "limit" / IntNumber) { case (address, limit) =>
      jsonRoute {

        val txJsons = state.accountTransactions(address)
          .takeRight(limit)
          .map(_.json)
        Json.arr(txJsons).toString()
      }
    }
  }

  @Path("/address/{address}")
  @ApiOperation(value = "Address", notes = "Get list of transactions where specified address has been involved", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Wallet address ", required = true, dataType = "String", paramType = "path")
  ))
  def address: Route = {
    path("address" / Segment) { case address =>
      jsonRoute {
        val txJsons = state.accountTransactions(address).map(_.json)
        Json.arr(txJsons).toString()
      }
    }
  }

  @Path("/info/{signature}")
  @ApiOperation(value = "Info", notes = "Get transaction info", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "signature", value = "transaction signature ", required = true, dataType = "String", paramType = "path")
  ))
  def info: Route = {
    path("info" / Segment) { case encoded =>
      jsonRoute {
        Base58.decode(encoded) match {
          case Success(sig) =>
            state.included(sig, None) match {
              case Some(h) =>
                Try {
                  val block = application.blockStorage.history.asInstanceOf[StoredBlockchain].blockAt(h).get
                  val tx = block.transactions.filter(_.signature sameElements sig).head
                  tx.json
                }.getOrElse(Json.obj("status" -> "error", "details" -> "Internal error")).toString
              case None => Json.obj("status" -> "error", "details" -> "Transaction is not in blockchain").toString()
            }
          case _ => Json.obj("status" -> "error", "details" -> "Incorrect signature").toString()
        }
      }
    }
  }

  @Path("/unconfirmed")
  @ApiOperation(value = "Unconfirmed", notes = "Get list of unconfirmed transactions", httpMethod = "GET")
  def unconfirmed: Route = {
    path("unconfirmed") {
      jsonRoute {
        JsArray(UnconfirmedTransactionsDatabaseImpl.all().map(_.json)).toString()
      }
    }
  }

}
