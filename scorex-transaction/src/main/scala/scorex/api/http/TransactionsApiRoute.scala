package scorex.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import com.wordnik.swagger.annotations._
import play.api.libs.json.Json
import scorex.transaction.state.LagonakiState
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import spray.routing.Route


@Api(value = "/transactions", description = "Information about transactions")
case class TransactionsApiRoute(state: LagonakiState)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonApiFunctions {

  override lazy val route =
    pathPrefix("transactions") {
      unconfirmed ~ address ~ adressLimit
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

  @Path("/unconfirmed")
  @ApiOperation(value = "Unconfirmed", notes = "Get list of unconfirmed transactions", httpMethod = "GET")
  def unconfirmed: Route = {
    path("unconfirmed") {
      jsonRoute {
        Json.arr(UnconfirmedTransactionsDatabaseImpl.all().map(_.json)).toString()
      }
    }
  }

}
