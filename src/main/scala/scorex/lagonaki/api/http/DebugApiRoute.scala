package scorex.lagonaki.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import com.wordnik.swagger.annotations._
import scorex.api.http._
import scorex.lagonaki.server.LagonakiApplication
import spray.routing.Route

@Api(value = "/debug", description = "Debug methods", position = 1)
case class DebugApiRoute(application: LagonakiApplication)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  implicit lazy val transactionModule = application.transactionModule
  lazy val wallet = application.wallet

  override lazy val route = pathPrefix("debug") {
    state
  }

  @Path("/state")
  @ApiOperation(value = "State", notes = "get current state", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json state")
  ))
  def state: Route = {
    path("state") {
      jsonRoute {
        application.blockStorage.state.toString
      }
    }
  }
}
