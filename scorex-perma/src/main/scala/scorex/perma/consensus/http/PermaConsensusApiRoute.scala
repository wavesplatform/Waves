package scorex.perma.consensus.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import com.wordnik.swagger.annotations._
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions}
import scorex.perma.consensus.PermaConsensusModule
import scorex.transaction.BlockChain
import spray.routing.Route


@Api(value = "/consensus", description = "Consensus-related calls")
class PermaConsensusApiRoute(consensusModule: PermaConsensusModule, blockchain: BlockChain)
                            (implicit val context: ActorRefFactory)
  extends ApiRoute with CommonApiFunctions {

  override val route: Route =
    pathPrefix("consensus") {
      algo
    }

  @Path("/algo")
  @ApiOperation(value = "Consensus algo", notes = "Shows which consensus algo being using", httpMethod = "GET")
  def algo = {
    path("algo") {
      jsonRoute {
        Json.obj("consensus-algo" -> "perma").toString()
      }
    }
  }
}
