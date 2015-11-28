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
      algo ~ target
    }

  @Path("/target")
  @ApiOperation(value = "Last target", notes = "Target of a last block", httpMethod = "GET")
  def target = {
    path("target") {
      jsonRoute {
        val lastBlock = blockchain.lastBlock
        val bt = consensusModule.consensusBlockData(lastBlock).target
        Json.obj("target" -> bt.toString).toString
      }
    }
  }

  @Path("/target/{blockId}")
  @ApiOperation(value = "Target of selected block", notes = "Target of a block with specified id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "blockId", value = "Block id ", required = true, dataType = "String", paramType = "path")
  ))
  def baseTargetId = {
    path("target" / Segment) { case encodedSignature =>
      jsonRoute {
        withBlock(blockchain, encodedSignature) { block =>
          Json.obj(
            "target" -> consensusModule.consensusBlockData(block).target.toString
          )
        }.toString
      }
    }
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
