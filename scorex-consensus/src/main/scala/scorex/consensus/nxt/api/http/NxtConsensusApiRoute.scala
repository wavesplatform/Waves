package scorex.consensus.nxt.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import com.wordnik.swagger.annotations._
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions}
import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.crypto.Base58
import scorex.transaction.{BlockChain, History}
import spray.routing.Route


@Api(value = "/consensus", description = "Consensus-related calls")
class NxtConsensusApiRoute(consensusModule: NxtLikeConsensusModule, blockchain: BlockChain)
                          (implicit val context: ActorRefFactory)
  extends ApiRoute with CommonApiFunctions {

  override val route: Route =
    pathPrefix("consensus") {
      algo ~ basetarget ~ baseTargetId ~ generationSignature ~ generationSignatureId
    }

  @Path("/generationsignature/{blockId}")
  @ApiOperation(value = "Generation signature", notes = "Generation signature of a block with specified id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "blockId", value = "Block id ", required = true, dataType = "String", paramType = "path")
  ))
  def generationSignatureId = {
    path("generationsignature" / Segment) { case encodedSignature =>
      jsonRoute {
        withBlock(blockchain, encodedSignature) { block =>
          val gs = consensusModule.consensusBlockData(block).generationSignature
          Json.obj(
            "generation-signature" -> Base58.encode(gs)
          )
        }.toString()
      }
    }
  }

  @Path("/generationsignature")
  @ApiOperation(value = "Generation signature last", notes = "Generation signature of a last block", httpMethod = "GET")
  def generationSignature = {
    path("generationsignature") {
      jsonRoute {
        val lastBlock = blockchain.lastBlock
        val gs = consensusModule.consensusBlockData(lastBlock).generationSignature
        Json.obj("generation-signature" -> Base58.encode(gs)).toString()
      }
    }
  }

  @Path("/basetarget/{blockId}")
  @ApiOperation(value = "Base target", notes = "base target of a block with specified id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "blockId", value = "Block id ", required = true, dataType = "String", paramType = "path")
  ))
  def baseTargetId = {
    path("basetarget" / Segment) { case encodedSignature =>
      jsonRoute {
        withBlock(blockchain, encodedSignature) { block =>
          Json.obj(
            "base-target" -> consensusModule.consensusBlockData(block).baseTarget
          )
        }.toString
      }
    }
  }

  @Path("/basetarget")
  @ApiOperation(value = "Base target last", notes = "Base target of a last block", httpMethod = "GET")
  def basetarget = {
    path("basetarget") {
      jsonRoute {
        val lastBlock = blockchain.lastBlock
        val bt = consensusModule.consensusBlockData(lastBlock).baseTarget
        Json.obj("base-target" -> bt).toString()
      }
    }

  }

  @Path("/algo")
  @ApiOperation(value = "Consensus algo", notes = "Shows which consensus algo being using", httpMethod = "GET")
  def algo = {
    path("algo") {
      jsonRoute {
        Json.obj("consensus-algo" -> "nxt").toString()
      }
    }
  }
}
