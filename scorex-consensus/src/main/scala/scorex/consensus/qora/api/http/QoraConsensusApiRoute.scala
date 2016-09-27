package scorex.consensus.qora.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions, InvalidNotNumber, JsonResponse}
import scorex.app.RunnableApplication
import scorex.consensus.qora.QoraLikeConsensusModule

import scala.util.Try

@Path("/consensus")
@Api(value = "/consensus", description = "Consensus-related calls")
case class QoraConsensusApiRoute(application: RunnableApplication)
                                (implicit val context: ActorRefFactory)
  extends ApiRoute with CommonApiFunctions {

  val settings = application.settings
  private val consensusModule = application.consensusModule.asInstanceOf[QoraLikeConsensusModule]
  private val blockStorage = application.blockStorage

  override val route: Route =
    pathPrefix("consensus") {
      algo ~ time ~ timeForBalance ~ nextGenerating ~ generating
    }

  @Path("/generatingbalance/{blockId}")
  @ApiOperation(value = "Generating balance", notes = "Generating balance of a block with given id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "blockId", value = "Block id", required = true, dataType = "string", paramType = "path")
  ))
  def generating: Route = {
    path("generatingbalance" / Segment) { case encodedSignature =>
      getJsonRoute {
        withBlock(blockStorage.history, encodedSignature) { block =>
          Json.obj(
            "generatingbalance" -> consensusModule.consensusBlockData(block).generatingBalance
          )
        }
      }
    }
  }

  @Path("/generatingbalance")
  @ApiOperation(value = "Next generating balance", notes = "Generating balance of a next block", httpMethod = "GET")
  def nextGenerating: Route = {
    path("generatingbalance") {
      getJsonRoute {
        val generatingBalance = consensusModule.getNextBlockGeneratingBalance(blockStorage.history)
        JsonResponse(Json.obj("generatingbalance" -> generatingBalance), StatusCodes.OK)
      }
    }
  }

  @Path("/time/{balance}")
  @ApiOperation(value = "Balance time", notes = "estimated time before next block with given generating balance", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "balance", value = "Generating balance", required = true, dataType = "long", paramType = "path")
  ))
  def timeForBalance: Route = {
    path("time" / Segment) { case generatingBalance =>
      getJsonRoute {
        Try {
          val timePerBlock = consensusModule.getBlockTime(generatingBalance.toLong)
          JsonResponse(Json.obj("time" -> timePerBlock), StatusCodes.OK)
        }.getOrElse(InvalidNotNumber.response)
      }
    }
  }

  @Path("/time")
  @ApiOperation(value = "Time", notes = "Estimated time before next block", httpMethod = "GET")
  def time: Route = {
    path("time") {
      getJsonRoute {
        val block = blockStorage.history.lastBlock
        val genBalance = consensusModule.consensusBlockData(block).generatingBalance
        val timePerBlock = consensusModule.getBlockTime(genBalance)
        JsonResponse(Json.obj("time" -> timePerBlock), StatusCodes.OK)
      }
    }
  }

  @Path("/algo")
  @ApiOperation(value = "Consensus algo", notes = "Shows which consensus algo being using", httpMethod = "GET")
  def algo: Route = {
    path("algo") {
      getJsonRoute {
        JsonResponse(Json.obj("consensusAlgo" -> "qora"), StatusCodes.OK)
      }
    }
  }
}
