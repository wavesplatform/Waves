package scorex.consensus.qora.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import com.wordnik.swagger.annotations._
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions, InvalidNotNumber}
import scorex.app.Application
import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.consensus.qora.QoraLikeConsensusModule
import scorex.transaction.BlockStorage
import spray.routing.Route

import scala.util.Try

@Api(value = "/consensus", description = "Consensus-related calls")
case class QoraConsensusApiRoute(override val application: Application)
                                (implicit val context: ActorRefFactory)
  extends ApiRoute with CommonApiFunctions {

  private val consensusModule = application.consensusModule.asInstanceOf[QoraLikeConsensusModule]
  private val blockStorage = application.blockStorage

  override val route: Route =
    pathPrefix("consensus") {
      algo ~ time ~ timeForBalance ~ nextGenerating ~ generating
    }

  @Path("/generatingbalance/{blockId}")
  @ApiOperation(value = "Generating balance", notes = "Generating balance of a block with given id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "blockId", value = "Block id", required = true, dataType = "String", paramType = "path")
  ))
  def generating: Route = {
    path("generatingbalance" / Segment) { case encodedSignature =>
      jsonRoute {
        withBlock(blockStorage.history, encodedSignature) { block =>
          Json.obj(
            "generatingbalance" -> consensusModule.consensusBlockData(block).generatingBalance
          )
        }.toString()
      }
    }
  }

  @Path("/generatingbalance")
  @ApiOperation(value = "Next generating balance", notes = "Generating balance of a next block", httpMethod = "GET")
  def nextGenerating: Route = {
    path("generatingbalance") {
      jsonRoute {
        val generatingBalance = consensusModule.getNextBlockGeneratingBalance(blockStorage.history)
        Json.obj("generatingbalance" -> generatingBalance).toString()
      }
    }
  }

  @Path("/time/{balance}")
  @ApiOperation(value = "Balance time", notes = "estimated time before next block with given generating balance", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "balance", value = "Generating balance", required = true, dataType = "Long", paramType = "path")
  ))
  def timeForBalance: Route = {
    path("time" / Segment) { case generatingBalance =>
      jsonRoute {
        val jsRes = Try {
          val timePerBlock = consensusModule.getBlockTime(generatingBalance.toLong)
          Json.obj("time" -> timePerBlock)
        }.getOrElse(InvalidNotNumber.json)
        jsRes.toString()
      }
    }
  }

  @Path("/time")
  @ApiOperation(value = "Time", notes = "Estimated time before next block", httpMethod = "GET")
  def time: Route = {
    path("time") {
      jsonRoute {
        val block = blockStorage.history.lastBlock
        val genBalance = consensusModule.consensusBlockData(block).generatingBalance
        val timePerBlock = consensusModule.getBlockTime(genBalance)
        Json.obj("time" -> timePerBlock).toString()
      }
    }
  }

  @Path("/algo")
  @ApiOperation(value = "Consensus algo", notes = "Shows which consensus algo being using", httpMethod = "GET")
  def algo: Route = {
    path("algo") {
      jsonRoute {
        Json.obj("consensusAlgo" -> "qora").toString()
      }
    }
  }
}
