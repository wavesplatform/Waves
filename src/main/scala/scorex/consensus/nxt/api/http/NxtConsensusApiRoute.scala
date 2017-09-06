package scorex.consensus.nxt.api.http

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.{FunctionalitySettings, RestAPISettings}
import com.wavesplatform.state2.reader.SnapshotStateReader
import io.swagger.annotations._
import play.api.libs.json.Json
import scorex.account.Address
import scorex.api.http.{ApiRoute, CommonApiFunctions, InvalidAddress}
import scorex.transaction.{History, PoSCalc}

@Path("/consensus")
@Api(value = "/consensus")
case class NxtConsensusApiRoute(
    settings: RestAPISettings,
    state: SnapshotStateReader,
    history: History,
    fs: FunctionalitySettings) extends ApiRoute with CommonApiFunctions {

  override val route: Route =
    pathPrefix("consensus") {
      algo ~ basetarget ~ baseTargetId ~ generationSignature ~ generationSignatureId ~ generatingBalance
    }

  @Path("/generatingbalance/{address}")
  @ApiOperation(value = "Generating balance", notes = "Account's generating balance(the same as balance atm)", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def generatingBalance: Route = (path("generatingbalance" / Segment) & get) { address =>
    Address.fromString(address) match {
      case Left(_) => complete(InvalidAddress)
      case Right(account) =>
        val s = state
        complete(Json.obj(
          "address" -> account.address,
          "balance" -> PoSCalc.generatingBalance(s, fs, account, s.height)))
    }
  }

  @Path("/generationsignature/{blockId}")
  @ApiOperation(value = "Generation signature", notes = "Generation signature of a block with specified id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "blockId", value = "Block id ", required = true, dataType = "string", paramType = "path")
  ))
  def generationSignatureId: Route = (path("generationsignature" / Segment) & get) { encodedSignature =>
    withBlock(history, encodedSignature) { block =>
      complete(Json.obj("generationSignature" -> block.consensusData.generationSignature.base58))
    }
  }

  @Path("/generationsignature")
  @ApiOperation(value = "Generation signature last", notes = "Generation signature of a last block", httpMethod = "GET")
  def generationSignature: Route = (path("generationsignature") & get) {
    complete(Json.obj("generationSignature" -> history.lastBlock.get.consensusData.generationSignature.base58))
  }

  @Path("/basetarget/{blockId}")
  @ApiOperation(value = "Base target", notes = "base target of a block with specified id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "blockId", value = "Block id ", required = true, dataType = "string", paramType = "path")
  ))
  def baseTargetId: Route = (path("basetarget" / Segment) & get) { encodedSignature =>
    withBlock(history, encodedSignature) { block =>
      complete(Json.obj("baseTarget" -> block.consensusData.baseTarget))
    }
  }

  @Path("/basetarget")
  @ApiOperation(value = "Base target last", notes = "Base target of a last block", httpMethod = "GET")
  def basetarget: Route = (path("basetarget") & get) {
    complete(Json.obj(
      "baseTarget" -> history.lastBlock.get.consensusData.baseTarget,
      "score" -> history.score.toString()
    )
    )
  }

  @Path("/algo")
  @ApiOperation(value = "Consensus algo", notes = "Shows which consensus algo being using", httpMethod = "GET")
  def algo: Route = (path("algo") & get) {
    complete(Json.obj("consensusAlgo" -> "proof-of-stake (PoS)"))
  }
}
