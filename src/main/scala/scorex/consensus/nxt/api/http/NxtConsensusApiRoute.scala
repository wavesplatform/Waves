package scorex.consensus.nxt.api.http

import javax.ws.rs.Path

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json.Json
import scorex.account.Account
import scorex.api.http.{ApiRoute, CommonApiFunctions, InvalidAddress}
import scorex.app.RunnableApplication
import scorex.consensus.nxt.WavesConsensusModule
import scorex.crypto.encode.Base58


@Path("/consensus")
@Api(value = "/consensus")
class NxtConsensusApiRoute(application: RunnableApplication) extends ApiRoute with CommonApiFunctions {

  val settings = application.settings.restAPISettings
  private val consensusModule = application.consensusModule.asInstanceOf[WavesConsensusModule]
  private val blockStorage = application.blockStorage

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
    val account = new Account(address)
    if (!Account.isValid(account)) {
      complete(InvalidAddress)
    } else {
      complete(Json.obj(
        "address" -> account.address,
        "balance" -> consensusModule.generatingBalance(account)(application.transactionModule)))
    }
  }

  @Path("/generationsignature/{blockId}")
  @ApiOperation(value = "Generation signature", notes = "Generation signature of a block with specified id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "blockId", value = "Block id ", required = true, dataType = "string", paramType = "path")
  ))
  def generationSignatureId: Route = (path("generationsignature" / Segment) & get) { encodedSignature =>
    withBlock(blockStorage.history, encodedSignature) { block =>
      val gs = block.consensusDataField.value.generationSignature
      complete(Json.obj("generationSignature" -> Base58.encode(gs)))
    }
  }

  @Path("/generationsignature")
  @ApiOperation(value = "Generation signature last", notes = "Generation signature of a last block", httpMethod = "GET")
  def generationSignature: Route = (path("generationsignature") & get) {
    val lastBlock = blockStorage.history.lastBlock
    val gs = lastBlock.consensusDataField.value.generationSignature
    complete(Json.obj("generationSignature" -> Base58.encode(gs)))
  }

  @Path("/basetarget/{blockId}")
  @ApiOperation(value = "Base target", notes = "base target of a block with specified id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "blockId", value = "Block id ", required = true, dataType = "string", paramType = "path")
  ))
  def baseTargetId: Route = (path("basetarget" / Segment) & get) { encodedSignature =>
    withBlock(blockStorage.history, encodedSignature) { block =>
      complete(Json.obj(
        "baseTarget" -> block.consensusDataField.value.baseTarget
      ))
    }
  }

  @Path("/basetarget")
  @ApiOperation(value = "Base target last", notes = "Base target of a last block", httpMethod = "GET")
  def basetarget: Route = (path("basetarget") & get) {
    val lastBlock = blockStorage.history.lastBlock
    val bt = lastBlock.consensusDataField.value.baseTarget
    complete(Json.obj("baseTarget" -> bt))
  }

  @Path("/algo")
  @ApiOperation(value = "Consensus algo", notes = "Shows which consensus algo being using", httpMethod = "GET")
  def algo: Route = (path("algo") & get) {
    complete((Json.obj("consensusAlgo" -> "proof-of-stake (PoS)")))
  }
}
