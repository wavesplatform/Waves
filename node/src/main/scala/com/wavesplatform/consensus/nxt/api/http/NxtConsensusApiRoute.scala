package com.wavesplatform.consensus.nxt.api.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http.ApiError.BlockDoesNotExist
import com.wavesplatform.api.http._
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json.{JsObject, Json}

@Path("/consensus")
@Api(value = "/consensus")
case class NxtConsensusApiRoute(settings: RestAPISettings, blockchain: Blockchain) extends ApiRoute {

  override val route: Route =
    pathPrefix("consensus") {
      algo ~ basetarget ~ baseTargetId ~ generationSignature ~ generationSignatureId ~ generatingBalance
    }

  def generatingBalance: Route = (path("generatingbalance" / AddrSegment) & get) { address =>
    complete(Json.obj("address" -> address.stringRepr, "balance" -> blockchain.generatingBalance(address)))
  }

  private def headerForId(blockId: ByteStr, f: BlockHeader => JsObject) =
    complete {
      (for {
        height <- blockchain.heightOf(blockId)
        meta   <- blockchain.blockHeader(height)
      } yield f(meta.header)).toRight[ApiError](BlockDoesNotExist)
    }

  def generationSignatureId: Route = (path("generationsignature" / Signature) & get) { signature =>
    headerForId(signature, m => Json.obj("generationSignature" -> m.generationSignature.toString))
  }

  def baseTargetId: Route = (path("basetarget" / Signature) & get) { signature =>
    headerForId(signature, m => Json.obj("baseTarget" -> m.baseTarget))
  }

  def generationSignature: Route = (path("generationsignature") & get) {
    complete(
      blockchain.lastBlockHeader
        .map(m => Json.obj("generationSignature" -> m.header.generationSignature.toString))
        .toRight(BlockDoesNotExist)
    )
  }

  def basetarget: Route = (path("basetarget") & get) {
    complete(
      blockchain.lastBlockHeader
        .map(m => Json.obj("baseTarget" -> m.header.baseTarget))
        .toRight(BlockDoesNotExist)
    )

  }

  @Path("/algo")
  @ApiOperation(value = "Consensus algo", notes = "Shows which consensus algo being using", httpMethod = "GET")
  def algo: Route = (path("algo") & get) {
    complete(
      if (blockchain.activatedFeatures.contains(BlockchainFeatures.FairPoS.id))
        Json.obj("consensusAlgo" -> "Fair Proof-of-Stake (FairPoS)")
      else
        Json.obj("consensusAlgo" -> "proof-of-stake (PoS)")
    )
  }
}
