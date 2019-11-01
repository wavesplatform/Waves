package com.wavesplatform.consensus.nxt.api.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.account.Address
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.http.ApiError.{BlockDoesNotExist, InvalidSignature}
import com.wavesplatform.api.http.{ApiError, ApiRoute}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json.{JsObject, Json}

@Path("/consensus")
@Api(value = "/consensus")
case class NxtConsensusApiRoute(settings: RestAPISettings, blockchain: Blockchain, commonApi: CommonBlocksApi) extends ApiRoute {

  override val route: Route =
    pathPrefix("consensus") {
      algo ~ basetarget ~ baseTargetId ~ generationSignature ~ generationSignatureId ~ generatingBalance
    }

  def generatingBalance: Route = (path("generatingbalance" / Segment) & get) { address =>
    Address.fromString(address) match {
      case Left(_) => complete(ApiError.InvalidAddress)
      case Right(account) =>
        complete(Json.obj("address" -> account.stringRepr, "balance" -> blockchain.generatingBalance(account)))
    }
  }

  private def headerForId(id: String, f: BlockMeta => JsObject) =
    complete {
      for {
        blockId <- ByteStr.decodeBase58(id).toEither.left.map(_ => InvalidSignature)
        meta    <- commonApi.meta(blockId).toRight[ApiError](BlockDoesNotExist)
      } yield f(meta)
    }

  def generationSignatureId: Route = (path("generationsignature" / Segment) & get) { encodedSignature =>
    headerForId(encodedSignature, m => Json.obj("generationSignature" -> m.header.generationSignature.toString))
  }

  def baseTargetId: Route = (path("basetarget" / Segment) & get) { encodedSignature =>
    headerForId(encodedSignature, m => Json.obj("baseTarget" -> m.header.baseTarget))
  }

  def generationSignature: Route = (path("generationsignature") & get) {
    complete(
      commonApi
        .metaAtHeight()
        .map(m => Json.obj("generationSignature" -> m.header.generationSignature.toString))
        .toRight(BlockDoesNotExist)
    )
  }

  def basetarget: Route = (path("basetarget") & get) {
    complete(
      commonApi
        .metaAtHeight()
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
