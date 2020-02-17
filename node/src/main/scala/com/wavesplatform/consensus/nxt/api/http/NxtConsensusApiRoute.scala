package com.wavesplatform.consensus.nxt.api.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.{ApiError, ApiRoute, CommonApiFunctions}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import play.api.libs.json.Json

case class NxtConsensusApiRoute(settings: RestAPISettings, blockchain: Blockchain) extends ApiRoute with CommonApiFunctions {

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

  def generationSignatureId: Route = (path("generationsignature" / Segment) & get) { encodedSignature =>
    withBlock(blockchain, encodedSignature) { block =>
      complete(Json.obj("generationSignature" -> block.header.generationSignature.toString))
    }
  }

  def generationSignature: Route = (path("generationsignature") & get) {
    complete(Json.obj("generationSignature" -> blockchain.lastBlock.get.header.generationSignature.toString))
  }

  def baseTargetId: Route = (path("basetarget" / Segment) & get) { encodedSignature =>
    withBlock(blockchain, encodedSignature) { block =>
      complete(Json.obj("baseTarget" -> block.header.baseTarget))
    }
  }

  def basetarget: Route = (path("basetarget") & get) {
    complete(
      Json.obj(
        "baseTarget" -> blockchain.lastBlock.get.header.baseTarget,
        "score"      -> blockchain.score.toString()
      )
    )
  }

  def algo: Route = (path("algo") & get) {
    complete(
      if (blockchain.activatedFeatures.contains(BlockchainFeatures.FairPoS.id))
        Json.obj("consensusAlgo" -> "Fair Proof-of-Stake (FairPoS)")
      else
        Json.obj("consensusAlgo" -> "proof-of-stake (PoS)")
    )
  }
}
