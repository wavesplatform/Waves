package com.wavesplatform.api.http

import com.wavesplatform.api.common.CommonGeneratorsApi.GeneratorEntry
import com.wavesplatform.api.common.{CommonBlocksApi, CommonGeneratorsApi}
import com.wavesplatform.state.{Blockchain, GenerationPeriod, Height}
import org.apache.pekko.http.scaladsl.server.Route
import play.api.libs.json.*

case class FinalityApiRoute(blockchain: Blockchain, blocksApi: CommonBlocksApi, generatorsApi: CommonGeneratorsApi) extends ApiRoute {
  import FinalityApiRoute.given

  override def route: Route = pathPrefix("blockchain" / "finality") {
    (get & pathEndOrSingleSlash) {
      complete(finalityInfo)
    }
  }

  private def finalityInfo: JsObject = {
    val currentHeight = Height(blockchain.height)
    val currentPeriod = blockchain.generationPeriodOf(currentHeight)
    Json.obj(
      "height"                  -> currentHeight,
      "finalizedHeight"         -> blocksApi.currentFinalizedHeight,
      "currentGenerationPeriod" -> currentPeriod,
      "currentGenerators"       -> generatorsApi.generators(currentHeight),
      "nextGenerationPeriod"    -> currentPeriod.map(_.next),
      "nextGenerators" -> currentPeriod.fold(Seq.empty)(p =>
        generatorsApi
          .generators(p.next.start)
          .map(ge =>
            Json.obj(
              "address"       -> ge.address,
              "transactionId" -> ge.commitTxnId
            )
          )
      )
    )
  }
}

object FinalityApiRoute {
  given Writes[GenerationPeriod] = (gp: GenerationPeriod) =>
    Json.obj(
      "start" -> gp.start,
      "end"   -> gp.end
    )

  given Writes[GeneratorEntry] = (ge: GeneratorEntry) =>
    Json.obj(
      "address"        -> ge.address,
      "transactionId"  -> ge.commitTxnId,
      "balance"        -> ge.balance,
      "conflictHeight" -> ge.conflictHeight
    )
}
