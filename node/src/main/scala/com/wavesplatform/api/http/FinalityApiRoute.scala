package com.wavesplatform.api.http

import com.wavesplatform.api.common.CommonGeneratorsApi
import com.wavesplatform.api.common.CommonGeneratorsApi.GeneratorEntry
import com.wavesplatform.state.{Blockchain, GenerationPeriod, Height}
import org.apache.pekko.http.scaladsl.server.Route
import play.api.libs.json.*

case class FinalityApiRoute(blockchain: Blockchain, maxRollback: Int, generatorsApi: CommonGeneratorsApi) extends ApiRoute {
  import FinalityApiRoute.given

  override def route: Route = pathPrefix("blockchain" / "finality") {
    (get & pathEndOrSingleSlash) {
      complete(finalityInfo)
    }
  }

  private def finalityInfo: JsObject = {
    val currentHeight = blockchain.height
    val currentPeriod = blockchain.generationPeriodOf(Height(currentHeight))
    Json.obj(
      "height"                  -> currentHeight,
      "finalizedHeight"         -> blockchain.finalizedHeightAtOrFallback(maxRollback, Height(currentHeight)),
      "currentGenerationPeriod" -> currentPeriod,
      "currentGenerators"       -> generatorsApi.generators(Height(currentHeight)),
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
