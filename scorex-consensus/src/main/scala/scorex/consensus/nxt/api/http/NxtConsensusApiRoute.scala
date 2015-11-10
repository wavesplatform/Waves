package scorex.consensus.nxt.api.http

import akka.actor.ActorRefFactory
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions}
import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.crypto.Base58
import scorex.transaction.{BlockChain, History}
import spray.routing.HttpService._
import spray.routing.Route


class NxtConsensusApiRoute(consensusModule: NxtLikeConsensusModule, blockchain: BlockChain)
                          (implicit val context: ActorRefFactory)
  extends ApiRoute with CommonApiFunctions {

  private implicit val history: History = blockchain

  override val route: Route =
    pathPrefix("consensus") {
      path("algo") {
        get(complete(Json.obj("consensus-algo" -> "qora").toString()))
      } ~ path("basetarget") {
        get {
          complete {
            val lastBlock = blockchain.lastBlock
            val bt = consensusModule.consensusBlockData(lastBlock).baseTarget
            Json.obj("base-target" -> bt).toString()
          }
        }
      } ~ path("basetarget" / Segment) { case encodedSignature =>
        get {
          complete(withBlock(encodedSignature) { block =>
            Json.obj(
              "base-target" -> consensusModule.consensusBlockData(block).baseTarget
            )
          }.toString())
        }
      } ~ path("generationsignature") {
        get {
          complete {
            val lastBlock = blockchain.lastBlock
            val gs = consensusModule.consensusBlockData(lastBlock).generationSignature
            Json.obj("generation-signature" -> Base58.encode(gs)).toString()
          }
        }
      } ~ path("generationsignature" / Segment) { case encodedSignature =>
        get {
          complete(withBlock(encodedSignature) { block =>
            val gs = consensusModule.consensusBlockData(block).generationSignature
            Json.obj(
              "generation-signature" -> Base58.encode(gs)
            )
          }.toString())
        }
      }
    }
}
