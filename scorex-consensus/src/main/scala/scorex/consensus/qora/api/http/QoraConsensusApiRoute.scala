package scorex.consensus.qora.api.http

import akka.actor.ActorRefFactory
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions, InvalidNotNumber}
import scorex.consensus.qora.QoraLikeConsensusModule
import scorex.transaction.{BlockChain, History}
import spray.routing.HttpService._
import spray.routing.Route

import scala.util.Try


case class QoraConsensusApiRoute(consensusModule: QoraLikeConsensusModule, blockchain: BlockChain)
                                (implicit val context: ActorRefFactory)
  extends ApiRoute with CommonApiFunctions {

  private implicit val history: History = blockchain

  override val route: Route =
    pathPrefix("consensus") {
      path("algo") {
        get(complete(Json.obj("consensus-algo" -> "qora").toString()))
      } ~ path("time") {
        get {
          complete {
            val block = blockchain.lastBlock
            val genBalance = consensusModule.consensusBlockData(block).generatingBalance
            val timePerBlock = consensusModule.getBlockTime(genBalance)
            Json.obj("time" -> timePerBlock).toString()
          }
        }
      } ~ path("time" / Segment) { case generatingBalance =>
        get {
          complete {
            val jsRes = Try {
              val timePerBlock = consensusModule.getBlockTime(generatingBalance.toLong)
              Json.obj("time" -> timePerBlock)
            }.getOrElse(InvalidNotNumber.json)
            jsRes.toString()
          }
        }
      } ~ path("generatingbalance") {
        get {
          complete {
            val generatingBalance = consensusModule.getNextBlockGeneratingBalance(blockchain)
            Json.obj("generatingbalance" -> generatingBalance).toString()
          }
        }
      } ~ path("generatingbalance" / Segment) { case encodedSignature =>
        get {
          complete(withBlock(encodedSignature) { block =>
            Json.obj(
              "generatingbalance" -> consensusModule.consensusBlockData(block).generatingBalance
            )
          }.toString())
        }
      }
    }
}
