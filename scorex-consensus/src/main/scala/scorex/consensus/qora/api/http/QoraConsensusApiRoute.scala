package scorex.consensus.qora.api.http

import play.api.libs.json.Json
import scorex.api.http.{CommonApiFunctions, InvalidNotNumber, ApiRoute}
import scorex.consensus.qora.QoraLikeConsensusModule
import scorex.transaction.{History, BlockChain}
import spray.routing.HttpService._
import spray.routing.Route

import scala.util.Try


case class QoraConsensusApiRoute(consensusModule:QoraLikeConsensusModule,
                                 blockchain:BlockChain) extends ApiRoute with CommonApiFunctions {

  private implicit val history:History = blockchain

  override val route: Route =
    pathPrefix("consensus") {
      path("time") {
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
