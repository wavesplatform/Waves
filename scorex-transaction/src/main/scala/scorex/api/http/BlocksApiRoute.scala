package scorex.api.http

import play.api.libs.json.Json
import scorex.transaction.BlockChain
import scorex.transaction.state.wallet.Wallet
import spray.routing.HttpService._


case class BlocksApiRoute(implicit blockchain: BlockChain, wallet: Wallet)
  extends ApiRoute with CommonTransactionApiFunctions {

  override lazy val route =
    pathPrefix("blocks") {
      path("signature" / Segment) { case encodedSignature =>
        get {
          complete(withBlock(encodedSignature)(_.json).toString())
        }
      } ~ path("first") {
        get {
          complete(blockchain.blockAt(1).get.json.toString())
        }
      } ~ path("last") {
        get {
          complete(blockchain.lastBlock.json.toString())
        }
      } ~ path("at" / IntNumber) { case height =>
        get {
          val res = blockchain
            .blockAt(height)
            .map(_.json.toString())
            .getOrElse(Json.obj("status" -> "error", "details" -> "No block for this height").toString())
          complete(res)
        }
      } ~ path("height") {
        get {
          complete(Json.obj("height" -> blockchain.height()).toString())
        }
      } ~ path("height" / Segment) { case encodedSignature =>
        get {
          complete {
            withBlock(encodedSignature) { block =>
              Json.obj("height" -> blockchain.heightOf(block))
            }.toString()
          }
        }
      } ~ path("child" / Segment) { case encodedSignature =>
        get {
          complete(withBlock(encodedSignature) { block =>
            blockchain.children(block).head.json
          }.toString())
        }
      } ~ path("address" / Segment) { case address =>
        get {
          complete(withPrivateKeyAccount(address) { account =>
            Json.arr(blockchain.generatedBy(account).map(_.json))
          }.toString())
        }
      }

      /* todo: consider how to obtain consensus-specific data via API, commented out for now
      ~ path("time") {
        get {
          complete {
            val block = StoredBlockchain.lastBlock
            val timePerBlock = QoraBlockGenerationFunctions.getBlockTime(block.generationData.generatingBalance)
            Json.obj("time" -> timePerBlock).toString()
          }
        }
      } ~ path("time" / Segment) { case generatingBalance =>
        get {
          complete {
            val jsRes = Try {
              val timePerBlock = QoraBlockGenerationFunctions.getBlockTime(generatingBalance.toLong)
              Json.obj("time" -> timePerBlock)
            }.getOrElse(ApiError.toJson(ApiError.ERROR_INVALID_NOT_NUMBER))
            jsRes.toString()
          }
        }
      } ~ path("generatingbalance") {
        get {
          complete {
            val generatingBalance = LagonakiApplication.nextBlockGeneratingBalance()
            Json.obj("generatingbalance" -> generatingBalance).toString()
          }
        }
      } ~ path("generatingbalance" / Segment) { case encodedSignature =>
        get {
          complete(withBlock(encodedSignature) { block =>
            Json.obj("generatingbalance" -> block.generationData.generatingBalance)
          }.toString())
        }
      } */
    }
}
