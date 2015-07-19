package scorex.api.http

import scorex.controller.Controller
import play.api.libs.json.Json
import scorex.settings.Constants
import spray.routing.HttpService


trait BlocksHttpService extends HttpService with CommonApiFunctions {

  lazy val blocksRouting =
    pathPrefix("blocks") {
      path("signature" / Segment) { case encodedSignature =>
        get {
          complete(withBlock(encodedSignature)(_.json).toString())
        }
      } ~ path("first") {
        get {
          complete(Constants.ConsensusAlgo.genesisBlock.json.toString())
        }
      } ~ path("last") {
        get {
          complete(Controller.blockchainStorage.lastBlock.json.toString())
        }
      } ~ path("at" / IntNumber) {case height =>
        get {
          //todo: json instead of n/a
          val res = Controller.blockchainStorage.blockAt(height).map(_.json.toString()).getOrElse("n/a")
          complete(res)
        }
      } ~ path("height") {
        get {
          complete(Json.obj("height" -> Controller.blockchainStorage.height()).toString())
        }
      } ~ path("height" / Segment) { case encodedSignature =>
        get {
          complete {
            withBlock(encodedSignature) { block =>
              Json.obj("height" -> block.height())
            }.toString()
          }
        }
      } ~ path("child" / Segment) { case encodedSignature =>
        get {
          complete(withBlock(encodedSignature)(_.child().get.json).toString())
        }
      } ~ path("address" / Segment) { case address =>
        get {
          complete(withAccount(address) { account =>
            Json.arr(Controller.blockchainStorage.generatedBy(account).map(_.json))
          }.toString())
        }
      }

      /* todo: consider how to obtain consensus-specific data via API, commented out for now
      ~ path("time") {
        get {
          complete {
            val block = PrunableBlockchainStorage.lastBlock
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
            val generatingBalance = Controller.nextBlockGeneratingBalance()
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
