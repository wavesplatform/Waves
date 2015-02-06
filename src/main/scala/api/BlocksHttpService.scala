package api

import controller.Controller
import play.api.libs.json.{JsObject, Json, JsArray}
import scorex.BlockGenerator
import scorex.block.{Block, GenesisBlock}
import scorex.crypto.Base58
import spray.routing.HttpService

import scala.util.Try


trait BlocksHttpService extends HttpService with CommonApifunctions {

  def withBlock(encodedSignature: String)(action: Block => JsObject): JsObject =
    Try {
      Base58.decode(encodedSignature)
    }.toOption.map { signature =>
      Controller.block(signature) match {
        case Some(block) => action(block)
        case None => ApiError.toJson(ApiError.ERROR_BLOCK_NO_EXISTS)
      }
    }.getOrElse(ApiError.toJson(ApiError.ERROR_INVALID_SIGNATURE))


  lazy val route =
    path("/") {
      get {
        complete(walletNotExists().getOrElse(JsArray(Controller.lastBlocks().map(_._2.toJson()))).toString())
      }
    } ~ path("/" / Segment) { case encodedSignature =>
      get {
        complete(withBlock(encodedSignature)(_.toJson()).toString())
      }
    } ~ path("first") {
      get {
        complete(GenesisBlock.toJson().toString())
      }
    } ~ path("last") {
      get {
        complete(Controller.lastBlock().toJson().toString())
      }
    } ~ path("height") {
      get {
        complete(Json.obj("height" -> Controller.height()).toString())
      }
    } ~ path("height" / Segment) {case encodedSignature =>
      get {
        val jsRes = withBlock(encodedSignature) {block =>
          Json.obj("height" -> block.getHeight())
        }
        complete(jsRes.toString())
      }
    } ~ path("time") {
      get {
        val block = Controller.lastBlock()
        val timePerBlock = BlockGenerator.getBlockTime(block.generatingBalance)
        complete(Json.obj("time" -> timePerBlock).toString())
      }
    }  ~ path("time" / Segment) { case generatingBalance =>
      get {
        val jsRes = Try {
          val timePerBlock = BlockGenerator.getBlockTime(generatingBalance.toLong)
          Json.obj("time" -> timePerBlock)
        }.getOrElse(ApiError.toJson(ApiError.ERROR_INVALID_NOT_NUMBER))
        complete(jsRes.toString())
      }
    } ~ path("generatingbalance") {
      get {
        val generatingBalance = Controller.nextBlockGeneratingBalance()
        complete(Json.obj("generatingbalance" -> generatingBalance).toString())
      }
    } ~ path("generatingbalance" / Segment) { case encodedSignature =>
      get {
        val jsRes = withBlock(encodedSignature) { block =>
          Json.obj("generatingbalance" -> block.generatingBalance)
        }
        complete(jsRes.toString())
      }
    } ~ path("child" / Segment) { case encodedSignature =>
        get {
          complete(withBlock(encodedSignature)(_.getChild().toJson()).toString())
        }
    } ~ path("address" / Segment) { case address =>
      get {
        complete("")
      }
    }
}
