package api

import controller.Controller
import play.api.libs.json.JsArray
import scorex.block.GenesisBlock
import scorex.crypto.Base58
import spray.routing.HttpService

import scala.util.Try


trait BlocksHttpService extends HttpService with CommonApifunctions {

  lazy val route =
    path("/") {
      get {
        complete(walletNotExists().getOrElse(JsArray(Controller.getLastBlocks().map(_._2.toJson()))).toString())
      }
    } ~ path("/" / Segment) { case signature =>
      get {
        val jsRes = Try {
          Base58.decode(signature)
        }.toOption.flatMap { signatureBytes =>
          Controller.getBlock(signatureBytes) match {
            case Some(block) => Some(block.toJson())
            case None => Some(ApiError.toJson(ApiError.ERROR_BLOCK_NO_EXISTS))
          }
        }.getOrElse(ApiError.toJson(ApiError.ERROR_INVALID_SIGNATURE))
        complete(jsRes.toString())
      } ~ path("first") {
        get{
          complete(GenesisBlock.toJson().toString())
        }
      } ~ path("last") {
        get{
          complete(Controller.getLastBlock().toJson().toString())
        }
      }
    }

}
