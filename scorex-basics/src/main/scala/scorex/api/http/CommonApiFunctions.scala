package scorex.api.http

import akka.util.Timeout
import play.api.libs.json.{JsObject, JsValue, Json}
import scorex.block.Block
import scorex.crypto.encode.Base58
import scorex.transaction.History

import scala.concurrent.duration._


trait CommonApiFunctions {

  def json(t: Throwable): JsObject = Json.obj("error" -> Unknown.id, "message" -> t.getMessage)

  protected[api] def withBlock(history: History, encodedSignature: String)
                              (action: Block => JsValue): JsValue =
    Base58.decode(encodedSignature).toOption.map { signature =>
      history.blockById(signature) match {
        case Some(block) => action(block)
        case None => BlockNotExists.json
      }
    }.getOrElse(InvalidSignature.json)
}
