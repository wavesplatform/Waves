package scorex.api.http

import play.api.libs.json.{JsValue, Json}
import akka.util.Timeout
import scorex.block.Block
import scorex.crypto.Base58
import scorex.transaction.History
import scala.concurrent.duration._


trait CommonApiFunctions {
  implicit val timeout = Timeout(5.seconds)

  def json(t: Throwable) = Json.obj("error" -> Unknown.id, "message" -> t.getMessage)

  protected[api] def withBlock(history: History, encodedSignature: String)
                              (action: Block => JsValue): JsValue =
    Base58.decode(encodedSignature).toOption.map { signature =>
      history.blockById(signature) match {
        case Some(block) => action(block)
        case None => BlockNotExists.json
      }
    }.getOrElse(InvalidSignature.json)
}
