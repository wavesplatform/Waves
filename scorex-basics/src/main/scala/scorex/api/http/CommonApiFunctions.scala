package scorex.api.http

import play.api.libs.json.Json
import akka.util.Timeout
import scala.concurrent.duration._


trait CommonApiFunctions {
  implicit val timeout = Timeout(5.seconds)

  def json(t: Throwable) = Json.obj("error" -> Unknown.id, "message" -> t.getMessage)
}
