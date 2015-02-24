package api

import java.security.SecureRandom

import play.api.libs.json.Json
import scorex.crypto.Base58
import spray.routing.HttpService

trait SeedHttpService extends HttpService with CommonApifunctions {
  lazy val random = new SecureRandom()
  lazy val seedRouting =
    path("seed"./) {
      get(complete(seed(32)))
    } ~ path("seed" / IntNumber) { case length =>
      get(complete(seed(length)))
    }

  private def seed(length: Int): String = {
    val seed = new Array[Byte](length)
    random.nextBytes(seed)
    Json.obj("seed" -> Base58.encode(seed)).toString()
  }
}
