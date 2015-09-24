package scorex.app.api.http

import java.security.SecureRandom

import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions}
import scorex.crypto.Base58
import spray.routing.HttpService._

//todo: move to basics?
case object SeedHttpService extends ApiRoute with CommonApiFunctions {

  private def seed(length: Int): String = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    Json.obj("seed" -> Base58.encode(seed)).toString()
  }

  override lazy val route =
    path("seed"./) {
      get(complete(seed(32)))
    } ~ path("seed" / IntNumber) { case length =>
      get(complete(seed(length)))
    }
}
