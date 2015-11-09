package scorex.api.http

import java.security.SecureRandom

import play.api.libs.json.Json
import scorex.crypto.Base58
import spray.routing.HttpService._

case object SeedApiRoute extends ApiRoute with CommonApiFunctions {
  val SeedSize = 32

  private def seed(length: Int): String = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    Json.obj("seed" -> Base58.encode(seed)).toString()
  }

  override lazy val route =
    path("seed"./) {
      get(complete(seed(SeedSize)))
    } ~ path("seed" / IntNumber) { case length =>
      get(complete(seed(length)))
    }
}
