package scorex.api.http

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{FreeSpec, Matchers}

class AddressRouteSpec extends FreeSpec with Matchers with ScalatestRouteTest {
  private val pathPrefix = "/addresses/"
  private def routePath(suffix: String) = s"$pathPrefix$suffix"

  routePath("") in pending
  routePath("{address}") in pending
  routePath("sign/{address}") in pending
  routePath("signText/{address}") in pending
  routePath("verify/{address}") in pending
  routePath("verifyText/{address}") in pending
  routePath("balance/{address}") in pending
  routePath("seed/{address}") in pending
  routePath("validate/{address}") in pending
  routePath("seq/{from}/{to}") in pending
  routePath("publicKey/{publicKey}") in pending
}
