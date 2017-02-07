package scorex.api.http

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{FreeSpec, Matchers}

class AssetsRouteSpec extends FreeSpec with Matchers with ScalatestRouteTest {
  private def routePath(suffix: String) = s"/assets/$suffix"

  routePath("balance/{address}/{assetId}") in pending
  routePath("balance/{address}") in pending
  routePath("transfer") in pending
  routePath("issue") in pending
  routePath("reissue") in pending
  routePath("burn") in pending
  routePath("order") in pending
}
