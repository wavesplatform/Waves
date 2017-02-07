package scorex.api.http

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{FreeSpec, Matchers}

abstract class RouteSpec(basePath: String) extends FreeSpec with ScalatestRouteTest with Matchers {
  protected def routePath(suffix: String) = s"$basePath$suffix"
}
