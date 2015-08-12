package scorex.integration

import org.scalatest.{Matchers, FlatSpec}
import spray.routing.HttpService
import spray.testkit.ScalatestRouteTest

trait RouteTest extends FlatSpec with ScalatestRouteTest with HttpService with Matchers {
  def actorRefFactory = system
}
