package scorex.lagonaki.integration

import org.scalatest.{FlatSpec, Matchers}
import scorex.lagonaki.TestingCommons
import spray.routing.HttpService
import spray.testkit.ScalatestRouteTest

trait RouteTest extends FlatSpec with ScalatestRouteTest with HttpService with Matchers with TestingCommons {
  def actorRefFactory = system
}
