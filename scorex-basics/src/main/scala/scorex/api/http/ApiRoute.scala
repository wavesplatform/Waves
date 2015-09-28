package scorex.api.http

import spray.routing.Route

trait ApiRoute {
  val route: Route
}
