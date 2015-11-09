package scorex.consensus.nxt.api.http

import scorex.api.http.ApiRoute
import spray.routing.HttpService._
import spray.routing.Route


class NxtConsensusApiRoute extends ApiRoute {
  override val route: Route =
    pathPrefix("consensus") {
      path(""){
        get{
          ???
        }
      }
    }
}
