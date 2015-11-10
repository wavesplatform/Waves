package scorex.api.http

import akka.actor.ActorRefFactory
import spray.routing.HttpService

trait ApiRoute extends HttpService {
  val context: ActorRefFactory
  val route: spray.routing.Route
  def actorRefFactory = context

}
