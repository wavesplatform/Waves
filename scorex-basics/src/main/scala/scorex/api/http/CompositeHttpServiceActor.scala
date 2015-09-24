package scorex.api.http

import akka.actor.Actor
import spray.routing.{HttpService, Route}


class CompositeHttpServiceActor(val routes: ApiRoute*) extends Actor with HttpService {

  override def actorRefFactory = context

  override def receive = runRoute(routes.map(_.route).reduce(_~_))
}
