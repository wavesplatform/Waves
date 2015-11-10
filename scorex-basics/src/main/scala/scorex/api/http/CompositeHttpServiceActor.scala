package scorex.api.http

import akka.actor.Actor
import spray.routing.HttpService
import scala.reflect.runtime.universe.Type


class CompositeHttpServiceActor(val swaggerApiTypes: Seq[Type], val routes: ApiRoute*) extends Actor
with HttpService with SwaggerService {

  override def actorRefFactory = context

  override def receive = runRoute(routes.map(_.route).reduce(_ ~ _) ~ swaggerRoute)

}
