package scorex.api.http

import akka.actor.ActorSystem
import akka.http.scaladsl.server.RouteConcatenation._
import akka.stream.ActorMaterializer

import scala.reflect.runtime.universe.Type


case class CompositeHttpService(system: ActorSystem, swaggerApiTypes: Seq[Type], routes: Seq[ApiRoute])
  extends SwaggerService {
  implicit val actorSystem = system

  override val actorMaterializer: ActorMaterializer = ActorMaterializer()

  val compositeRoute = routes.map(_.route).reduce(_ ~ _) ~ swaggerRoute
}
