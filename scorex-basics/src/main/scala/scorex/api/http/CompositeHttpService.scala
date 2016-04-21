package scorex.api.http

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import scorex.api.http.swagger.{CorsSupport, SwaggerDocService}

import scala.reflect.runtime.universe.Type


case class CompositeHttpService(system: ActorSystem, apiTypes: Seq[Type], routes: Seq[ApiRoute]) extends CorsSupport {
  implicit val actorSystem = system

  val swaggerService = new SwaggerDocService(system, apiTypes)

  val compositeRoute = routes.map(_.route).reduce(_ ~ _) ~ corsHandler(swaggerService.routes) ~
    path("swagger") { getFromResource("swagger-ui/index.html") } ~
    getFromResourceDirectory("swagger-ui")
}
