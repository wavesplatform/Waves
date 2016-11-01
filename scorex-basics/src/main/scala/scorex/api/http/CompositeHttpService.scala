package scorex.api.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import scorex.api.http.swagger.{CorsSupport, SwaggerDocService}
import scorex.settings.Settings

import scala.reflect.runtime.universe.Type


case class CompositeHttpService(system: ActorSystem, apiTypes: Seq[Type], routes: Seq[ApiRoute], settings: Settings)
  extends CorsSupport {

  implicit val actorSystem = system

  val swaggerService = new SwaggerDocService(system, apiTypes, settings)

  val redirectToSwagger: Route = {
    redirect("/swagger", StatusCodes.PermanentRedirect)
  }

  val compositeRoute = routes.map(_.route).reduce(_ ~ _) ~ corsHandler(swaggerService.routes) ~
    path("") {
      redirectToSwagger
    } ~
    path("swagger") {
      getFromResource("swagger-ui/index.html")
    } ~ getFromResourceDirectory("swagger-ui")

}
