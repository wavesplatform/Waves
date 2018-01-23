package scorex.api.http

import akka.actor.ActorSystem
import akka.event.Logging.LogLevel
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.{HttpRequest, StatusCodes}
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.server.{Directive0, Route}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.RouteResult.Complete
import akka.http.scaladsl.server.directives.{DebuggingDirectives, LogEntry, LoggingMagnet}
import akka.stream.ActorMaterializer
import com.wavesplatform.settings.RestAPISettings
import scorex.api.http.swagger.SwaggerDocService

import scala.reflect.runtime.universe.Type

case class CompositeHttpService(system: ActorSystem, apiTypes: Seq[Type], routes: Seq[ApiRoute], settings: RestAPISettings) {

  val swaggerService = new SwaggerDocService(system, ActorMaterializer()(system), apiTypes, settings)

  def withCors: Directive0 = if (settings.cors)
    respondWithHeader(`Access-Control-Allow-Origin`.*) else pass

  private val headers: scala.collection.immutable.Seq[String] = scala.collection.immutable.Seq("Authorization", "Content-Type", "X-Requested-With", "Timestamp", "Signature") ++
    (if (settings.apiKeyDifferentHost) Seq("api_key") else Seq.empty[String])

  val compositeRoute: Route =
    withCors(routes.map(_.route).reduce(_ ~ _)) ~
      swaggerService.routes ~
      (pathEndOrSingleSlash | path("swagger")) {
        redirect("/api-docs/index.html", StatusCodes.PermanentRedirect)
      } ~
      pathPrefix("api-docs") {
        pathEndOrSingleSlash {
          redirect("/api-docs/index.html", StatusCodes.PermanentRedirect)
        } ~
          getFromResourceDirectory("swagger-ui")
      } ~ options {
        respondWithDefaultHeaders(
          `Access-Control-Allow-Credentials`(true),
          `Access-Control-Allow-Headers`(headers),
          `Access-Control-Allow-Methods`(OPTIONS, POST, PUT, GET, DELETE))(withCors(complete(StatusCodes.OK)))
      } ~ complete(StatusCodes.NotFound)

  private def loggingRoute(route: Route) = {
    def logResponse(loggingAdapter: LoggingAdapter)(req: HttpRequest)(res: Any): Unit = {
      res match {
        case Complete(resp) =>
          val level = if (resp.status == StatusCodes.OK) Logging.DebugLevel else Logging.WarningLevel
          val entry = LogEntry(s"HTTP ${resp.status.value} from ${req.method.value} ${req.uri}", level)
          entry.logTo(loggingAdapter)
        case _ =>
      }
    }
    DebuggingDirectives.logRequestResult(LoggingMagnet(logResponse))(route)
  }

  val loggingCompositeRoute: Route = loggingRoute(compositeRoute)
}