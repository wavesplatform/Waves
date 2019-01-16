package com.wavesplatform.api.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model.{HttpRequest, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.RouteResult.Complete
import akka.http.scaladsl.server.directives.{DebuggingDirectives, LoggingMagnet}
import akka.http.scaladsl.server.{Directive0, Route, RouteResult}
import akka.stream.ActorMaterializer
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.api.http.swagger.SwaggerDocService
import com.wavesplatform.utils.ScorexLogging

case class CompositeHttpService(system: ActorSystem, apiTypes: Set[Class[_]], routes: Seq[ApiRoute], settings: RestAPISettings)
    extends ScorexLogging {

  val swaggerService = new SwaggerDocService(system, ActorMaterializer()(system), apiTypes, settings)

  def withCors: Directive0 =
    if (settings.cors)
      respondWithHeader(`Access-Control-Allow-Origin`.*)
    else pass

  private val headers: scala.collection.immutable.Seq[String] = scala.collection.immutable.Seq("Authorization",
                                                                                               "Content-Type",
                                                                                               "X-Requested-With",
                                                                                               "Timestamp",
                                                                                               "x-api-key",
                                                                                               "Signature") ++
    (if (settings.apiKeyDifferentHost) Seq("api_key", "X-API-Key") else Seq.empty[String])

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
      respondWithDefaultHeaders(`Access-Control-Allow-Credentials`(true),
                                `Access-Control-Allow-Headers`(headers),
                                `Access-Control-Allow-Methods`(OPTIONS, POST, PUT, GET, DELETE))(withCors(complete(StatusCodes.OK)))
    } ~ complete(StatusCodes.NotFound)

  def logRequestResponse(req: HttpRequest)(res: RouteResult): Unit = res match {
    case Complete(resp) =>
      val msg = s"HTTP ${resp.status.value} from ${req.method.value} ${req.uri}"
      if (resp.status == StatusCodes.OK) log.debug(msg) else log.warn(msg)
    case _ =>
  }

  val loggingCompositeRoute: Route =
    DebuggingDirectives.logRequestResult(LoggingMagnet(_ => logRequestResponse))(compositeRoute)
}
