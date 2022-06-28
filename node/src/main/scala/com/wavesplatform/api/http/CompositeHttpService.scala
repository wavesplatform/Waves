package com.wavesplatform.api.http

import akka.http.scaladsl.model.*
import akka.http.scaladsl.model.HttpMethods.*
import akka.http.scaladsl.model.headers.*
import akka.http.scaladsl.server.*
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.RouteResult.Complete
import akka.http.scaladsl.server.directives.{DebuggingDirectives, LoggingMagnet}
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.utils.ScorexLogging
import kamon.Kamon

import scala.io.Source

case class CompositeHttpService(routes: Seq[ApiRoute], settings: RestAPISettings) extends ScorexLogging {

  private val redirectToSwagger = redirect("/api-docs/index.html", StatusCodes.PermanentRedirect)
  private val swaggerRoute: Route =
    (pathEndOrSingleSlash | path("swagger"))(redirectToSwagger) ~
      pathPrefix("api-docs") {
        pathEndOrSingleSlash(redirectToSwagger) ~
          path("openapi.yaml")(complete(patchedSwaggerJson)) ~
          getFromResourceDirectory("swagger-ui")
      }

  val compositeRoute: Route = extractRequest { _ =>
    Kamon
      .currentSpan()
      .mark("processing.start")

    extendRoute(routes.map(_.route).reduce(_ ~ _)) ~ swaggerRoute ~ complete(
      StatusCodes.NotFound
    )
  }

  val loggingCompositeRoute: Route = Route.seal(DebuggingDirectives.logRequestResult(LoggingMagnet(_ => logRequestResponse))(compositeRoute))

  private def logRequestResponse(req: HttpRequest)(res: RouteResult): Unit = res match {
    case Complete(resp) =>
      val msg = s"HTTP ${resp.status.value} from ${req.method.value} ${req.uri}"
      if (resp.status == StatusCodes.OK) log.info(msg) else log.warn(msg)
    case _ =>
  }

  private val commonCorsHeaders =
    Seq(
      `Access-Control-Allow-Headers`(settings.corsHeaders.accessControlAllowHeaders),
      `Access-Control-Allow-Methods`(settings.corsHeaders.accessControlAllowMethods.flatMap(getForKeyCaseInsensitive))
    )

  private def corsAllowAll =
    respondWithHeaders(
      commonCorsHeaders :+ `Access-Control-Allow-Origin`(settings.corsHeaders.accessControlAllowOrigin)
    )

  private def extendRoute(base: Route): Route = handleAllExceptions { ctx =>
    val extendedRoute =
      options {
        val headers = commonCorsHeaders :+ `Access-Control-Allow-Credentials`(settings.corsHeaders.accessControlAllowCredentials)
        respondWithDefaultHeaders(headers)(corsAllowAll(complete(StatusCodes.OK)))
      } ~ corsAllowAll(base)

    extendedRoute(ctx)
  }

  private[this] lazy val patchedSwaggerJson = {
    import com.wavesplatform.Version
    import com.wavesplatform.account.AddressScheme

    def chainIdString: String =
      if (Character.isAlphabetic(AddressScheme.current.chainId)) AddressScheme.current.chainId.toChar.toString
      else s"#${AddressScheme.current.chainId}"

    HttpEntity(
      MediaType.customWithFixedCharset("text", "x-yaml", HttpCharsets.`UTF-8`, List("yaml")),
      Source
        .fromResource("swagger-ui/openapi.yaml")
        .mkString
        .replace("{{version}}", Version.VersionString)
        .replace("{{chainId}}", chainIdString)
    )
  }
}
