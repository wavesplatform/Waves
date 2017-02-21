package scorex.api.http

import scala.reflect.runtime.universe.Type
import akka.actor.ActorSystem
import akka.http.scaladsl.model.{HttpMethods, HttpResponse, StatusCodes}
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive0, RejectionHandler, ValidationRejection}
import akka.stream.ActorMaterializer
import com.wavesplatform.http.PlayJsonException
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.RestAPISettings
import scorex.api.http.swagger.SwaggerDocService

case class CompositeHttpService(system: ActorSystem, apiTypes: Seq[Type], routes: Seq[ApiRoute], settings: RestAPISettings) {

  val swaggerService = new SwaggerDocService(system, ActorMaterializer()(system), apiTypes, settings)

  def withCors: Directive0 = if (settings.cors)
    respondWithHeader(`Access-Control-Allow-Origin`.*) else pass

  private val defaultRejectionHandler = RejectionHandler.newBuilder().handle {
    case ValidationRejection(_, Some(PlayJsonException(cause, errors))) => complete(WrongJson(cause, errors))
  }.result()

  val compositeRoute =
    handleRejections(defaultRejectionHandler) {
      withCors(routes.map(_.route).reduce(_ ~ _))
    } ~
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
        `Access-Control-Allow-Headers`("Authorization", "Content-Type", "X-Requested-With"),
        `Access-Control-Allow-Methods`(OPTIONS, POST, PUT, GET, DELETE))(withCors(complete(StatusCodes.OK)))
    } ~ complete(StatusCodes.NotFound)
}
