package scorex.api.http

import akka.actor.ActorRefFactory
import com.gettyimages.spray.swagger.SwaggerHttpService
import com.wordnik.swagger.model.ApiInfo
import spray.routing.HttpService._

import scala.reflect.runtime.universe.Type

trait SwaggerService {

  implicit val context: ActorRefFactory
  val swaggerApiTypes: Seq[Type]

  val swaggerService = new SwaggerHttpService {
    override def apiTypes = swaggerApiTypes

    override def apiVersion = "1.0.0"

    override def baseUrl = "/" // let swagger-ui determine the host and port

    override def docsPath = "api-docs"

    override def actorRefFactory = context

    override def apiInfo = Some(
      new ApiInfo(
        "Scorex API",
        "The Web Interface to the Scorex API",
        null,
        "kushti@protonmail.ch",
        "License: Creative Commons CC0",
        "https://github.com/ScorexProject/Scorex/blob/master/COPYING")
    )

    //authorizations, not used
  }


  lazy val swaggerRoute = swaggerService.routes ~
    get {
      pathPrefix("") {
        pathEndOrSingleSlash {
          getFromResource("swagger-ui/index.html")
        }
      } ~
        getFromResourceDirectory("swagger-ui")
    }
}
