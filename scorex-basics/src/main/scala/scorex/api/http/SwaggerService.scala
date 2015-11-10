package scorex.api.http

import java.net.InetSocketAddress

import akka.actor.{ActorRefFactory, ActorRef}
import akka.pattern.ask
import com.wordnik.swagger.model.ApiInfo
import com.gettyimages.spray.swagger.SwaggerHttpService
import play.api.libs.json.Json
import spray.routing.HttpService._
import scala.reflect.runtime.universe.Type

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

trait SwaggerService {

  implicit val context: ActorRefFactory
  val swaggerApiTypes: Seq[Type]

  val swaggerService = new SwaggerHttpService {
    override def apiTypes = swaggerApiTypes
    override def apiVersion = "2.0"
    override def baseUrl = "/" // let swagger-ui determine the host and port
    override def docsPath = "api-docs"
    override def actorRefFactory = context
    override def apiInfo = Some(new ApiInfo("Spray-Swagger Sample", "A sample petstore service using spray and spray-swagger.", "TOC Url", "Michael Hamrah @mhamrah", "Apache V2", "http://www.apache.org/licenses/LICENSE-2.0"))

    //authorizations, not used
  }

  lazy val swaggerRoute = swaggerService.routes ~
    get {
      pathPrefix("") { pathEndOrSingleSlash {
        getFromResource("swagger-ui/index.html")
      }
      } ~
        getFromResourceDirectory("swagger-ui")
    }
}