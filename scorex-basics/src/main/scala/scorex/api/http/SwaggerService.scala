package scorex.api.http

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import com.github.swagger.akka.model.{Contact, Info, License}
import com.github.swagger.akka.{HasActorSystem, SwaggerHttpService}

import scala.reflect.runtime.universe.Type

trait SwaggerService {

  implicit val system: ActorSystem
  val actorMaterializer: ActorMaterializer
  val swaggerApiTypes: Seq[Type]

  val swaggerService = new SwaggerHttpService with HasActorSystem {
    override def apiTypes = swaggerApiTypes

    override val info: Info = Info("The Web Interface to the Scorex API",
      "1.2.4",
      "Scorex API",
      "License: Creative Commons CC0",
      Some(Contact("Alex", "https://scorex-dev.groups.io/g/main", "alex.chepurnoy@iohk.io")),
      Some(License("License: Creative Commons CC0", "https://github.com/ScorexProject/Scorex/blob/master/COPYING"))
    )

    override implicit val actorSystem: ActorSystem = system
    override implicit val materializer: ActorMaterializer = actorMaterializer
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
