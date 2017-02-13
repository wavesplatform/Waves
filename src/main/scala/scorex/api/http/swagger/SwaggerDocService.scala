package scorex.api.http.swagger

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.github.swagger.akka.model.{Info, License}
import com.github.swagger.akka.{HasActorSystem, SwaggerHttpService}
import com.wavesplatform.settings.RestAPISettings
import io.swagger.models.Swagger

import scala.reflect.runtime.universe.Type


class SwaggerDocService(system: ActorSystem, val apiTypes: Seq[Type], settings: RestAPISettings)
  extends SwaggerHttpService with HasActorSystem {

  override implicit val actorSystem: ActorSystem = system
  override implicit val materializer: ActorMaterializer = ActorMaterializer()

  override val host = s"${settings.bindAddress}:${settings.port}"
  override val apiDocsPath: String = "swagger"

  override val info: Info = Info("The Web Interface to the Waves API",
    "0.5.0",
    "Waves API",
    "License: Creative Commons CC0",
    None,
    Some(License("License: Creative Commons CC0", "https://github.com/ScorexProject/Scorex/blob/master/COPYING"))
  )

  //Let swagger-ui determine the host and port
  override val swaggerConfig: Swagger = new Swagger().basePath(prependSlashIfNecessary(basePath)).info(info).scheme(scheme)
}