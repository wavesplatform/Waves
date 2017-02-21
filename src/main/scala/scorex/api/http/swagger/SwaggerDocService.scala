package scorex.api.http.swagger

import scala.reflect.runtime.universe.Type
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.github.swagger.akka.model.{Info, License}
import com.github.swagger.akka.{HasActorSystem, SwaggerHttpService}
import com.wavesplatform.settings.{Constants, RestAPISettings}
import io.swagger.models.Swagger

class SwaggerDocService(val actorSystem: ActorSystem, val materializer: ActorMaterializer, val apiTypes: Seq[Type], settings: RestAPISettings)
  extends SwaggerHttpService with HasActorSystem {

  override val host: String = settings.bindAddress + ":" + settings.port
  override val info: Info = Info("The Web Interface to the Waves Full Node API",
    Constants.VersionString,
    "Waves Full Node",
    "License: Apache License, Version 2.0",
    None,
    Some(License("Apache License, Version 2.0", "https://github.com/wavesplatform/Waves/blob/master/LICENSE"))
  )

  //Let swagger-ui determine the host and port
  override val swaggerConfig: Swagger = new Swagger().basePath(prependSlashIfNecessary(basePath)).info(info).scheme(scheme)
}
