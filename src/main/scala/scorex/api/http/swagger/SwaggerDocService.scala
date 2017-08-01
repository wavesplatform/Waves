package scorex.api.http.swagger

import akka.actor.ActorSystem
import com.github.swagger.akka.SwaggerHttpService
import com.github.swagger.akka.model.{Info, License}
import com.wavesplatform.Version
import com.wavesplatform.settings.RestAPISettings
import io.swagger.models.Swagger

class SwaggerDocService(val actorSystem: ActorSystem, val apiClasses: Set[Class[_]], settings: RestAPISettings)
  extends SwaggerHttpService {

  import SwaggerHttpService.prependSlashIfNecessary

  override val host: String = settings.bindAddress + ":" + settings.port
  override val info: Info = Info("The Web Interface to the Waves Full Node API",
    Version.VersionString,
    "Waves Full Node",
    "License: Apache License, Version 2.0",
    None,
    Some(License("Apache License, Version 2.0", "https://github.com/wavesplatform/Waves/blob/master/LICENSE"))
  )

  //Let swagger-ui determine the host and port
  override val swaggerConfig: Swagger = new Swagger().basePath(prependSlashIfNecessary(basePath)).info(info).scheme(scheme)
}
