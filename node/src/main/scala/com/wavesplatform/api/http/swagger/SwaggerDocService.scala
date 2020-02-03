package com.wavesplatform.api.http.swagger

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.github.swagger.akka.SwaggerHttpService
import com.github.swagger.akka.model.{Info, License}
import com.wavesplatform.Version
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.http.`X-Api-Key`
import com.wavesplatform.settings.RestAPISettings
import io.swagger.models.auth.{ApiKeyAuthDefinition, In}
import io.swagger.models.{Scheme, Swagger}

class SwaggerDocService(val actorSystem: ActorSystem, val materializer: ActorMaterializer, val apiClasses: Set[Class[_]], settings: RestAPISettings)
    extends SwaggerHttpService {

  override val host: String = settings.bindAddress + ":" + settings.port
  override lazy val info: Info = {
    def chainIdString: String =
      if (Character.isAlphabetic(AddressScheme.current.chainId)) AddressScheme.current.chainId.toChar.toString
      else "#" + AddressScheme.current.chainId.toInt

    Info(
      "The Web Interface to the Waves Full Node API",
      Version.VersionString,
      s"Waves Full Node ($chainIdString)",
      "",
      None,
      Some(License("MIT License", "https://github.com/wavesplatform/Waves/blob/master/LICENSE"))
    )
  }

  //Let swagger-ui determine the host and port
  override val swaggerConfig: Swagger = new Swagger()
    .basePath(SwaggerHttpService.prependSlashIfNecessary(basePath))
    .info(info)
    .scheme(Scheme.HTTP)
    .scheme(Scheme.HTTPS)
    .securityDefinition(SwaggerDocService.ApiKeyDefName, new ApiKeyAuthDefinition(`X-Api-Key`.name, In.HEADER))

  override def unwantedDefinitions: Seq[String] = Seq("Function1RequestContextFutureRouteResult", "Function1")
}

object SwaggerDocService {
  final val ApiKeyDefName = "API Key"
}
