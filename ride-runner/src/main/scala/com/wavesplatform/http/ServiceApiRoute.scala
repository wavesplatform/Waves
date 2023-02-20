package com.wavesplatform.http

import akka.http.scaladsl.model.{ContentTypes, StatusCodes}
import akka.http.scaladsl.server.{Directive0, Route}
import com.wavesplatform.api.http.*
import com.wavesplatform.api.http.ApiError.ApiKeyNotValid
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.http.ServiceApiRoute.Settings
import com.wavesplatform.jvm.HeapDumps
import com.wavesplatform.utils.StringBytes
import play.api.libs.json.Json

case class ServiceApiRoute(settings: Settings, getServiceStatus: () => HttpServiceStatus) extends ApiRoute {
  protected lazy val apiKeyHash: Option[Array[Byte]] = Base58.tryDecode(settings.apiKeyHash).toOption

  def withAuth: Directive0 = apiKeyHash.fold[Directive0](complete(ApiKeyNotValid)) { hashFromSettings =>
    optionalHeaderValueByType(`X-Api-Key`).flatMap {
      case Some(k) if java.util.Arrays.equals(crypto.secureHash(k.value.utf8Bytes), hashFromSettings) => pass
      case _ =>
        complete(ApiKeyNotValid)
    }
  }

  override val route: Route = pathPrefix("ride") {
    status ~ withAuth { heapDumps }
  }

  def status: Route = path("status") {
    val s = getServiceStatus()
    complete(
      if (s.healthy) StatusCodes.OK else StatusCodes.InternalServerError,
      s
    )
  }

  def heapDumps: Route = pathPrefix("heap-dumps") {
    listHeapDumps ~ createHeapDump ~ downloadHeapDump
  }

  def createHeapDump: Route = (post & pathEndOrSingleSlash & parameter("live".as[Boolean] ? true)) { live =>
    complete(HeapDumps.mk("http", live))
  }

  def listHeapDumps: Route = (get & pathEndOrSingleSlash) {
    complete(HeapDumps.all())
  }

  def downloadHeapDump: Route = (get & path(Remaining)) { path =>
    HeapDumps.pathToFile(path) match {
      case Some(path) => getFromFile(path.toFile, ContentTypes.`application/octet-stream`)
      case None       => complete(StatusCodes.NotFound, Json.obj("error" -> "Path not found"))
    }
  }
}

object ServiceApiRoute {
  case class Settings(apiKeyHash: String)
}
