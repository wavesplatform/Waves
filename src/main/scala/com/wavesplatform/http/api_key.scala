package com.wavesplatform.http

import akka.http.scaladsl.model.headers._

import scala.util.Try

object api_key extends ModeledCustomHeaderCompanion[api_key] {
  override val name                 = "X-API-Key"
  override def parse(value: String) = Try(new api_key(value))
}

final class api_key(override val value: String) extends ModeledCustomHeader[api_key] {
  override def companion         = api_key
  override def renderInRequests  = true
  override def renderInResponses = false
}

object deprecated_api_key extends ModeledCustomHeaderCompanion[deprecated_api_key] {
  override val name                 = "api_key"
  override def parse(value: String) = Try(new deprecated_api_key(value))
}

final class deprecated_api_key(override val value: String) extends ModeledCustomHeader[deprecated_api_key] {
  override def companion         = deprecated_api_key
  override def renderInRequests  = true
  override def renderInResponses = false
}
