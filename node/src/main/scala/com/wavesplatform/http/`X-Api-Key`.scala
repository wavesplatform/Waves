package com.wavesplatform.http

import akka.http.scaladsl.model.headers._

import scala.util.Try

object `X-Api-Key` extends ModeledCustomHeaderCompanion[`X-Api-Key`] {
  override val name                 = "X-API-Key"

  override def parse(value: String) = Try(new `X-Api-Key`(value))
}

final class `X-Api-Key`(val value: String) extends ModeledCustomHeader[`X-Api-Key`] {
  override def companion = `X-Api-Key`
  override def renderInRequests  = true
  override def renderInResponses = false
}

object api_key extends ModeledCustomHeaderCompanion[api_key] {
  override val name                 = "api_key"

  override def parse(value: String) = Try(new api_key(value))
}

final class api_key(val value: String) extends ModeledCustomHeader[api_key] {
  override def companion = api_key
  override def renderInRequests  = true
  override def renderInResponses = false
}
