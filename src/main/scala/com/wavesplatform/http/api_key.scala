package com.wavesplatform.http

import akka.http.scaladsl.model.headers._

import scala.util.Try

object api_key extends ModeledCustomHeaderCompanion[api_key] {
  override val name = "api_key"
  override def parse(value: String) = Try(new api_key(value))
}

final class api_key(val key: String) extends ModeledCustomHeader[api_key] {
  override def companion = api_key
  override def value = key
  override def renderInRequests = true
  override def renderInResponses = false
}

