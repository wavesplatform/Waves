package com.wavesplatform.http

import akka.http.scaladsl.model.headers._

import scala.util.Try

object api_key extends ModeledCustomHeaderCompanion[api_key] {
  override val name = "api_key"
  override def parse(value: String) = Try(api_key(value))
}

final case class api_key(key: String) extends ModeledCustomHeader[api_key] {
  override def companion = api_key
  override def value = key
  override def renderInRequests = true
  override def renderInResponses = false
}

