package com.wavesplatform.matcher

import play.api.libs.json.JsObject

trait MatcherSettings {
  def settingsJSON: JsObject

  private val DefaultAddress = "127.0.0.1"
  private val DefaultPort = 6870

  lazy val matcherSettings = settingsJSON \ "matcher"
  lazy val isRunMatcher = matcherSettings.toOption.isDefined
  lazy val matcherAddress = (matcherSettings \ "address").asOpt[String].getOrElse(DefaultAddress)
  lazy val matcherPort = (matcherSettings \ "port").asOpt[Int].getOrElse(DefaultPort)
}
