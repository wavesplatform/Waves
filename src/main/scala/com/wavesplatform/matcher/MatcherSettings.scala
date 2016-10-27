package com.wavesplatform.matcher

import play.api.libs.json.JsObject

trait MatcherSettings {
  def settingsJSON: JsObject

  private val DefaultHost = "127.0.0.1"
  private val DefaultPort = 6870
  private val DefaultOrderMatchTxFee = 100000

  lazy val matcherSettings = settingsJSON \ "matcher"
  lazy val isRunMatcher = matcherSettings.toOption.isDefined
  lazy val matcherAccount = (matcherSettings \ "account").as[String]
  lazy val matcherHost = (matcherSettings \ "host").asOpt[String].getOrElse(DefaultHost)
  lazy val matcherPort = (matcherSettings \ "port").asOpt[Int].getOrElse(DefaultPort)
  lazy val matcherTxFee = (matcherSettings \ "orderMatchTxFee").asOpt[Int].getOrElse(DefaultOrderMatchTxFee)
}
