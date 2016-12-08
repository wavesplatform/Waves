package com.wavesplatform.matcher

import com.wavesplatform.settings.WavesSettings
import play.api.libs.json.JsObject

trait MatcherSettings {
  this: WavesSettings =>
  def settingsJSON: JsObject

  private val DefaultMatcherHost = "127.0.0.1"
  private val DefaultMatcherPort = 6870
  private val DefaultOrderMatchTxFee = 100000

  lazy val matcherSettings = settingsJSON \ "matcher"
  lazy val isRunMatcher = matcherSettings.toOption.isDefined
  lazy val matcherAccount = (matcherSettings \ "account").as[String]
  lazy val matcherHost = (matcherSettings \ "host").asOpt[String].getOrElse(DefaultMatcherHost)
  lazy val matcherPort = (matcherSettings \ "port").asOpt[Int].getOrElse(DefaultMatcherPort)
  lazy val orderMatchTxFee = (matcherSettings \ "orderMatchTxFee").asOpt[Int].getOrElse(DefaultOrderMatchTxFee)
  lazy val orderCanceTxFee = (matcherSettings \ "orderCanceTxFee").asOpt[Int].getOrElse(DefaultOrderMatchTxFee)
  lazy val matcherJournalDataDir = (matcherSettings \ "journalDataDir").asOpt[String].getOrElse(getDir("dataDir", "journal").get)
  lazy val matcherSnapshotsDataDir = (matcherSettings \ "snapshotsDataDir").asOpt[String].getOrElse(getDir("dataDir", "snapshots").get)
}
