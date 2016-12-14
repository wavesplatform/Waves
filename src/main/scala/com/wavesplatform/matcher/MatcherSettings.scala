package com.wavesplatform.matcher

import com.wavesplatform.settings.WavesSettings
import play.api.libs.json.JsObject

import scala.concurrent.duration._

trait MatcherSettings {
  this: WavesSettings =>
  def settingsJSON: JsObject

  private val DefaultMatcherHost = "127.0.0.1"
  private val DefaultMatcherPort = 6886
  private val DefaultOrderMatchTxFee = 100000
  private val DefaultMaxOpenOrderCount = 1000

  lazy val matcherSettings = settingsJSON \ "matcher"
  lazy val isRunMatcher = matcherSettings.toOption.isDefined
  lazy val matcherAccount = (matcherSettings \ "account").as[String]
  lazy val matcherHost = (matcherSettings \ "host").asOpt[String].getOrElse(DefaultMatcherHost)
  lazy val matcherPort = (matcherSettings \ "port").asOpt[Int].getOrElse(DefaultMatcherPort)
  lazy val minOrderFee = (matcherSettings \ "minOrderFee").asOpt[Int].getOrElse(DefaultOrderMatchTxFee)
  lazy val minCancelOrderFee = (matcherSettings \ "minCancelOrderFee").asOpt[Int].getOrElse(DefaultOrderMatchTxFee)
  lazy val orderMatchTxFee = (matcherSettings \ "orderMatchTxFee").asOpt[Int].getOrElse(DefaultOrderMatchTxFee)
  lazy val orderCanceTxFee = (matcherSettings \ "orderCanceTxFee").asOpt[Int].getOrElse(DefaultOrderMatchTxFee)
  lazy val matcherJournalDataDir = (matcherSettings \ "journalDataDir").asOpt[String].getOrElse(getDir("dataDir", "journal").get)
  lazy val matcherSnapshotsDataDir = (matcherSettings \ "snapshotsDataDir").asOpt[String].getOrElse(getDir("dataDir", "snapshots").get)
  lazy val snapshotInterval = (matcherSettings \ "snapshotInterval").asOpt[Int].map(x => FiniteDuration(x, MINUTES)).getOrElse(1.day)
  lazy val maxOpenOrdersCount = (matcherSettings \ "maxOpenOrdersCount").asOpt[Int].getOrElse(DefaultMaxOpenOrderCount)
}
