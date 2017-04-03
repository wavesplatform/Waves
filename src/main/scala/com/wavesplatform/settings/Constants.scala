package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import scorex.utils.ScorexLogging

/**
  * System constants here.
  */

object Constants extends ScorexLogging {
  private val appConf = ConfigFactory.load().getConfig("app")

  val ApplicationName = "waves"
  val Product = appConf.getString("product")
  val Release = appConf.getString("release")
  val VersionString = appConf.getString("version")
  val AgentName = s"$Product - $Release v$VersionString"

  val UnitsInWave = 100000000L
  val TotalWaves = 100000000L
}
