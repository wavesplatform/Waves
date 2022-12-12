package com.wavesplatform.ride.app

import com.typesafe.config.Config
import com.wavesplatform.utils.ScorexLogging
import kamon.Kamon

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Try

class Metrics(globalConfig: Config) extends AutoCloseable with ScorexLogging {
  // IMPORTANT: to make use of default settings for histograms and timers, it's crucial to reconfigure Kamon with
  //            our merged config BEFORE initializing any metrics, including in settings-related companion objects
  if (globalConfig.getBoolean("kamon.enable")) {
    Kamon.init(globalConfig)
    log.info("Metrics enabled")
  } else {
    Kamon.stop()
    log.info("Metrics disabled")
  }

  override def close(): Unit = {
    log.info("Stopping metrics")
    Try(Await.result(Kamon.stop(), 5 seconds))
  }
}
