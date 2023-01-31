package com.wavesplatform.ride.app

import com.typesafe.config.Config
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.utils.ScorexLogging
import kamon.Kamon
import kamon.metric.Timer

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Try

class RideRunnerMetrics(globalConfig: Config) extends AutoCloseable with ScorexLogging {
  // IMPORTANT: to make use of default settings for histograms and timers, it's crucial to reconfigure Kamon with
  //            our merged config BEFORE initializing any metrics, including in settings-related companion objects
  if (globalConfig.getBoolean("kamon.enable")) {
    Kamon.init(globalConfig)
    log.info("Metrics enabled")
  } else {
    Kamon.stop()
    log.info("Metrics disabled")
  }

  override def close(): Unit = Try(Await.result(Kamon.stop(), 5 seconds))
}

object RideRunnerMetrics {
  val rideScriptTotalNumber = Kamon.gauge("ride.script.number", "Total registered RIDE scripts").withoutTags()
  val rideScriptCacheHits   = Kamon.counter("ride.script.cache.hit", "Cache hits for whole script").withoutTags()
  val rideScriptCacheMisses = Kamon.counter("ride.script.cache.miss", "Cache misses for whole script").withoutTags()
  val rideScriptRunTime     = Kamon.timer("ride.script.run", "Script running time")

  private val rideScriptCalls    = Kamon.counter("ride.script.calls", "Ride calls")
  val rideScriptOkCalls          = rideScriptCalls.withTag("type", "ok")
  val rideScriptUnnecessaryCalls = rideScriptCalls.withTag("type", "unnecessary")
  val rideScriptFailedCalls      = rideScriptCalls.withTag("type", "failed")

  private val anyEventProcessingTime = Kamon.timer("blockchain.event.process", "Blockchain events processing time")
  val blockProcessingTime            = anyEventProcessingTime.withTag("tpe", "b")
  val microBlockProcessingTime       = anyEventProcessingTime.withTag("tpe", "mb")
  val rollbackProcessingTime         = anyEventProcessingTime.withTag("tpe", "r")

  private val grpcCallTimer                              = Kamon.timer("grpc.call", "gRPC calls time")
  def grpcCallTimerFor(methodName: String, raw: Boolean) = grpcCallTimer.withTag("method", methodName).withTag("raw", raw)

  implicit def timeExt(timer: Timer): TimerExt = new TimerExt(timer)
}
