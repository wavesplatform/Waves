package com.wavesplatform.riderunner.app

import com.typesafe.config.Config
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.utils.ScorexLogging
import kamon.Kamon
import kamon.metric.{MeasurementUnit, Timer}

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
  val lastKnownHeight = Kamon.gauge("ride.height", "The last known blockchain height").withoutTags()

  private val rideScriptRunOnHeightTime_ = Kamon.gauge("ride.script.run-on-height", "Run script running time", MeasurementUnit.time.nanoseconds)
  def rideScriptRunOnHeightTime(force: Boolean) = rideScriptRunOnHeightTime_.withTag("force", force)

  val rideRequestTotalNumber = Kamon.gauge("ride.request.number", "Total registered unique RIDE requests").withoutTags()
  val rideRequestCacheHits   = Kamon.counter("ride.request.cache.hit", "Cache hits for whole request").withoutTags()
  val rideRequestCacheMisses = Kamon.counter("ride.request.cache.miss", "Cache misses for whole request").withoutTags()
  val rideRequestRunTime     = Kamon.timer("ride.request.run", "Request running time")

  private val rideStorageKeyNumber          = Kamon.counter("ride.storage.number", "Number of unique keys in storage")
  def rideStorageKeyNumberFor(name: String) = rideStorageKeyNumber.withTag("name", name)

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

  private val workSchedulerSize = Kamon.gauge("work-scheduler.size", "A size of queues in a work scheduler")
  val prioritizedWorkSize       = workSchedulerSize.withTag("tpe", "prioritized")
  // TODO duplicates ride.script.number
  val allWorkSize = workSchedulerSize.withTag("tpe", "all")

  private val jobSchedulerTime       = Kamon.timer("job-scheduler.time")
  val jobSchedulerPrioritizeTime     = jobSchedulerTime.withTag("op", "prioritize")
  val jobSchedulerPrioritizeManyTime = jobSchedulerTime.withTag("op", "prioritizeMany")
  val jobSchedulerGetJobTime         = jobSchedulerTime.withTag("op", "getJob")

  implicit def timeExt(timer: Timer): TimerExt = new TimerExt(timer)
}
