package com.wavesplatform.ride.runner.stats

import com.typesafe.config.Config
import com.wavesplatform.api.UpdateType
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.utils.ScorexLogging
import kamon.Kamon
import kamon.metric.Timer

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Try

class RideRunnerStats(globalConfig: Config) extends AutoCloseable with ScorexLogging {
  // IMPORTANT: to make use of default settings for histograms and timers, it's crucial to reconfigure Kamon with
  //            our merged config BEFORE initializing any metrics, including in settings-related companion objects
  Kamon.init(globalConfig)

  if (Kamon.enabled()) log.info("Metrics enabled")
  else log.info("Metrics disabled")

  override def close(): Unit = if (Kamon.enabled()) { Try(Await.result(Kamon.stop(), 5 seconds)) }
}

object RideRunnerStats {
  val lastKnownHeight = Kamon.gauge("ride.height", "The last known blockchain height").withoutTags()

  private val rideRequestTotalAffectedNumberByTypes = {
    val stat = Kamon.gauge("ride.affected.total", "Affected unique RIDE requests")
    UpdateType.All.map { x => x -> stat.withTag("tpe", x.toString) }.toMap
  }
  def rideRequestTotalAffectedNumber(tpe: UpdateType) = rideRequestTotalAffectedNumberByTypes(tpe)

  val rideRequestActiveAffectedNumberByTypes = Kamon.gauge("ride.affected.active", "Affected and active unique RIDE requests").withoutTags()

  val rideRequestTrackNew = Kamon
    .counter("ride.request.track", "New requests and those, who became ignored and now is active after a user's request")
    .withTag("tpe", "new")
  val rideRequestTrackReAdded = Kamon.counter("ride.request.track").withTag("tpe", "re-added")
  val rideRequestActiveNumber = Kamon.gauge("ride.request.active", "Total number of active unique RIDE requests").withoutTags()
  val rideRequestCacheHits    = Kamon.counter("ride.request.cache.hit", "Cache hits for whole request").withoutTags()
  val rideRequestCacheMisses  = Kamon.counter("ride.request.cache.miss", "Cache misses for whole request").withoutTags()
  val rideRequestRunTime      = Kamon.timer("ride.request.run", "Request running time").withoutTags()

  val rideRequestTimeInQueue = Kamon.timer("ride.request.in-queue", "Time in queue").withoutTags()

  private val rideScriptCalls    = Kamon.counter("ride.script.calls", "Ride calls")
  val rideScriptOkCalls          = rideScriptCalls.withTag("type", "ok")
  val rideScriptUnnecessaryCalls = rideScriptCalls.withTag("type", "unnecessary")
  val rideScriptFailedCalls      = rideScriptCalls.withTag("type", "failed")

  val requestServiceIgnoredNumber = Kamon.gauge("ride.request.ignored.total", "Total number of ignored requests").withoutTags()

  private val anyEventProcessingTime = Kamon.timer("blockchain.event.process", "Blockchain events processing time")
  val blockProcessingTime            = anyEventProcessingTime.withTag("tpe", "b")
  val microBlockProcessingTime       = anyEventProcessingTime.withTag("tpe", "mb")
  val rollbackProcessingTime         = anyEventProcessingTime.withTag("tpe", "r")

  private val jobSchedulerSize = Kamon.gauge("job-scheduler.size", "A size of queues in a work scheduler")
  val prioritizedJobSize       = jobSchedulerSize.withTag("tpe", "prioritized")
  val regularJobSize           = jobSchedulerSize.withTag("tpe", "regular")

  val columnFamilyProperties = Kamon.gauge("rocksdb.props", "RocksDB column family properties")
  val dbStats                = Kamon.gauge("rocksdb.stats", "RocksDB statistics")

  implicit def timeExt(timer: Timer): TimerExt = new TimerExt(timer)
}
