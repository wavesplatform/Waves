package com.wavesplatform.metrics

import java.net.URI
import java.util.concurrent.TimeUnit

import monix.eval.Task
import monix.execution.schedulers.SchedulerService
import org.influxdb.dto.Point
import org.influxdb.{InfluxDB, InfluxDBFactory}
import scorex.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration
import scala.collection.JavaConverters._

object Metrics extends ScorexLogging {

  case class InfluxDbSettings(uri: URI, db: String, batchActions: Int, batchFlashDuration: FiniteDuration)

  case class Settings(enable: Boolean,
                      addTags: Map[String, String],
                      influxDb: InfluxDbSettings) {
    private[Metrics] val addMetricTags: java.util.Map[String, String] = addTags.asJava
  }

  private implicit val scheduler: SchedulerService = monix.execution.Scheduler.singleThread("metrics", reporter = com.wavesplatform.utils.UncaughtExceptionsToLogReporter)

  private var settings: Settings = _
  private var db: Option[InfluxDB] = None

  def start(config: Settings): Unit = Task {
    shutdown()
    settings = config
    if (settings.enable) {
      log.info(s"Metrics are enabled and will be sent to ${settings.influxDb.uri}/${settings.influxDb.db}")
      val x = InfluxDBFactory.connect(settings.influxDb.uri.toString)
      x.setDatabase(settings.influxDb.db)
      x.enableBatch(settings.influxDb.batchActions, settings.influxDb.batchFlashDuration.toSeconds.toInt, TimeUnit.SECONDS)

      db = Some(x)
    }
  }.runAsync

  def shutdown(): Unit = Task {
    db.foreach(_.close())
  }.runAsync

  def write(b: Point.Builder): Unit = Task {
    db.foreach(_.write(b
      .tag(settings.addMetricTags)
      .build()
    ))
  }.runAsync

  def writeEvent(name: String): Unit = write(Point.measurement(name))

}
