package com.wavesplatform.metrics

import java.net.URI
import java.util.concurrent.TimeUnit

import monix.eval.Task
import monix.execution.schedulers.SchedulerService
import org.influxdb.dto.Point
import org.influxdb.{InfluxDB, InfluxDBFactory}
import scorex.utils.ScorexLogging

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

object Metrics extends ScorexLogging {

  case class InfluxDbSettings(uri: URI,
                              db: String,
                              username: Option[String],
                              password: Option[String],
                              batchActions: Int,
                              batchFlashDuration: FiniteDuration)

  case class Settings(enable: Boolean,
                      nodeId: Int,
                      influxDb: InfluxDbSettings)

  private implicit val scheduler: SchedulerService = monix.execution.Scheduler.singleThread("metrics", reporter = com.wavesplatform.utils.UncaughtExceptionsToLogReporter)

  private var settings: Settings = _
  private var db: Option[InfluxDB] = None

  def start(config: Settings): Future[Boolean] = Task {
    shutdown()
    settings = config
    if (settings.enable) {
      import config.{influxDb => dbSettings}

      log.info(s"Metrics are enabled and will be sent to ${dbSettings.uri}/${dbSettings.db}")
      val x = if (dbSettings.username.nonEmpty && dbSettings.password.nonEmpty) {
        InfluxDBFactory.connect(
          dbSettings.uri.toString,
          dbSettings.username.getOrElse(""),
          dbSettings.password.getOrElse("")
        )
      } else {
        InfluxDBFactory.connect(dbSettings.uri.toString)
      }
      x.setDatabase(dbSettings.db)
      x.enableBatch(dbSettings.batchActions, dbSettings.batchFlashDuration.toSeconds.toInt, TimeUnit.SECONDS)

      try {
        val pong = x.ping()
        log.info(s"Metrics will be sent to ${dbSettings.uri}/${dbSettings.db}. Connected in ${pong.getResponseTime}ms.")
        db = Some(x)
      } catch {
        case NonFatal(e) =>
          log.warn("Can't connect to InfluxDB", e)
      }
    }

    db.nonEmpty
  }.runAsync

  def shutdown(): Unit = Task {
    db.foreach(_.close())
  }.runAsync

  def write(b: Point.Builder): Unit = Task {
    db.foreach(_.write(b
      // Should be a tag, but tags are the strings now
      // https://docs.influxdata.com/influxdb/v1.3/concepts/glossary/#tag-value
      .addField("node", settings.nodeId)
      .build()
    ))
  }.runAsync

  def writeEvent(name: String): Unit = write(Point.measurement(name))

}
