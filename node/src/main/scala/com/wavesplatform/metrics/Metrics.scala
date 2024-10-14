package com.wavesplatform.metrics

import java.net.URI
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration
import scala.util.Try
import scala.util.control.NonFatal

import com.wavesplatform.ResponsivenessLogs
import com.wavesplatform.utils.{Schedulers, ScorexLogging, Time}
import monix.eval.Task
import monix.execution.schedulers.SchedulerService
import org.influxdb.{InfluxDB, InfluxDBFactory}
import org.influxdb.dto.Point

object Metrics extends ScorexLogging {
  case class InfluxDbSettings(
      uri: URI,
      db: String,
      retentionPolicy: String,
      username: Option[String],
      password: Option[String],
      batchActions: Int,
      batchFlashDuration: FiniteDuration
  )

  case class Settings(
      enable: Boolean,
      nodeId: Int,
      influxDb: InfluxDbSettings,
      collectResponsivenessMetrics: Boolean,
      createResponsivenessCsv: Boolean,
      responsivenessMetricsRetentionPolicy: String
  )

  private implicit val scheduler: SchedulerService = Schedulers.singleThread("metrics")

  private[this] var settings: Settings   = _
  private[this] var time: Time           = _
  private var db: Option[InfluxDB] = None

  private def currentTimestamp: Long =
    if (time == null) System.currentTimeMillis()
    else time.getTimestamp()

  def withRetentionPolicy(retentionPolicy: String)(f: => Unit): Unit =
    db.synchronized(db.foreach { db =>
      db.setRetentionPolicy(retentionPolicy)
      try f
      finally db.setRetentionPolicy("")
    })

  def write(b: Point.Builder, ts: Long = currentTimestamp): Unit = db.synchronized {
    db.foreach { db =>
      Task {
        try {
          db.write(
            b
              // Should be a tag, but tags are the strings now
              // https://docs.influxdata.com/influxdb/v1.3/concepts/glossary/#tag-value
              .addField("node", settings.nodeId)
              .tag("node", settings.nodeId.toString)
              .time(ts, TimeUnit.MILLISECONDS)
              .build()
          )
        } catch {
          case e: Throwable => log.warn(s"Failed to send data to InfluxDB (${e.getMessage})")
        }
      }.runAsyncLogErr
    }
  }

  def writeEvent(name: String): Unit = write(Point.measurement(name))

  def start(config: Settings, thatTime: Time): Boolean = synchronized {
    this.shutdown()
    settings = config
    time = thatTime
    if (settings.enable) {
      import config.{influxDb as dbSettings}

      log.info(s"Precise metrics are enabled and will be sent to ${dbSettings.uri}/${dbSettings.db}")
      try {
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
        x.setRetentionPolicy("")

        try {
          val pong = x.ping()
          log.info(s"Metrics will be sent to ${dbSettings.uri}/${dbSettings.db}. Connected in ${pong.getResponseTime}ms.")
          db = Some(x)
        } catch {
          case NonFatal(e) =>
            log.warn("Can't connect to InfluxDB", e)
        }
      } catch {
        case NonFatal(e) => log.warn(s"Failed to connect to InfluxDB (${e.getMessage})")
      }
    }

    ResponsivenessLogs.enableMetrics = config.collectResponsivenessMetrics
    ResponsivenessLogs.enableCsv = config.createResponsivenessCsv
    ResponsivenessLogs.retentionPolicy = config.responsivenessMetricsRetentionPolicy

    db.nonEmpty
  }

  def shutdown(): Unit = synchronized {
    val dbValue = this.db
    this.db = None
    Try(dbValue.foreach(_.close()))
  }
}
