package com.wavesplatform.metrics

import java.net.URI
import java.util.concurrent.TimeUnit

import com.wavesplatform.utils.{Schedulers, ScorexLogging, Time}
import monix.eval.Task
import monix.execution.schedulers.SchedulerService
import org.influxdb.dto.Point
import org.influxdb.{InfluxDB, InfluxDBFactory}

import scala.concurrent.duration.FiniteDuration
import scala.util.Try
import scala.util.control.NonFatal

object Metrics extends ScorexLogging {

  case class InfluxDbSettings(
      uri: URI,
      db: String,
      username: Option[String],
      password: Option[String],
      batchActions: Int,
      batchFlashDuration: FiniteDuration
  )

  case class Settings(enable: Boolean, nodeId: Int, influxDb: InfluxDbSettings)

  private[this] implicit val scheduler: SchedulerService = Schedulers.singleThread("metrics")

  private[this] var settings: Settings   = _
  private[this] var time: Time           = _
  private[this] var db: Option[InfluxDB] = None

  private[this] def currentTimestamp: Long =
    if (time == null) System.currentTimeMillis()
    else time.getTimestamp()

  def write(b: Point.Builder, ts: Long = currentTimestamp): Unit = {
    db.foreach { db =>
      Task {
        try {
          val point = b
            .addField("node", settings.nodeId) // Should be a tag, but tags are the strings now: https://docs.influxdata.com/influxdb/v1.3/concepts/glossary/#tag-value
            .tag("node", settings.nodeId.toString)
            .time(ts, TimeUnit.MILLISECONDS)
            .build()

          // log.trace(s"Point written: $point")
          db.write(point)
        } catch {
          case NonFatal(e) => log.warn(s"Failed to send data to InfluxDB (${e.getMessage})")
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
      import config.{influxDb => dbSettings}

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

    db.nonEmpty
  }

  def shutdown(): Unit = synchronized {
    val dbValue = this.db
    this.db = None
    Try(dbValue.foreach(_.close()))
  }
}
