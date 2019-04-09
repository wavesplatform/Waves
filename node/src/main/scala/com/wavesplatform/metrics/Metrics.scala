package com.wavesplatform.metrics

import java.net.URI
import java.util.concurrent.TimeUnit

import com.wavesplatform.utils.{ScorexLogging, Time}
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import org.influxdb.dto.Point
import org.influxdb.{InfluxDB, InfluxDBFactory}

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

  case class Settings(enable: Boolean, nodeId: Int, influxDb: InfluxDbSettings)

  private implicit val scheduler: SchedulerService = Scheduler.singleThread("metrics")

  private var settings: Settings   = _
  private var time: Time           = _
  private var db: Option[InfluxDB] = None

  def start(config: Settings, thatTime: Time): Future[Boolean] =
    Task {
      db.foreach { dbc =>
        try {
          db = None
          dbc.close()
        } catch {
          case e: Throwable => log.warn(s"Failed to close InfluxDB (${e.getMessage})")
        }
      }
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
          case e: Throwable => log.warn(s"Failed to connect to InfluxDB (${e.getMessage})")
        }
      }

      db.nonEmpty
    }.runAsyncLogErr

  def shutdown(): Unit =
    Task {
      db.foreach(_.close())
    }.runAsyncLogErr

  def write(b: Point.Builder): Unit = {
    db.foreach { db =>
      val ts = time.getTimestamp()
      Task {
        try {
          db.write(
            b
            // Should be a tag, but tags are the strings now
            // https://docs.influxdata.com/influxdb/v1.3/concepts/glossary/#tag-value
              .addField("node", settings.nodeId)
              .tag("node", settings.nodeId.toString)
              .time(ts, TimeUnit.MILLISECONDS)
              .build())
        } catch {
          case e: Throwable => log.warn(s"Failed to send data to InfluxDB (${e.getMessage})")
        }
      }.runAsyncLogErr
    }
  }

  def writeEvent(name: String): Unit = write(Point.measurement(name))

}
