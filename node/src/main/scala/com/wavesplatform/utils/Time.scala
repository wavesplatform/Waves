package com.wavesplatform.utils

import java.net.{InetAddress, SocketTimeoutException}
import monix.eval.Task
import monix.execution.ExecutionModel
import monix.execution.schedulers.SchedulerService
import org.apache.commons.net.ntp.NTPUDPClient

import java.time.Duration
import scala.concurrent.duration.DurationInt

trait Time {
  def correctedTime(): Long
  def getTimestamp(): Long
}

class NTP(ntpServer: String) extends Time with ScorexLogging with AutoCloseable {
  private[this] val ExpirationTimeout = 60.seconds
  private[this] val RetryDelay        = 10.seconds
  private[this] val ResponseTimeout   = 10.seconds

  private[this] implicit val scheduler: SchedulerService =
    Schedulers.singleThread(name = "time-impl", reporter = log.error("Error in NTP", _), ExecutionModel.AlwaysAsyncExecution)

  private[this] val client = new NTPUDPClient()
  client.setDefaultTimeout(Duration.ofMillis(ResponseTimeout.toMillis))

  @volatile private[this] var ntpTimestamp = System.currentTimeMillis()
  @volatile private[this] var nanoTime     = System.nanoTime()

  def correctedTime(): Long = {
    val timestamp = ntpTimestamp
    val offset    = (System.nanoTime() - nanoTime) / 1000000
    timestamp + offset
  }

  @volatile private[this] var txTime: Long = 0

  def getTimestamp(): Long = {
    txTime = Math.max(correctedTime(), txTime + 1)
    txTime
  }

  private[this] val updateTask: Task[Unit] = {
    def newOffsetTask: Task[Option[(InetAddress, Long, Long)]] = Task {
      try {
        client.open()
        val beforeRequest   = System.nanoTime()
        val info            = client.getTime(InetAddress.getByName(ntpServer))
        val message         = info.getMessage
        val ntpTime         = message.getTransmitTimeStamp.getTime
        val serverSpentTime = message.getTransmitTimeStamp.getTime - message.getReceiveTimeStamp.getTime
        val roundripTime    = (System.nanoTime() - beforeRequest) / 1000000 - serverSpentTime
        val corrected       = ntpTime + roundripTime / 2
        Some((info.getAddress, corrected, System.nanoTime()))
      } catch {
        case _: SocketTimeoutException =>
          None
        case t: Throwable =>
          log.warn("Problems with NTP: ", t)
          None
      } finally {
        client.close()
      }
    }

    newOffsetTask.flatMap {
      case None if !scheduler.isShutdown => updateTask.delayExecution(RetryDelay)
      case Some((server, ntpTimestamp, nanoTime)) if !scheduler.isShutdown =>
        log.trace(s"Adjusting time with ${ntpTimestamp - System.currentTimeMillis()} milliseconds, source: ${server.getHostName}.")
        this.ntpTimestamp = ntpTimestamp
        this.nanoTime = nanoTime
        updateTask.delayExecution(ExpirationTimeout)
      case _ => Task.unit
    }
  }

  private[this] val taskHandle = updateTask.runAsyncLogErr

  override def close(): Unit = {
    log.trace("Shutting down Time")
    taskHandle.cancel()
    scheduler.shutdown()
  }
}
