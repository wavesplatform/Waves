package com.wavesplatform.utils

import org.apache.commons.net.ntp.NTPUDPClient

import java.net.{InetAddress, SocketTimeoutException}
import java.time.Duration
import java.util.concurrent.*

trait Time extends AutoCloseable {
  def correctedTime(): Long
  def monotonicMillis(): Long = System.nanoTime() / 1_000_000
  override def close(): Unit  = {}
}

object Time {
  object SystemTime extends Time {
    override def correctedTime(): Long = System.currentTimeMillis()
  }

  def apply(ntpServer: String): Time = if (ntpServer.isBlank) SystemTime else new NTP(ntpServer)
}

class NTP(ntpServer: String) extends Time with ScorexLogging {
  private val ExpirationTimeout = 60 // seconds
  private val RetryDelay        = 10 // seconds
  private val ResponseTimeout   = 10 // seconds

  private val scheduler = {
    val xc = new ScheduledThreadPoolExecutor(1, Schedulers.threadFactory("time-impl", true, log.error("Error in NTP", _)))
    xc.setExecuteExistingDelayedTasksAfterShutdownPolicy(false)
    xc
  }

  private val client = new NTPUDPClient()
  client.setDefaultTimeout(Duration.ofSeconds(ResponseTimeout))

  @volatile private var ntpTimestamp = System.currentTimeMillis()
  @volatile private var nanoTime     = System.nanoTime()

  def correctedTime(): Long = {
    val timestamp = ntpTimestamp
    val offset    = (System.nanoTime() - nanoTime) / 1_000_000
    timestamp + offset
  }

  private def retry(delayInSeconds: Long) = scheduler.schedule((() => updateTimestamp()): Runnable, delayInSeconds, TimeUnit.SECONDS)

  private def updateTimestamp(): Unit =
    try {
      client.open()
      val beforeRequest   = System.nanoTime()
      val info            = client.getTime(InetAddress.getByName(ntpServer))
      val message         = info.getMessage
      val ntpTime         = message.getTransmitTimeStamp.getTime
      val serverSpentTime = message.getTransmitTimeStamp.getTime - message.getReceiveTimeStamp.getTime
      val roundTripTime   = (System.nanoTime() - beforeRequest) / 1_000_000 - serverSpentTime
      val corrected       = ntpTime + roundTripTime / 2
      log.trace(s"Adjusting time with ${ntpTimestamp - System.currentTimeMillis()} milliseconds, source: ${info.getAddress.getHostName}.")
      ntpTimestamp = corrected
      nanoTime = System.nanoTime()
      retry(ExpirationTimeout)
    } catch {
      case _: SocketTimeoutException => retry(RetryDelay)
      case t: Throwable =>
        log.warn("Problems with NTP", t)
        retry(RetryDelay)
    } finally {
      client.close()
    }

  override def close(): Unit = {
    log.trace("Shutting down Time")
    scheduler.shutdown()
  }
}
