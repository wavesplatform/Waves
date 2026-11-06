package com.wavesplatform.test

import ch.qos.logback.classic.{Level, Logger}
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.AppenderBase
import com.wavesplatform.utils.ScorexLogging
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

trait CatchLogs { this: ScorexLogging =>
  val inMemoryLog: InMemoryAppender = CatchLogs.attach(log.underlying.asInstanceOf[Logger])
}

object CatchLogs {

  /** Catches the logs of `cls`, which the test does not construct itself, and detaches afterwards: its logger is JVM-wide. */
  def withLogsOf[A](cls: Class[?])(f: InMemoryAppender => A): A = {
    val logger    = LoggerFactory.getLogger(cls).asInstanceOf[Logger]
    val prevLevel = logger.getLevel
    val appender  = attach(logger)
    try f(appender)
    finally {
      logger.detachAppender(appender)
      appender.stop()
      logger.setLevel(prevLevel)
    }
  }

  private def attach(logger: Logger): InMemoryAppender = {
    val r = new InMemoryAppender()
    r.setContext(logger.getLoggerContext)
    r.start()
    logger.addAppender(r)
    logger.setLevel(Level.TRACE) // Catch logs even logging is disabled
    r
  }
}

class InMemoryAppender extends AppenderBase[ILoggingEvent] {
  private val events = ListBuffer.empty[ILoggingEvent]

  override def append(eventObject: ILoggingEvent): Unit = synchronized {
    events += eventObject
  }

  def getMessages: List[String] = synchronized {
    events.map(_.getFormattedMessage).toList
  }

  def getEvents: List[ILoggingEvent] = synchronized {
    events.toList
  }

  def clear(): Unit = synchronized {
    events.clear()
  }
}
