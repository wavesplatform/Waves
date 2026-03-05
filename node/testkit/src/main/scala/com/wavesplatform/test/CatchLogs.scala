package com.wavesplatform.test

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.AppenderBase
import com.wavesplatform.utils.ScorexLogging

import scala.collection.mutable.ListBuffer

trait CatchLogs { this: ScorexLogging =>
  import ch.qos.logback.classic.Logger

  val inMemoryLog = {
    val logger = log.underlying.asInstanceOf[Logger]
    val r      = new InMemoryAppender()
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
