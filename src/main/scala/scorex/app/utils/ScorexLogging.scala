package scorex.app.utils

import org.slf4j.LoggerFactory
import ch.qos.logback.core.util.StatusPrinter
import ch.qos.logback.classic.LoggerContext

trait ScorexLogging {
  protected def log = LoggerFactory.getLogger(this.getClass)
}
