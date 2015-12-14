package scorex.utils

import org.slf4j.LoggerFactory

trait ScorexLogging {
  protected def log = LoggerFactory.getLogger(this.getClass)
}
