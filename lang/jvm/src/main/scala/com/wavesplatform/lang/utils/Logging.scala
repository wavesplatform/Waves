package com.wavesplatform.lang.utils

import com.typesafe.scalalogging.StrictLogging

trait Logging extends StrictLogging {
  def trace(message: => String): Unit = logger.trace(message)
}
