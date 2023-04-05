package com.wavesplatform.lang.utils

trait Logging {
  def trace(message: => String): Unit = println(message)
}
