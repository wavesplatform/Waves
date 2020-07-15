package com.wavesplatform.extensions

import scala.concurrent.Future

trait Extension {
  def start(context: Context): Unit
  def shutdown(): Future[Unit]
}
