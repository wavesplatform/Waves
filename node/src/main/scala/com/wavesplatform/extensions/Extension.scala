package com.wavesplatform.extensions
import scala.concurrent.Future

trait Extension {
  def shutdown(): Future[Unit]
}
