package com.wavesplatform

trait Shutdownable {
  def shutdown(): Unit
}
