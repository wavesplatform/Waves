package com.wavesplatform.extensions

import com.wavesplatform.events.BlockchainUpdateTriggers

import scala.concurrent.Future

trait Extension {
  def start(): Unit
  def shutdown(): Future[Unit]
  def blockchainUpdateTriggers: BlockchainUpdateTriggers = BlockchainUpdateTriggers.noop
}
