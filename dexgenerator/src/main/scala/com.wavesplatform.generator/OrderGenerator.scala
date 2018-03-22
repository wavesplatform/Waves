package com.wavesplatform.generator

import scorex.transaction.assets.exchange.Order

import scala.concurrent.Future

trait OrderGenerator extends Iterator[Iterator[Future[Unit]]] {
  override val hasNext = true
}
