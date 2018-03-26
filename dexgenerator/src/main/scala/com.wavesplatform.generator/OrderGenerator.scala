package com.wavesplatform.generator

import scala.concurrent.Future

trait OrderGenerator extends Iterator[Iterator[Future[Unit]]] {
  override val hasNext = true
}
