package com.wavesplatform.dexgen

import scala.concurrent.Future

trait OrderGenerator extends Iterator[Iterator[Future[Unit]]] {
  override val hasNext = true
}
