package com.wavesplatform.it

import com.typesafe.config.Config

trait AsyncNodes {
  protected def nodes: Seq[NodeImpl]
  protected def nodeConfigs: Seq[Config]
}
