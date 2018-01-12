package com.wavesplatform.it

import com.typesafe.config.Config
import com.wavesplatform.it.api.Node

trait Nodes {
  protected def nodes: Seq[Node]
  protected def nodeConfigs: Seq[Config]
}
