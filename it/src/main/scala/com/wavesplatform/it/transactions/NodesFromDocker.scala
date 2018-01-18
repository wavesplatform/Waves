package com.wavesplatform.it.transactions

import com.wavesplatform.it.api.Node
import com.wavesplatform.it.{DockerBased, Nodes}
import monix.eval.Coeval
import org.scalatest.Suite

trait NodesFromDocker extends Nodes with DockerBased { _: Suite =>
  protected val nodesSingleton: Coeval[Seq[Node]] = dockerSingleton
    .map(_.startNodes(nodeConfigs))
    .memoize

  override protected def nodes: Seq[Node] = nodesSingleton()
}
