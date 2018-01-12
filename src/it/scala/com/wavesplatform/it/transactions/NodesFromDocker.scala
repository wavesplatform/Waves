package com.wavesplatform.it.transactions

import com.typesafe.config.Config
import com.wavesplatform.it.api.Node
import com.wavesplatform.it.{DockerBased, Nodes}
import monix.eval.Coeval
import org.scalatest.Suite

trait NodesFromDocker extends Suite with Nodes with DockerBased {
  protected def nodeConfigs: Seq[Config]

  protected val nodesSingleton: Coeval[Seq[Node]] = dockerSingleton
    .map(_.startNodes(nodeConfigs))
    .memoize

  override protected def nodes: Seq[Node] = nodesSingleton()

  override protected def beforeAll(): Unit = {
    nodesSingleton.run
    super.beforeAll()
  }
}