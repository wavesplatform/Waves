package com.wavesplatform.it

import com.typesafe.config.Config
import monix.eval.Coeval
import org.scalatest.Suite

trait HasNodes extends HasDocker {
  this: Suite =>

  protected def nodeConfigs: Seq[Config]
  protected val nodesSingleton: Coeval[Seq[AsyncDockerNode]] = dockerSingleton
    .map(_.startNodes(nodeConfigs))
    .memoize

  protected final def nodes: Seq[AsyncDockerNode] = nodesSingleton()

  override protected def beforeAll(): Unit = {
    nodesSingleton.run
    super.beforeAll()
  }

}
