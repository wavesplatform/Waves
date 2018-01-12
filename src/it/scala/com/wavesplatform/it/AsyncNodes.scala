package com.wavesplatform.it

import com.typesafe.config.Config

trait AsyncNodes {
  protected def nodes: Seq[AsyncNode]
  protected def nodeConfigs: Seq[Config]
}

//trait HasAsyncNodesWithDocker extends HasAsyncNodes with HasDocker {
//  this: Suite =>
//
//  protected val nodesSingleton: Coeval[Seq[AsyncNode]] = dockerSingleton
//    .map(_.startNodes(nodeConfigs))
//    .memoize
//
//  protected final def nodes: Seq[AsyncNode] = nodesSingleton()
//
//  override protected def beforeAll(): Unit = {
//    nodesSingleton.run
//    super.beforeAll()
//  }
//}
