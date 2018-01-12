package com.wavesplatform.it.transactions

import com.typesafe.config.Config
import com.wavesplatform.it._
import com.wavesplatform.it.api.Node
import monix.eval.Coeval
import org.scalatest.{FunSuite, Suite}

import scala.concurrent.ExecutionContext

abstract class BaseTransactionSuite extends FunSuite with IntegrationNodesInitializationAndStopping
  with IntegrationSuiteWithThreeAddresses {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  protected def nodeConfigs: Seq[Config] = NodeConfigs.newBuilder
    .overrideBase(_.quorum(3))
    .withDefault(3)
    .withSpecial(_.nonMiner)
    .buildNonConflicting()

  override def notMiner: Node = nodes.last
}

trait AsyncNodesFromDocker extends Suite with AsyncNodes with DockerBased {
  protected def nodeConfigs: Seq[Config]

  protected val nodesSingleton: Coeval[Seq[NodeImpl]] = dockerSingleton
    .map(_.startNodes(nodeConfigs))
    .memoize

  override protected def nodes: Seq[NodeImpl] = nodesSingleton()

  override protected def beforeAll(): Unit = {
    nodesSingleton.run
    super.beforeAll()
  }
}