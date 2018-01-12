package com.wavesplatform.it.transactions

import com.typesafe.config.Config
import com.wavesplatform.it._
import com.wavesplatform.it.api.Node
import monix.eval.Coeval
import org.scalatest.{FunSuite, Suite}

import scala.concurrent.ExecutionContext

class BaseTransactionSuite extends FunSuite with IntegrationNodesInitializationAndStopping
  with IntegrationSuiteWithThreeAddresses {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  protected def nodeConfigs: Seq[Config] = NodeConfigs.newBuilder
    .overrideBase(_.quorum(3))
    .withDefault(3)
    .withSpecial(_.nonMiner)
    .buildNonConflicting()

  override def notMiner: Node = nodes.last

  override protected def nodes: Seq[Node] = ??? // if(command ) else AsyncFromDocker
}

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