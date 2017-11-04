package com.wavesplatform.it.transactions

import com.wavesplatform.it._
import org.scalatest.FunSuite

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

abstract class BaseTransactionSuite extends FunSuite with IntegrationNodesInitializationAndStopping
  with IntegrationSuiteWithThreeAddresses {

  protected implicit val ec: ExecutionContext = global

  override val docker = Docker(getClass)
  override val nodes: Seq[Node] = NodeConfigs.default(3, 1).map(docker.startNode)

  override val allNodes: Seq[Node] = nodes
  override val notMiner: Node = allNodes.last

}
