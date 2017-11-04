package com.wavesplatform.it.transactions

import com.wavesplatform.it._
import org.scalatest.FunSuite

import scala.concurrent.ExecutionContext

abstract class BaseTransactionSuite extends FunSuite with IntegrationNodesInitializationAndStopping
  with IntegrationSuiteWithThreeAddresses {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  override val docker = Docker(getClass)
  override val nodes: Seq[Node] = docker.startNodes(NodeConfigs.default(3, 1))
  override val notMiner: Node = nodes.last

}
