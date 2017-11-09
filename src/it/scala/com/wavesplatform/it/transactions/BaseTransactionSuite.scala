package com.wavesplatform.it.transactions

import com.wavesplatform.it._
import org.scalatest.FunSuite

import scala.concurrent.ExecutionContext

abstract class BaseTransactionSuite extends FunSuite with IntegrationNodesInitializationAndStopping
  with IntegrationSuiteWithThreeAddresses {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  override lazy val nodes: Seq[Node] = docker.startNodes(NodeConfigs.forTest(3, 1 -> "waves.miner.enable = no"))
  override lazy val notMiner: Node = nodes.last

}
