package com.wavesplatform.it.transactions

import com.wavesplatform.it._
import org.scalatest.FunSuite

import scala.concurrent.ExecutionContext

abstract class BaseTransactionSuite extends FunSuite with IntegrationNodesInitializationAndStopping
  with IntegrationSuiteWithThreeAddresses {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  override lazy val nodes: Seq[Node] = docker.startNodes(
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(3))
      .withDefault(3)
      .withSpecial(_.nonMiner)
      .build
  )

  override lazy val notMiner: Node = nodes.last

}
