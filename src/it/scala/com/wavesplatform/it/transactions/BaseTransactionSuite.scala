package com.wavesplatform.it.transactions

import com.typesafe.config.Config
import com.wavesplatform.it._
import org.scalatest.FunSuite

import scala.concurrent.ExecutionContext

abstract class BaseTransactionSuite extends FunSuite with IntegrationNodesInitializationAndStopping
  with IntegrationSuiteWithThreeAddresses {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  override protected def nodeConfigs: Seq[Config] = NodeConfigs.newBuilder
    .overrideBase(_.quorum(3))
    .withDefault(3)
    .withSpecial(_.nonMiner)
    .buildNonConflicting()

  override def notMiner: AsyncDockerNode = nodes.last

}
