package com.wavesplatform.it.matcher

import com.typesafe.config.Config
import com.wavesplatform.it._
import com.wavesplatform.it.transactions.{MatcherNode, NodesFromDocker}
import org.scalatest._
import com.wavesplatform.it.util._
import scala.concurrent.ExecutionContext

abstract class MatcherSuiteBase
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with ReportingTestName
    with NodesFromDocker
    with BeforeAndAfterAll
    with MatcherNode {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  val defaultAssetQuantity = 999999999999l

  val smartFee   = 0.004.waves
  val minFee     = 0.001.waves + smartFee
  val issueFee   = 1.waves + smartFee
  val leasingFee = 0.002.waves + smartFee

  protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .withDefault(4)
      .buildNonConflicting()

  override protected def nodes: Seq[Node] = dockerNodes()

  protected override def beforeAll(): Unit = {
    super.beforeAll()
  }
}
