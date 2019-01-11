package com.wavesplatform.it.matcher

import com.typesafe.config.Config
import com.wavesplatform.it._
import com.wavesplatform.it.transactions.NodesFromDocker
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

  val defaultAssetQuantity = 999999999999L

  val smartFee         = 0.004.waves
  val minFee           = 0.001.waves + smartFee
  val issueFee         = 1.waves
  val smartIssueFee    = 1.waves + smartFee
  val leasingFee       = 0.002.waves + smartFee
  val tradeFee         = 0.003.waves
  val smartTradeFee    = tradeFee + smartFee
  val twoSmartTradeFee = tradeFee + 2 * smartFee

  protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .withDefault(4)
      .buildNonConflicting()

}
