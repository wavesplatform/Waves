package com.wavesplatform.it

import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase.baseConfig
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import org.scalatest._

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

  val smartFee         = 0.004.waves
  val minFee           = 0.001.waves + smartFee
  val issueFee         = 1.waves
  val smartIssueFee    = 1.waves + smartFee
  val leasingFee       = 0.002.waves + smartFee
  val tradeFee         = 0.003.waves
  val smartTradeFee    = tradeFee + smartFee
  val twoSmartTradeFee = tradeFee + 2 * smartFee

  protected override def createDocker: Docker = new Docker(
    imageName = "com.wavesplatform/dex-it:latest",
    tag = getClass.getSimpleName,
    suiteConfig = baseConfig(ThreadLocalRandom.current().nextInt(0, Int.MaxValue))
  )

  protected def node = dockerNodes().head

  protected def nodeConfigs: Seq[Config] = MatcherPriceAssetConfig.Configs

}

object MatcherSuiteBase {
  private def baseConfig(seed: Int): Config = Option(System.getenv("KAFKA_SERVER")).fold(ConfigFactory.empty()) { kafkaServer =>
    ConfigFactory.parseString(s"""
         |waves.matcher.events-queue {
         |  type = kafka
         |  kafka {
         |    servers = "$kafkaServer"
         |    topic = "dex-$seed"
         |  }
         |}""".stripMargin)
  }
}
