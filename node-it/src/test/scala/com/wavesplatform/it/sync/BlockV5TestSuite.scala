package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.block.Block
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures}
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.activation.ActivationStatusRequest
import com.wavesplatform.it.transactions.NodesFromDocker
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers, OptionValues}

import scala.concurrent.duration._

class BlockV5TestSuite
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with NodesFromDocker
    with ActivationStatusRequest
    with ReportingTestName
    with OptionValues {
  import BlockV5TestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  "block v5 appears and blockchain grows" - {
    "when feature activation happened" in {

      nodes.head.featureActivationStatus(BlockchainFeatures.BlockV5.id).blockchainStatus should not be BlockchainFeatureStatus.Activated

      nodes.head.waitForHeight(ActivationHeight, 5.minutes)

      nodes.head.featureActivationStatus(BlockchainFeatures.BlockV5.id).blockchainStatus shouldBe BlockchainFeatureStatus.Activated

      val blockAtActivationHeight = nodes.head.blockAt(ActivationHeight)
      blockAtActivationHeight.version shouldBe 'defined
      blockAtActivationHeight.version.value should not be Block.ProtoBlockVersion

      nodes.waitForHeightArise()

      nodes.head.transfer(nodes.head.address, nodes.last.address, transferAmount, minFee, waitForTx = true)

      val blockAfterActivationHeight = nodes.head.blockAt(ActivationHeight + 1)
      blockAfterActivationHeight.version shouldBe 'defined
      blockAfterActivationHeight.version.value shouldBe Block.ProtoBlockVersion

      nodes.waitForHeightArise()

      nodes.head.transfer(nodes.head.address, nodes.last.address, transferAmount, minFee, waitForTx = true)

      val blockAfterVRFUsing = nodes.head.blockAt(ActivationHeight + 2)
      blockAfterVRFUsing.version shouldBe 'defined
      blockAfterVRFUsing.version.value shouldBe Block.ProtoBlockVersion

      nodes.head.transfer(nodes.head.address, nodes.last.address, transferAmount, minFee, waitForTx = true)

      nodes.waitForSameBlockHeadersAt(nodes.head.height + 5, conditionAwaitTime = 11.minutes)
    }
  }
}

object BlockV5TestSuite {

  import com.wavesplatform.it.NodeConfigs.Default

  val MicroblockActivationHeight = 0
  val FairPosActivationHeight    = 0
  val ActivationHeight           = 3

  val Config: Config = ConfigFactory.parseString(
    s"""
       |waves {
       |   blockchain.custom {
       |      functionality {
       |        pre-activated-features {
       |          ${BlockchainFeatures.NG.id} = $MicroblockActivationHeight,
       |          ${BlockchainFeatures.FairPoS.id} = $FairPosActivationHeight,
       |          ${BlockchainFeatures.BlockV5.id} = $ActivationHeight
       |        }
       |        generation-balance-depth-from-50-to-1000-after-height = 1000
       |      }
       |   }
       |   miner.quorum = 1
       |}""".stripMargin
  )

  val Configs: Seq[Config] = Default.map(Config.withFallback(_)).take(3)
}
