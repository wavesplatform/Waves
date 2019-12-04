package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.sync.BlockV5TestSuite.Configs
import com.wavesplatform.it.sync.activation.ActivationStatusRequest
import com.wavesplatform.it.transactions.NodesFromDocker
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers, OptionValues}

class MerkleRootApiTestSuite
  extends FreeSpec
  with Matchers
  with CancelAfterFailure
  with NodesFromDocker
  with ActivationStatusRequest
  with ReportingTestName
  with OptionValues{

  override protected def nodeConfigs: Seq[Config] = Configs

  "merkle root api returns merkleRootHashes for defined ids" - {

  }
}

object MerkleRootApiTestSuite {

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

  val Configs: Seq[Config] = Default.map(Config.withFallback(_)).take(2)
}
