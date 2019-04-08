package com.wavesplatform.it.async

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{NodeConfigs, TransferSending}
import org.scalatest._

import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class MicroblocksGenerationSuite extends FreeSpec with Matchers with TransferSending with NodesFromDocker {
  import MicroblocksGenerationSuite._

  override protected val nodeConfigs: Seq[Config] =
    Seq(ConfigOverrides.withFallback(NodeConfigs.randomMiner))

  private val nodeAddresses = nodeConfigs.map(_.getString("address")).toSet

  private def miner = nodes.head

  s"Generate transactions and wait for one block with $maxTxs txs" in result(
    for {
      uploadedTxs <- processRequests(generateTransfersToRandomAddresses(maxTxs, nodeAddresses))
      _           <- miner.waitForHeight(3)
      block       <- miner.blockAt(2)
    } yield {
      block.transactions.size shouldBe maxTxs

      val blockTxs = block.transactions.map(_.id)
      val diff     = uploadedTxs.map(_.id).toSet -- blockTxs
      diff shouldBe empty
    },
    3.minutes
  )

}

object MicroblocksGenerationSuite {
  private val txsInMicroBlock = 200
  private val maxTxs          = 2000
  private val ConfigOverrides = ConfigFactory.parseString(s"""waves {
                                                             |    miner {
                                                             |      quorum = 0
                                                             |      minimal-block-generation-offset = 1m
                                                             |      micro-block-interval = 3s
                                                             |      max-transactions-in-key-block = 0
                                                             |      max-transactions-in-micro-block = $txsInMicroBlock
                                                             |    }
                                                             |
                                                             |    blockchain.custom.functionality.pre-activated-features.2 = 0
                                                             |    features.supported = [2]
                                                             |}""".stripMargin)
}
