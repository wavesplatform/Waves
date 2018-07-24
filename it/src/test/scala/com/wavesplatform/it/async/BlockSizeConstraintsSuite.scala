package com.wavesplatform.it.async

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{NodeConfigs, TransferSending}
import org.scalatest._

import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Future

class BlockSizeConstraintsSuite extends FreeSpec with Matchers with TransferSending with NodesFromDocker {
  import BlockSizeConstraintsSuite._

  override protected val nodeConfigs: Seq[Config] =
    Seq(ConfigOverrides.withFallback(NodeConfigs.randomMiner))

  private val nodeAddresses = nodeConfigs.map(_.getString("address")).toSet
  private val miner         = nodes.head

  val transfers = generateTransfersToRandomAddresses(maxTxsGroup, nodeAddresses)
  s"Block is limited by size after activation" in result(
    for {
      _                 <- Future.sequence((0 to maxGroups).map(_ => processRequests(transfers, includeAttachment = true)))
      _                 <- miner.waitForHeight(3)
      _                 <- Future.sequence((0 to maxGroups).map(_ => processRequests(transfers, includeAttachment = true)))
      blockHeaderBefore <- miner.blockHeadersAt(2)
      _                 <- miner.waitForHeight(4)
      blockHeaderAfter  <- miner.blockHeadersAt(3)
    } yield {
      val maxSizeInBytesAfterActivation = (1.1d * 1024 * 1024).toInt // including headers
      val blockSizeInBytesBefore        = blockHeaderBefore.blocksize
      blockSizeInBytesBefore should be > maxSizeInBytesAfterActivation

      val blockSizeInBytesAfter = blockHeaderAfter.blocksize
      blockSizeInBytesAfter should be <= maxSizeInBytesAfterActivation
    },
    10.minutes
  )

}

object BlockSizeConstraintsSuite {
  private val maxTxsGroup     = 500 // More, than 1mb of block
  private val maxGroups       = 9
  private val txsInMicroBlock = 500
  private val ConfigOverrides = ConfigFactory.parseString(s"""akka.http.server {
                                                             |  parsing.max-content-length = 3737439
                                                             |  request-timeout = 60s
                                                             |}
                                                             |
                                                             |waves {
                                                             |  network.enable-peers-exchange = no
                                                             |
                                                             |  miner {
                                                             |    quorum = 0
                                                             |    minimal-block-generation-offset = 60000ms
                                                             |    micro-block-interval = 1s
                                                             |    max-transactions-in-key-block = 0
                                                             |    max-transactions-in-micro-block = $txsInMicroBlock
                                                             |  }
                                                             |
                                                             |  blockchain.custom {
                                                             |    functionality {
                                                             |      feature-check-blocks-period = 1
                                                             |      blocks-for-feature-activation = 1
                                                             |
                                                             |      pre-activated-features {
                                                             |        2: 0
                                                             |        3: 2
                                                             |      }
                                                             |    }
                                                             |
                                                             |    store-transactions-in-state = false
                                                             |  }
                                                             |
                                                             |  features.supported = [2, 3]
                                                             |}""".stripMargin)

}
