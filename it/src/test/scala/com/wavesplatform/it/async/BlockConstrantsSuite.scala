package com.wavesplatform.it

import com.typesafe.config.Config
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import org.scalatest._

import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class BlockConstrantsSuite extends FreeSpec with Matchers with TransferSending with NodesFromDocker {

  private val maxTxs = 5000 // More, than 1mb of block
  private val txsInMicroBlock = 500

  override protected val nodeConfigs: Seq[Config] = NodeConfigs.newBuilder
    .overrideBase(_.raw(
      s"""akka.http.server {
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
         |    micro-block-interval = 3s
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
         |}""".stripMargin
    ))
    .withDefault(1)
    .build()

  private val nodeAddresses = nodeConfigs.map(_.getString("address")).toSet

  private def miner = nodes.head

  s"Block is limited by size after activation" in result(for {
    _ <- processRequests(generateTransfersToRandomAddresses(maxTxs, nodeAddresses), includeAttachment = true)
    _ <- miner.waitForHeight(3)
    _ <- processRequests(generateTransfersToRandomAddresses(maxTxs, nodeAddresses), includeAttachment = true)
    _ <- miner.waitForHeight(4)
    Seq(blockHeaderBefore, blockHeaderAfter) <- miner.blockHeadersSeq(2, 3)
  } yield {
    val maxSizeInBytesAfterActivation = (1.1d * 1024 * 1024).toInt // including headers
    val blockSizeInBytesBefore = blockHeaderBefore.blocksize
    blockSizeInBytesBefore should be > maxSizeInBytesAfterActivation

    val blockSizeInBytesAfter = blockHeaderAfter.blocksize
    blockSizeInBytesAfter should be <= maxSizeInBytesAfterActivation
  }, 6.minutes)

}
