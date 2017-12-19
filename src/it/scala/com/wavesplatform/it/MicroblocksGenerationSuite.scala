package com.wavesplatform.it

import com.typesafe.config.ConfigFactory
import com.wavesplatform.it.api._
import org.scalatest._

import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

class MicroblocksGenerationSuite extends FreeSpec with BeforeAndAfterAll
  with Matchers with TransferSending with MultipleNodesApi {

  private val txsInMicroBlock = 200
  private val maxTxs = 2000

  private val config = ConfigFactory
    .parseString(
      s"""waves {
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
         |  blockchain {
         |    custom {
         |      average-block-delay: 60000ms
         |      initial-base-target: 100
         |      timestamp: 1489352400000
         |      block-timestamp: 1489352400000
         |      signature: "4zkuYsyYUS2XWTuYk2UHGbkfqufUnWUiataeKBnEL4BoXGxk2Cu1ZkEFqckMyVgsXjXxL32FMgZ2Y1MPaJNsGjdc"
         |      initial-balance: 1000000000000000
         |      transactions = [
         |        {recipient: "3HPG313x548Z9kJa5XY4LVMLnUuF77chcnG", amount: 50000000000000},
         |        {recipient: "3Hm3LGoNPmw1VTZ3eRA2pAfeQPhnaBm6YFC", amount: 50000000000000}
         |      ]
         |    }
         |
         |    functionality {
         |      pre-activated-features {
         |        2: 0
         |      }
         |    }
         |
         |    store-transactions-in-state = false
         |  }
         |
         |  features.supported = [2]
         |}""".stripMargin)
    .withFallback(NodeConfigs.Default.head)

  lazy val docker: Docker = Docker(getClass)
  lazy val nodes: Seq[Node] = docker.startNodes(Seq(config))
  private lazy val miner = nodes.head

  s"Generate transactions and wait for one block with $maxTxs txs" in result(for {
    richAccountsWithBalance <- Future.traverse(nodes)(balanceForNode).map(_.toMap)
    _ <- processRequests(generateTransfersToRandomAddresses(maxTxs, richAccountsWithBalance))
    _ <- miner.waitForHeight(3)
    block <- miner.blockAt(2)
  } yield block.transactions.size shouldBe maxTxs, 3.minutes)

  override def beforeAll(): Unit = {
    super.beforeAll()
    log.debug(s"There are ${nodes.size} in tests") // Initializing of a lazy variable
  }

  override def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }

}
