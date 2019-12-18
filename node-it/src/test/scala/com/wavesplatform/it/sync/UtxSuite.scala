package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.http.ApiError.CustomValidationError
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.Node
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.TransferTransactionV1
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.{CancelAfterFailure, FunSuite, Matchers}

import scala.concurrent.duration._
import scala.util.{Random, Try}

class UtxSuite extends FunSuite with CancelAfterFailure with NodesFromDocker with Matchers {
  override protected def nodeConfigs: Seq[Config] = UtxSuite.Configs

  val miner: Node    = nodes.head
  val notMiner: Node = nodes(1)

  val ENOUGH_FEE = 5000000
  val AMOUNT     = ENOUGH_FEE * 10

  test("Invalid transaction should be removed from from utx") {
    val seed = Array.fill(32)(-1: Byte)

    Random.nextBytes(seed)

    val account = KeyPair(seed)

    val transferToAccount = TransferTransactionV1
      .selfSigned(Waves, miner.privateKey, account, AMOUNT, System.currentTimeMillis(), Waves, ENOUGH_FEE, Array.emptyByteArray)
      .explicitGet()

    miner.signedBroadcast(transferToAccount.json())

    nodes.waitForHeightAriseAndTxPresent(transferToAccount.id().base58)

    val firstTransfer = TransferTransactionV1
      .selfSigned(Waves, account, miner.privateKey, AMOUNT - ENOUGH_FEE, System.currentTimeMillis(), Waves, ENOUGH_FEE, Array.emptyByteArray)
      .explicitGet()

    val secondTransfer = TransferTransactionV1
      .selfSigned(Waves, account, notMiner.privateKey, AMOUNT - ENOUGH_FEE, System.currentTimeMillis(), Waves, ENOUGH_FEE, Array.emptyByteArray)
      .explicitGet()

    val tx2Id = notMiner.signedBroadcast(secondTransfer.json()).id
    val tx1Id = miner.signedBroadcast(firstTransfer.json()).id

    waitForEmptyUtx(nodes)

    val exactlyOneTxInBlockchain =
      txInBlockchain(tx1Id, nodes) ^ txInBlockchain(tx2Id, nodes)

    assert(exactlyOneTxInBlockchain, "Only one tx should be in blockchain")
  }

  test("Transactions should be discarded if blockchain is not fully extended") {
    docker.stopContainer(dockerNodes()(1))
    miner.waitForHeight(miner.height + 3)
    docker.stopContainer(dockerNodes().head)
    docker.startContainer(dockerNodes()(1).containerId)

    val transferToAccount = TransferTransactionV1
      .selfSigned(Waves, miner.privateKey, notMiner.publicKey, AMOUNT, System.currentTimeMillis(), Waves, ENOUGH_FEE, Array.emptyByteArray)
      .explicitGet()

    assertApiError(notMiner.signedBroadcast(transferToAccount.json()), CustomValidationError("Transaction is rejected because blockchain is stale"))

    notMiner.utxSize shouldBe 0

    docker.startContainer(dockerNodes().head.containerId)
    nodes.waitForSameBlockHeadersAt(miner.height)

    notMiner.signedBroadcast(transferToAccount.json())

    notMiner.utxSize shouldBe 1

    nodes.waitForHeightAriseAndTxPresent(transferToAccount.id().toString)
  }

  def waitForEmptyUtx(nodes: Seq[Node]): Unit = {
    implicit val sch: Scheduler = monix.execution.Scheduler.global

    def loop(): Task[Unit] = {
      val utxIds = nodes.map(_.utx.size)

      if (utxIds.sum != 0) {
        Task
          .sleep(1.second)
          .flatMap(_ => loop())
      } else {
        Task.pure(())
      }
    }

    loop().runSyncUnsafe(6.minutes)
  }

  def txInBlockchain(txId: String, nodes: Seq[Node]): Boolean = {
    nodes.forall { node =>
      Try(node.transactionInfo(txId)).isSuccess
    }
  }
}

object UtxSuite {
  import com.wavesplatform.it.NodeConfigs._
  private val minerConfig = ConfigFactory.parseString(s"""
                                                         |waves {
                                                         |  synchronization.synchronization-timeout = 10s
                                                         |  blockchain.custom.functionality {
                                                         |    pre-activated-features.1 = 0
                                                         |    generation-balance-depth-from-50-to-1000-after-height = 100
                                                         |  }
                                                         |  miner.quorum = 0
                                                         |}""".stripMargin)

  private val notMinerConfig = ConfigFactory.parseString(s"""
                                                            |waves {
                                                            |  synchronization.synchronization-timeout = 10s
                                                            |  blockchain.custom.functionality {
                                                            |    pre-activated-features.1 = 0
                                                            |    generation-balance-depth-from-50-to-1000-after-height = 100
                                                            |    reject-transactions-when-blockchain-is-stale = yes
                                                            |  }
                                                            |  miner.enable = no
                                                            |}""".stripMargin)

  val Configs: Seq[Config] = Seq(
    minerConfig.withFallback(Default.head),
    notMinerConfig.withFallback(Default(1))
  )

}
