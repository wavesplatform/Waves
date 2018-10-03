package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.it.Node
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.state._
import com.wavesplatform.transaction.transfer.TransferTransactionV1
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.{CancelAfterFailure, FunSuite, Matchers}

import scala.concurrent.duration._
import scala.util.Random

class UtxSuite extends FunSuite with CancelAfterFailure with NodesFromDocker with Matchers {
  override protected def nodeConfigs: Seq[Config] = UtxSuite.Configs

  val miner: Node    = nodes.head
  val notMiner: Node = nodes(1)

  val ENOUGH_FEE = 5000000
  val AMOUNT     = ENOUGH_FEE * 10

  test("Invalid transaction should be removed from from utx") {
    val seed = Array.fill(32)(-1: Byte)

    Random.nextBytes(seed)

    val account = PrivateKeyAccount(seed)

    val transferToAccount = TransferTransactionV1
      .selfSigned(None, miner.privateKey, account, AMOUNT, System.currentTimeMillis(), None, ENOUGH_FEE, Array.emptyByteArray)
      .explicitGet()

    miner.signedBroadcast(transferToAccount.json())

    nodes.waitForHeightAriseAndTxPresent(transferToAccount.id().base58)

    val validTx = TransferTransactionV1
      .selfSigned(None, account, miner.privateKey, AMOUNT - ENOUGH_FEE, System.currentTimeMillis(), None, ENOUGH_FEE, Array.emptyByteArray)
      .explicitGet()

    val invalidTx = TransferTransactionV1
      .selfSigned(None, account, notMiner.privateKey, AMOUNT - ENOUGH_FEE, System.currentTimeMillis(), None, ENOUGH_FEE, Array.emptyByteArray)
      .explicitGet()

    miner.signedBroadcast(validTx.json())
    notMiner.signedBroadcast(invalidTx.json())

    assert((notMiner.utx map (_.id)) contains invalidTx.id().base58)

    nodes.waitForHeightAriseAndTxPresent(validTx.id().base58)

    waitForTxDisappearFromUtx(invalidTx.id().base58, notMiner, 6.minutes)
  }

  def waitForTxDisappearFromUtx(txId: String, node: Node, timeout: FiniteDuration): Unit = {

    implicit val sch: Scheduler = monix.execution.Scheduler.global

    def loop(): Task[Unit] = {
      val utxIds = node.utx.map(_.id)

      if (utxIds contains txId) {
        Task
          .sleep(1.second)
          .flatMap(_ => loop())
      } else {
        Task.pure(())
      }
    }

    loop().runSyncUnsafe(timeout)
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
                                                            |  }
                                                            |  miner.enable = no
                                                            |}""".stripMargin)

  val Configs: Seq[Config] = Seq(
    minerConfig.withFallback(Default.head),
    notMinerConfig.withFallback(Default(1)),
  )

}
