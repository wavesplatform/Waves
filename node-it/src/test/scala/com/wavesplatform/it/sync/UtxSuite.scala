package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.Node
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.TransactionInfo
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.TransferTransaction
import org.scalatest.{CancelAfterFailure, FunSuite, Matchers}

import scala.util.{Random, Try}

class UtxSuite extends FunSuite with CancelAfterFailure with NodesFromDocker with Matchers {
  private val miner: Node    = nodes.head
  private val notMiner: Node = nodes(1)

  private var whitelistedAccount: KeyPair     = _
  private var whitelistedDAppAccount: KeyPair = _

  private val ENOUGH_FEE = 5000000
  private val AMOUNT     = ENOUGH_FEE * 10

  test("Invalid transaction should be removed from from utx") {
    val account = UtxSuite.createAccount

    val transferToAccount = TransferTransaction
      .selfSigned(1.toByte, miner.keyPair, account.toAddress, Waves, AMOUNT, Waves, ENOUGH_FEE, ByteStr.empty, System.currentTimeMillis())
      .explicitGet()

    miner.signedBroadcast(transferToAccount.json())

    nodes.waitForHeightAriseAndTxPresent(transferToAccount.id().toString)

    val firstTransfer = TransferTransaction
      .selfSigned(
        1.toByte,
        account,
        miner.keyPair.toAddress,
        Waves,
        AMOUNT - ENOUGH_FEE,
        Waves,
        ENOUGH_FEE,
        ByteStr.empty,
        System.currentTimeMillis()
      )
      .explicitGet()

    val secondTransfer = TransferTransaction
      .selfSigned(
        1.toByte,
        account,
        notMiner.keyPair.toAddress,
        Waves,
        AMOUNT - ENOUGH_FEE,
        Waves,
        ENOUGH_FEE,
        ByteStr.empty,
        System.currentTimeMillis()
      )
      .explicitGet()

    val tx2Id = notMiner.signedBroadcast(secondTransfer.json()).id
    val tx1Id = miner.signedBroadcast(firstTransfer.json()).id

    nodes.waitFor("empty utx")(_.utxSize)(_.forall(_ == 0))

    val exactlyOneTxInBlockchain =
      txInBlockchain(tx1Id, nodes) ^ txInBlockchain(tx2Id, nodes)

    assert(exactlyOneTxInBlockchain, "Only one tx should be in blockchain")
  }

  test("Whitelisted transactions should be mined first of all") {
    val minTransferFee  = 100000L
    val minInvokeFee    = 500000L
    val minSetScriptFee = 100000000L
    val higherFee       = minInvokeFee * 2

    val invokeAccount = UtxSuite.createAccount

    def time: Long = System.currentTimeMillis()

    val whitelistedAccountTransfer =
      TransferTransaction
        .selfSigned(
          TxVersion.V1,
          miner.keyPair,
          whitelistedAccount.toAddress,
          Waves,
          5 * minTransferFee + 5,
          Waves,
          minTransferFee,
          ByteStr.empty,
          time
        )
        .explicitGet()
    val whitelistedDAppAccountTransfer =
      TransferTransaction
        .selfSigned(
          TxVersion.V1,
          miner.keyPair,
          whitelistedDAppAccount.toAddress,
          Waves,
          minSetScriptFee,
          Waves,
          minTransferFee,
          ByteStr.empty,
          time
        )
        .explicitGet()
    val invokeAccountTransfer = TransferTransaction
      .selfSigned(
        TxVersion.V1,
        miner.keyPair,
        invokeAccount.toAddress,
        Waves,
        5 * minInvokeFee,
        Waves,
        minTransferFee,
        ByteStr.empty,
        time
      )
      .explicitGet()

    Seq(whitelistedAccountTransfer, whitelistedDAppAccountTransfer, invokeAccountTransfer)
      .map(tx => miner.signedBroadcast(tx.json()).id)
      .foreach(nodes.waitForTransaction)

    val scriptText =
      """
        |{-# STDLIB_VERSION 3 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |@Callable(i)
        |func default() = { WriteSet([DataEntry("0", true)]) }
        |""".stripMargin
    val script    = ScriptCompiler.compile(scriptText, ScriptEstimatorV1).explicitGet()._1
    val setScript = SetScriptTransaction.selfSigned(TxVersion.V1, whitelistedDAppAccount, Some(script), minSetScriptFee, time).explicitGet()
    miner.signedBroadcast(setScript.json())
    nodes.waitForHeightAriseAndTxPresent(setScript.id().toString)

    val txs = (1 to 5000).map { _ =>
      TransferTransaction
        .selfSigned(TxVersion.V1, miner.keyPair, UtxSuite.createAccount.toAddress, Waves, 1L, Waves, higherFee, ByteStr.empty, time)
        .explicitGet()
    }

    val whitelistedTxs = {
      val bySender = (1 to 5).map { _ =>
        TransferTransaction
          .selfSigned(TxVersion.V1, whitelistedAccount, UtxSuite.createAccount.toAddress, Waves, 1L, Waves, minTransferFee, ByteStr.empty, time)
          .explicitGet()
      }
      val byDApp = (1 to 5).map { _ =>
        InvokeScriptTransaction
          .selfSigned(TxVersion.V1, invokeAccount, whitelistedDAppAccount.toAddress, None, Seq.empty, minInvokeFee, Waves, extraFeePerStep = InvokeScriptTransaction.DefaultExtraFeePerStep, timestamp = time)
          .explicitGet()
      }
      Random.shuffle(bySender ++ byDApp)
    }

    txs.foreach(tx => miner.signedBroadcast(tx.json()))

    miner.utxSize should be > 0

    whitelistedTxs.map(tx => miner.signedBroadcast(tx.json()).id).foreach(nodes.waitForTransaction)

    miner.utxSize should be > 0
  }

  def txInBlockchain(txId: String, nodes: Seq[Node]): Boolean = {
    nodes.forall { node =>
      Try(node.transactionInfo[TransactionInfo](txId)).isSuccess
    }
  }

  override protected def nodeConfigs: Seq[Config] = {
    import UtxSuite._
    import com.wavesplatform.it.NodeConfigs._

    whitelistedAccount = createAccount
    whitelistedDAppAccount = createAccount

    val whitelist = Seq(whitelistedAccount, whitelistedDAppAccount).map(_.toAddress.stringRepr)

    val minerConfig    = ConfigFactory.parseString(UtxSuite.minerConfigPredef(whitelist))
    val notMinerConfig = ConfigFactory.parseString(UtxSuite.notMinerConfigPredef(whitelist))

    Seq(
      minerConfig.withFallback(Default.head),
      notMinerConfig.withFallback(Default(1))
    )
  }
}

object UtxSuite {
  private def createAccount = {
    val seed = Array.fill(32)(-1: Byte)
    Random.nextBytes(seed)
    KeyPair(seed)
  }

  private def minerConfigPredef(whitelist: Seq[String]) =
    s"""
       |waves {
       |  synchronization.synchronization-timeout = 10s
       |  utx {
       |    max-size = 5000
       |    fast-lane-addresses = [${whitelist.mkString(",")}]
       |  }
       |  blockchain.custom.functionality {
       |    pre-activated-features.1 = 0
       |    generation-balance-depth-from-50-to-1000-after-height = 100
       |  }
       |  miner.quorum = 0
       |}""".stripMargin

  private def notMinerConfigPredef(whitelist: Seq[String]) =
    s"""
       |waves {
       |  synchronization.synchronization-timeout = 10s
       |  utx {
       |    max-size = 5000
       |    fast-lane-addresses = [${whitelist.mkString(",")}]
       |  }
       |  blockchain.custom.functionality {
       |    pre-activated-features.1 = 0
       |    generation-balance-depth-from-50-to-1000-after-height = 100
       |  }
       |  miner.enable = no
       |}""".stripMargin
}
