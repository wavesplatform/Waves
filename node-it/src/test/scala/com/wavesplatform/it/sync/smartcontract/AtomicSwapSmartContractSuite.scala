package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.account.{AddressScheme, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.TransactionInfo
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.{Proofs, TxPositiveAmount}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer._
import org.scalatest.CancelAfterFailure

/*
Scenario:
1. Alice's and swapBC1's balances initialisation
2. Create and setup smart contract for swapBC1
3. Alice funds swapBC1t
4. Alice can't take money from swapBC1
5.1 Bob takes funds because he knows secret hash and 5.2 after rollback wait height and Alice takes funds back
 */

class AtomicSwapSmartContractSuite extends BaseTransactionSuite with CancelAfterFailure {

  /*
  One node because:
  1. There is an expected behavior of rollback and a possible issue with this. When the node are going rollback,
     there is a chance, it has a downloaded (but don't applied) extension. After the rollback, it applies an extension.
     This breaks the test.
  2. We have RollbackSuite
   */
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .buildNonConflicting()

  private lazy val BobBC1: KeyPair   = sender.createKeyPair()
  private lazy val AliceBC1: KeyPair = sender.createKeyPair()
  private lazy val swapBC1: KeyPair  = sender.createKeyPair()

  private val secretText = "some secret message from Alice"
  private val shaSecret  = "BN6RTYGWcwektQfSFzH8raYo9awaLgQ7pLyWLQY4S4F5"

  test("step1 - Balances initialization") {
    val toAliceBC1TxId = sender.transfer(sender.keyPair, AliceBC1.toAddress.toString, 10 * transferAmount, minFee).id
    nodes.waitForHeightAriseAndTxPresent(toAliceBC1TxId)

    val toSwapBC1TxId = sender.transfer(sender.keyPair, swapBC1.toAddress.toString, setScriptFee, minFee).id
    nodes.waitForHeightAriseAndTxPresent(toSwapBC1TxId)
  }

  test("step2 - Create and setup smart contract for swapBC1") {
    val beforeHeight = sender.height
    val scriptText   = s"""
    let Bob = Address(base58'${BobBC1.toAddress}')
    let Alice = Address(base58'${AliceBC1.toAddress}')

    match tx {
      case ttx: TransferTransaction =>
        let txToBob = (ttx.recipient == Bob) && (sha256(ttx.proofs[0]) == base58'$shaSecret') && ((20 + $beforeHeight) >= height)
        let backToAliceAfterHeight = ((height >= (21 + $beforeHeight)) && (ttx.recipient == Alice))

        txToBob || backToAliceAfterHeight
      case _ => false
    }""".stripMargin

    val pkSwapBC1 = swapBC1
    val script    = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1
    val sc1SetTx = SetScriptTransaction
      .selfSigned(1.toByte, sender = pkSwapBC1, script = Some(script), fee = setScriptFee, timestamp = System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(sc1SetTx.json())
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val swapBC1ScriptInfo = sender.addressScriptInfo(swapBC1.toAddress.toString)

    swapBC1ScriptInfo.script.isEmpty shouldBe false
    swapBC1ScriptInfo.scriptText.isEmpty shouldBe false
  }

  test("step3 - Alice makes transfer to swapBC1") {
    val txToSwapBC1 =
      TransferTransaction
        .selfSigned(
          version = 2.toByte,
          sender = AliceBC1,
          recipient = swapBC1.toAddress,
          asset = Waves,
          amount = transferAmount + setScriptFee + smartFee,
          feeAsset = Waves,
          fee = setScriptFee + smartFee,
          attachment = ByteStr.empty,
          timestamp = System.currentTimeMillis()
        )
        .explicitGet()

    val transferId = sender
      .signedBroadcast(txToSwapBC1.json())
      .id
    nodes.waitForHeightAriseAndTxPresent(transferId)
  }

  test("step4 - Alice cannot make transfer from swapBC1 if height is incorrect") {
    val txToSwapBC1 =
      TransferTransaction
        .selfSigned(
          version = 2.toByte,
          sender = swapBC1,
          recipient = AliceBC1.toAddress,
          asset = Waves,
          amount = transferAmount,
          feeAsset = Waves,
          fee = setScriptFee + smartFee,
          attachment = ByteStr.empty,
          timestamp = System.currentTimeMillis()
        )
        .explicitGet()

    assertApiErrorRaised(sender.signedBroadcast(txToSwapBC1.json()))
  }

  test("step5 - Bob makes transfer; after revert Alice takes funds back") {
    val height = nodes.height.max

    val (bobBalance, bobEffBalance)     = miner.accountBalances(BobBC1.toAddress.toString)
    val (aliceBalance, aliceEffBalance) = miner.accountBalances(AliceBC1.toAddress.toString)
    val (swapBalance, swapEffBalance)   = miner.accountBalances(swapBC1.toAddress.toString)

    val unsigned =
      TransferTransaction(
        version = 2.toByte,
        sender = swapBC1.publicKey,
        recipient = BobBC1.toAddress,
        assetId = Waves,
        amount = TxPositiveAmount.unsafeFrom(transferAmount),
        feeAssetId = Waves,
        fee = TxPositiveAmount.unsafeFrom(setScriptFee + smartFee),
        attachment = ByteStr.empty,
        timestamp = System.currentTimeMillis(),
        proofs = Proofs.empty,
        AddressScheme.current.chainId
      )

    val proof    = ByteStr(secretText.getBytes("UTF-8"))
    val sigAlice = crypto.sign(AliceBC1.privateKey, unsigned.bodyBytes())
    val signed   = unsigned.copy(proofs = Proofs(Seq(proof, sigAlice)))

    nodes.waitForHeightArise()
    val versionedTransferId = sender.signedBroadcast(signed.json()).id
    nodes.waitForHeightAriseAndTxPresent(versionedTransferId)

    miner.assertBalances(
      swapBC1.toAddress.toString,
      swapBalance - transferAmount - (setScriptFee + smartFee),
      swapEffBalance - transferAmount - (setScriptFee + smartFee)
    )
    miner.assertBalances(BobBC1.toAddress.toString, bobBalance + transferAmount, bobEffBalance + transferAmount)
    miner.assertBalances(AliceBC1.toAddress.toString, aliceBalance, aliceEffBalance)

    nodes.rollback(height, false)

    nodes.waitForHeight(height + 20)

    miner.accountBalances(swapBC1.toAddress.toString)
    assertBadRequestAndMessage(miner.transactionInfo[TransactionInfo](versionedTransferId), "transactions does not exist", 404)

    val selfSignedToAlice = TransferTransaction
      .selfSigned(
        version = 2.toByte,
        sender = swapBC1,
        recipient = AliceBC1.toAddress,
        asset = Waves,
        amount = transferAmount,
        feeAsset = Waves,
        fee = setScriptFee + smartFee,
        attachment = ByteStr.empty,
        timestamp = System.currentTimeMillis()
      )
      .explicitGet()

    val transferToAlice =
      sender.signedBroadcast(selfSignedToAlice.json()).id
    nodes.waitForHeightAriseAndTxPresent(transferToAlice)

    miner.assertBalances(
      swapBC1.toAddress.toString,
      swapBalance - transferAmount - (setScriptFee + smartFee),
      swapEffBalance - transferAmount - (setScriptFee + smartFee)
    )
    miner.assertBalances(BobBC1.toAddress.toString, bobBalance, bobEffBalance)
    miner.assertBalances(AliceBC1.toAddress.toString, aliceBalance + transferAmount, aliceEffBalance + transferAmount)
  }

}
