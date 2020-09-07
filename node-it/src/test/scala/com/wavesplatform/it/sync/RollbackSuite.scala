package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.state.{BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{CancelAfterFailure, FunSuite, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Random

@LoadTest
class RollbackSuite
    extends FunSuite
    with CancelAfterFailure
    with TransferSending
    with NodesFromDocker
    with ReportingTestName
    with Matchers
    with TableDrivenPropertyChecks {
  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((14, 1000000)))
      .withDefault(1)
      .withSpecial(1, _.nonMiner)
      .buildNonConflicting()

  private val nodeAddresses        = nodeConfigs.map(_.getString("address")).toSet
  private def sender: Node         = nodes.last
  private def miner: Node          = nodes.head
  private def firstAddress: String = sender.address

  test("Apply the same transfer transactions twice with return to UTX") {

    val startHeight = sender.height

    Await.result(processRequests(generateTransfersToRandomAddresses(190, nodeAddresses)), 2.minutes)
    nodes.waitFor("empty utx")(_.utxSize)(_.forall(_ == 0))
    nodes.waitForHeightArise()

    val stateHeight        = sender.height
    val stateAfterFirstTry = nodes.head.debugStateAt(stateHeight)

    nodes.rollback(startHeight)
    nodes.waitFor("empty utx")(_.utxSize)(_.forall(_ == 0))
    nodes.waitForHeightArise()

    val stateAfterSecondTry = nodes.head.debugStateAt(stateHeight)
    stateAfterSecondTry.toSet shouldBe stateAfterFirstTry.toSet
  }

  test("Just rollback transactions") {
    nodes.waitForHeightArise() // so that NG fees won't affect miner's balances
    val startHeight      = sender.height
    val stateBeforeApply = sender.debugStateAt(startHeight)

    nodes.waitForHeightArise()

    val requests = generateTransfersToRandomAddresses(190, nodeAddresses)
    Await.result(processRequests(requests), 2.minutes)

    nodes.waitFor("empty utx")(_.utxSize)(_.forall(_ == 0))

    nodes.waitForHeightArise()

    sender.debugStateAt(sender.height).size shouldBe stateBeforeApply.size + 190

    nodes.rollback(startHeight, returnToUTX = false)

    nodes.waitFor("empty utx")(_.utxSize)(_.forall(_ == 0))

    nodes.waitForHeightArise()

    val stateAfterApply = sender.debugStateAt(sender.height)

    stateAfterApply should contain theSameElementsAs stateBeforeApply

  }

  test("Alias transaction rollback should work fine") {
    val alias = "test_alias4"

    val aliasTxId = sender.createAlias(sender.keyPair, alias, transferAmount).id
    nodes.waitForHeightAriseAndTxPresent(aliasTxId)

    val txsBefore = sender.transactionsByAddress(firstAddress, 10)

    val txHeight = sender.waitForTransaction(aliasTxId).height

    nodes.rollback(txHeight - 1, returnToUTX = false)
    nodes.waitForHeight(txHeight + 1)

    val secondAliasTxId = sender.createAlias(sender.keyPair, alias, transferAmount).id
    nodes.waitForHeightAriseAndTxPresent(secondAliasTxId)
    sender.transactionsByAddress(firstAddress, 10) shouldNot contain theSameElementsAs txsBefore

  }

  test("Data transaction rollback") {
    val node       = nodes.head
    val entry1     = IntegerDataEntry("1", 0)
    val entry2     = BooleanDataEntry("2", value = true)
    val entry3     = IntegerDataEntry("1", 1)
    val txsBefore0 = sender.transactionsByAddress(firstAddress, 10)

    val tx1 = sender.putData(sender.keyPair, List(entry1), calcDataFee(List(entry1), TxVersion.V1)).id
    nodes.waitForHeightAriseAndTxPresent(tx1)
    val txsBefore1 = sender.transactionsByAddress(firstAddress, 10)

    val tx1height = sender.waitForTransaction(tx1).height

    val tx2 = sender.putData(sender.keyPair, List(entry2, entry3), calcDataFee(List(entry2, entry3), TxVersion.V1)).id
    nodes.waitForHeightAriseAndTxPresent(tx2)

    val data2 = sender.getData(firstAddress)
    assert(data2 == List(entry3, entry2))

    nodes.rollback(tx1height, returnToUTX = false)
    nodes.waitForSameBlockHeadersAt(tx1height)

    val data1 = node.getData(firstAddress)
    assert(data1 == List(entry1))
    sender.transactionsByAddress(firstAddress, 10) should contain theSameElementsAs txsBefore1

    nodes.rollback(tx1height - 1, returnToUTX = false)
    nodes.waitForSameBlockHeadersAt(tx1height - 1)

    val data0 = node.getData(firstAddress)
    assert(data0 == List.empty)
    sender.transactionsByAddress(firstAddress, 10) should contain theSameElementsAs txsBefore0
  }

  test("Sponsorship transaction rollback") {
    val sponsorAssetTotal = 100 * 100L

    val sponsorAssetId =
      sender
        .issue(sender.keyPair, "SponsoredAsset", "For test usage", sponsorAssetTotal, reissuable = false, fee = issueFee)
        .id
    nodes.waitForHeightAriseAndTxPresent(sponsorAssetId)

    val sponsorId = sender.sponsorAsset(sender.keyPair, sponsorAssetId, baseFee = 100L, fee = issueFee).id
    nodes.waitForHeightAriseAndTxPresent(sponsorId)

    val height     = sender.waitForTransaction(sponsorId).height
    val txsBefore1 = sender.transactionsByAddress(firstAddress, 10)

    val assetDetailsBefore = sender.assetsDetails(sponsorAssetId)

    nodes.waitForHeightArise()
    val sponsorSecondId = sender.sponsorAsset(sender.keyPair, sponsorAssetId, baseFee = 2 * 100L, fee = issueFee).id
    nodes.waitForHeightAriseAndTxPresent(sponsorSecondId)

    nodes.rollback(height, returnToUTX = false)

    nodes.waitForHeightArise()

    val assetDetailsAfter = sender.assetsDetails(sponsorAssetId)

    assert(assetDetailsAfter.minSponsoredAssetFee == assetDetailsBefore.minSponsoredAssetFee)
    sender.transactionsByAddress(sender.address, 10) should contain theSameElementsAs txsBefore1
  }

  test("transfer depends from data tx") {
    val scriptText = s"""
    match tx {
      case tx: TransferTransaction =>
        let oracle = addressFromRecipient(tx.recipient)
        extract(getString(oracle,"oracle")) == "yes"
      case _: SetScriptTransaction | DataTransaction => true
      case _ => false
    }""".stripMargin

    val pkSwapBC1 = KeyPair.fromSeed(sender.seed(firstAddress)).explicitGet()
    val script    = ScriptCompiler(scriptText, isAssetScript = false, ScriptEstimatorV2).explicitGet()._1
    val sc1SetTx = SetScriptTransaction
      .selfSigned(1.toByte, sender = pkSwapBC1, script = Some(script), fee = setScriptFee, timestamp = System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender.signedBroadcast(sc1SetTx.json()).id
    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val height = nodes.waitForHeightArise()

    nodes.waitForHeightArise()
    val entry1 = StringDataEntry("oracle", "yes")
    val dtx    = sender.putData(sender.keyPair, List(entry1), calcDataFee(List(entry1), TxVersion.V1) + smartFee).id
    nodes.waitForHeightAriseAndTxPresent(dtx)

    val tx = sender.transfer(sender.keyPair, firstAddress, transferAmount, smartMinFee, waitForTx = true).id
    nodes.waitForHeightAriseAndTxPresent(tx)

    //as rollback is too fast, we should blacklist nodes from each other before rollback
    sender.blacklist(miner.networkAddress)
    miner.blacklist(sender.networkAddress)
    nodes.rollback(height)
    sender.connect(miner.networkAddress)
    miner.connect(sender.networkAddress)

    nodes.waitForSameBlockHeadersAt(height)

    nodes.waitForHeightArise()

    assert(sender.findTransactionInfo(dtx).isDefined)
    assert(sender.findTransactionInfo(tx).isDefined)

  }

  forAll(
    Table(
      ("num", "name"),
      (1, "1 of N"),
      (nodes.size, "N of N")
    )
  ) { (num, name) =>
    test(s"generate more blocks and resynchronise after rollback $name") {
      val baseHeight = nodes.map(_.height).max + 5
      nodes.waitForHeight(baseHeight)
      val rollbackNodes = Random.shuffle(nodes).take(num)
      rollbackNodes.foreach(_.rollback(baseHeight - 1))
      nodes.waitForHeightArise()
      nodes.waitForSameBlockHeadersAt(baseHeight)
    }
  }
}
