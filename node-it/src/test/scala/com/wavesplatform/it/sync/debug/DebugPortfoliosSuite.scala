package com.wavesplatform.it.sync.debug

import com.typesafe.config.Config
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.transactions.OverflowBlock
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{BaseFunSuite, NodeConfigs}
import com.wavesplatform.test._

class DebugPortfoliosSuite extends BaseFunSuite with NodesFromDocker with OverflowBlock {
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(entitiesNumber = 1)
      .buildNonConflicting()

  private lazy val firstAcc  = sender.createKeyPair()
  private lazy val secondAcc = sender.createKeyPair()

  private lazy val firstAddress: String  = firstAcc.toAddress.toString
  private lazy val secondAddress: String = secondAcc.toAddress.toString

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    sender.transfer(sender.keyPair, firstAddress, 20.waves, minFee, waitForTx = true)
    sender.transfer(sender.keyPair, secondAddress, 20.waves, minFee, waitForTx = true)
  }

  test("getting a balance considering pessimistic transactions from UTX pool - changed after UTX") {
    val portfolioBefore = sender.debugPortfoliosFor(firstAddress, considerUnspent = true)
    val utxSizeBefore   = sender.utxSize

    sender.transfer(firstAcc, secondAddress, 5.waves, 5.waves)
    sender.transfer(secondAcc, firstAddress, 7.waves, 5.waves)

    sender.waitForUtxIncreased(utxSizeBefore)

    val portfolioAfter = sender.debugPortfoliosFor(firstAddress, considerUnspent = true)

    val expectedBalance = portfolioBefore.balance - 10.waves // withdraw + fee
    assert(portfolioAfter.balance == expectedBalance)

  }

  test("getting a balance without pessimistic transactions from UTX pool - not changed after UTX") {
    nodes.waitForHeightArise()
    overflowBlock()

    val portfolioBefore = sender.debugPortfoliosFor(firstAddress, considerUnspent = false)
    val utxSizeBefore   = sender.utxSize

    sender.transfer(firstAcc, secondAddress, 5.waves, fee = 5.waves)
    sender.waitForUtxIncreased(utxSizeBefore)

    val portfolioAfter = sender.debugPortfoliosFor(firstAddress, considerUnspent = false)
    assert(portfolioAfter.balance == portfolioBefore.balance)
  }
}
