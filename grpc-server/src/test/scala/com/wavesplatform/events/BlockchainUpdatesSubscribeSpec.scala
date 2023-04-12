package com.wavesplatform.events

import com.wavesplatform.events.FakeObserver.{E, UpdatesRepoExt}
import com.wavesplatform.events.api.grpc.protobuf.SubscribeRequest
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.DomainPresets.RideV6
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.{CreateAliasTransaction, TxHelpers}
import com.wavesplatform.transaction.TxHelpers.{secondAddress, secondSigner, signer}
import org.scalatest.concurrent.ScalaFutures

class BlockchainUpdatesSubscribeSpec extends FreeSpec with WithBUDomain with ScalaFutures {
  val currentSettings: WavesSettings = RideV6
  val testAlias                      = "test"
  var balanceBefore                  = 0L
  var balanceAfter                   = 0L

  "return correct data for alias tx" in withDomainAndRepo(currentSettings) { case (d, repo) =>
    d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress))
    d.appendKeyBlock()
    d.appendBlock(TxHelpers.transfer())

    balanceBefore = d.balance(secondAddress)
    val alias        = TxHelpers.createAlias(testAlias, secondSigner)
    val subscription = repo.createFakeObserver(SubscribeRequest(2))
    d.appendMicroBlock(alias)
    val append = subscription.fetchAllEvents(d.blockchain).map(_.getUpdate.getAppend).last

    balanceAfter = balanceBefore - alias.fee.value
    assertions(append, alias)
  }

  // TODO - Доработать тест, сейчас нужно научиться создавать dApp account
  "return correct data for alias tx for smart account" in withDomainAndRepo(currentSettings) { case (d, repo) =>
    d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress))
    d.appendKeyBlock()
    d.appendBlock(TxHelpers.transfer())

    val script = TxHelpers.script(
      """
        |{-# STDLIB_VERSION 5 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        |@Callable(inv)
        |func foo() = {
        |  strict ii = invoke(this, "bar", [1], [])
        |  [IntegerEntry("test1", 1)]
        |}
        |
        |@Callable(inv)
        |func bar(i: Int) = [IntegerEntry("test", 2)]
        |""".stripMargin)

    d.appendBlock(TxHelpers.setScript(secondSigner, script))

    balanceBefore = d.balance(secondAddress)
    val alias        = TxHelpers.createAlias(testAlias, secondSigner)
    val subscription = repo.createFakeObserver(SubscribeRequest(2))
    d.appendMicroBlock(alias)
    val append = subscription.fetchAllEvents(d.blockchain).map(_.getUpdate.getAppend).last

    balanceAfter = balanceBefore - alias.fee.value
    assertions(append, alias)
  }

  def assertions(append: Append, alias: CreateAliasTransaction): Unit = {
    val tx                              = append.getMicroBlock.getMicroBlock.getMicroBlock.transactions
    val txId                            = append.transactionIds.head.toByteArray
    val transactionsMetadata            = append.transactionsMetadata
    val transactionStateUpdates         = append.transactionStateUpdates
    val transactionStateUpdatesBalances = transactionStateUpdates.head.balances.head

    tx.head.transaction.wavesTransaction.get.getFee.amount shouldEqual alias.fee.value
    tx.head.transaction.wavesTransaction.get.getCreateAlias.alias shouldBe testAlias
    txId shouldBe alias.id.apply().arr
    transactionsMetadata.head.senderAddress.toByteArray shouldBe secondAddress.bytes
    transactionStateUpdatesBalances.address.toByteArray shouldBe secondAddress.bytes
    transactionStateUpdatesBalances.amountBefore shouldEqual balanceBefore
    transactionStateUpdatesBalances.amountAfter.get.amount shouldEqual balanceAfter
    transactionStateUpdatesBalances.amountAfter.get.assetId.isEmpty shouldBe true
  }
}
