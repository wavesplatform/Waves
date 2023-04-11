package com.wavesplatform.events

import com.wavesplatform.events.FakeObserver.{E, UpdatesRepoExt}
import com.wavesplatform.events.api.grpc.protobuf.SubscribeRequest
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.DomainPresets.RideV6
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.TxHelpers.defaultAddress
import org.scalatest.concurrent.ScalaFutures

class BlockchainUpdatesSubscribeSpec extends FreeSpec with WithBUDomain with ScalaFutures {
  val currentSettings: WavesSettings = RideV6

  "return correct data for alias tx" in withDomainAndRepo(currentSettings) { case (d, repo) =>
    d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress))

    val testAlias     = "test"
    val balanceBefore = d.balance(defaultAddress)
    val alias         = TxHelpers.createAlias(testAlias)

    d.appendBlock(alias)

    val balanceAfter = d.balance(defaultAddress)

    val subscription                             = repo.createFakeObserver(SubscribeRequest.of(1, 2))
    val subscribeAppend                          = subscription.values.head.update.get.getAppend
    val subscribeTx                              = subscribeAppend.getBlock.getBlock.transactions
    val subscribeTxId                            = subscribeAppend.transactionIds.head.toByteArray
    val subscribeTransactionsMetadata            = subscribeAppend.transactionsMetadata
    val subscribeTransactionStateUpdates         = subscribeAppend.transactionStateUpdates
    val subscribeTransactionStateUpdatesBalances = subscribeTransactionStateUpdates.head.balances.head

    if (subscription.completed) {
      subscribeTx.head.transaction.wavesTransaction.get.getFee.amount shouldEqual alias.fee.value
      subscribeTx.head.transaction.wavesTransaction.get.getCreateAlias.alias shouldBe testAlias
      subscribeTxId shouldBe alias.id.apply().arr
      subscribeTransactionsMetadata.head.senderAddress.toByteArray shouldBe alias.sender.toAddress.bytes
      subscribeTransactionStateUpdatesBalances.address.toByteArray shouldBe alias.sender.toAddress.toString
      subscribeTransactionStateUpdatesBalances.amountBefore shouldEqual balanceBefore
      subscribeTransactionStateUpdatesBalances.amountAfter shouldEqual balanceAfter
    }
  }
}
