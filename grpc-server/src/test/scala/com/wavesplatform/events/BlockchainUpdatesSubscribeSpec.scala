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
    d.appendKeyBlock()

    val testAlias     = "test"
    val balanceBefore = d.balance(defaultAddress)
    val alias         = TxHelpers.createAlias(testAlias)
    val subscription  = repo.createFakeObserver(SubscribeRequest(1))

    d.appendMicroBlock(alias)

    val balanceAfter                    = d.balance(defaultAddress)
    val append                          = subscription.values.head.getUpdate.getAppend
    val tx                              = append.getMicroBlock.getMicroBlock.getMicroBlock.transactions
    val txId                            = append.transactionIds.head.toByteArray
    val transactionsMetadata            = append.transactionsMetadata
    val transactionStateUpdates         = append.transactionStateUpdates
    val transactionStateUpdatesBalances = transactionStateUpdates.head.balances.head

    tx.head.transaction.wavesTransaction.get.getFee.amount shouldEqual alias.fee.value
    tx.head.transaction.wavesTransaction.get.getCreateAlias.alias shouldBe testAlias
    txId shouldBe alias.id.apply().arr
    transactionsMetadata.head.senderAddress.toByteArray shouldBe alias.sender.toAddress.bytes
    transactionStateUpdatesBalances.address.toByteArray shouldBe alias.sender.toAddress.bytes
    transactionStateUpdatesBalances.amountBefore shouldEqual balanceBefore
    transactionStateUpdatesBalances.amountAfter shouldEqual balanceAfter
  }
}
