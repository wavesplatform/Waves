package com.wavesplatform.events

import com.wavesplatform.account.Address
import com.wavesplatform.events.FakeObserver.{E, UpdatesRepoExt}
import com.wavesplatform.events.api.grpc.protobuf.SubscribeRequest
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.DomainPresets.RideV6
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.TxHelpers.{secondAddress, secondSigner}
import org.scalatest.concurrent.ScalaFutures

class BlockchainUpdatesSubscribeSpec extends FreeSpec with WithBUDomain with ScalaFutures {
  val currentSettings: WavesSettings = RideV6
  val testAlias                      = "test"
  var amount                         = 1000
  var senderBalanceBefore            = 0L
  var senderBalanceAfter             = 0L
  var recipientBalanceBefore         = 0L
  var recipientBalanceAfter          = 0L
  val emptyAddressBytes: Array[Byte] = "".getBytes()

  "BlockchainUpdates subscribe tests" - {
    val recipient        = TxHelpers.signer(1234)
    val recipientAddress = recipient.toAddress

    "return correct data for alias tx" in withDomainAndRepo(currentSettings) { case (d, repo) =>
      d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress))
      d.appendKeyBlock()
      d.appendBlock(TxHelpers.transfer())

      senderBalanceBefore = d.balance(secondAddress)

      val alias        = TxHelpers.createAlias(testAlias, secondSigner)
      val subscription = repo.createFakeObserver(SubscribeRequest(2))
      d.appendMicroBlock(alias)
      val append = subscription.fetchAllEvents(d.blockchain).map(_.getUpdate.getAppend).last

      senderBalanceAfter = senderBalanceBefore - alias.fee.value

      val tx   = append.getMicroBlock.getMicroBlock.getMicroBlock.transactions.head.transaction.wavesTransaction.get
      val txId = append.transactionIds.head.toByteArray

      tx.getFee.amount shouldEqual alias.fee.value
      tx.getCreateAlias.alias shouldBe testAlias
      txId shouldBe alias.id.apply().arr

      assertionsTransactionsMetadata(append, emptyAddressBytes)
      assertionsTransactionStateUpdatesBalances(append, 0, 0, secondAddress, senderBalanceBefore, senderBalanceAfter, "")
    }

    "return correct data for transfer tx" in withDomainAndRepo(currentSettings) { case (d, repo) =>
      d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress))
      d.appendKeyBlock()
      d.appendBlock(TxHelpers.transfer())

      senderBalanceBefore = d.balance(secondAddress)

      val transfer     = TxHelpers.transfer(secondSigner, recipientAddress, amount)
      val subscription = repo.createFakeObserver(SubscribeRequest(2))
      d.appendMicroBlock(transfer)
      val append = subscription.fetchAllEvents(d.blockchain).map(_.getUpdate.getAppend).last

      senderBalanceAfter = senderBalanceBefore - transfer.fee.value - amount
      recipientBalanceAfter = recipientBalanceBefore + amount

      val tx   = append.getMicroBlock.getMicroBlock.getMicroBlock.transactions.head.transaction.wavesTransaction.get
      val txId = append.transactionIds.head.toByteArray

      tx.getFee.amount shouldEqual transfer.fee.value
      tx.getTransfer.amount.get.amount shouldBe transfer.amount.value
      tx.getTransfer.recipient.get.recipient.publicKeyHash.get.toByteArray shouldBe recipientAddress.publicKeyHash

      txId shouldBe transfer.id.apply().arr

      assertionsTransactionsMetadata(append, recipientAddress.bytes)
      assertionsTransactionStateUpdatesBalances(append, 0, 0, secondAddress, senderBalanceBefore, senderBalanceAfter, "")
      assertionsTransactionStateUpdatesBalances(append, 0, 1, recipientAddress, recipientBalanceBefore, recipientBalanceAfter, "")
    }

  }

  def assertionsTransactionsMetadata(append: Append, recipientAddress: Array[Byte]): Unit = {
    val transactionsMetadata = append.transactionsMetadata
    transactionsMetadata.head.senderAddress.toByteArray shouldBe secondAddress.bytes
    transactionsMetadata.head.getTransfer.recipientAddress.toByteArray shouldBe recipientAddress
  }

  def assertionsTransactionStateUpdatesBalances
  (append: Append, txUpdIndex: Int, balanceIndex: Int, address: Address, before: Long, after: Long, assetId: String): Unit = {
    val transactionStateUpdatesBalances = append.transactionStateUpdates.apply(txUpdIndex).balances

    transactionStateUpdatesBalances.apply(balanceIndex).address.toByteArray shouldBe address.bytes
    transactionStateUpdatesBalances.apply(balanceIndex).amountBefore shouldEqual before
    transactionStateUpdatesBalances.apply(balanceIndex).amountAfter.get.amount shouldEqual after
    transactionStateUpdatesBalances.apply(balanceIndex).amountAfter.get.assetId.toStringUtf8 shouldBe assetId
  }
}
