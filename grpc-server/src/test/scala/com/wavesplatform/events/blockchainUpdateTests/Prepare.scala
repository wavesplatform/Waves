package com.wavesplatform.events.blockchainUpdateTests

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.events.blockchainUpdateTests.fixtures.SubscribeHandler
import com.wavesplatform.events.FakeObserver.UpdatesRepoExt
import com.wavesplatform.events.{FakeObserver, Repo}
import com.wavesplatform.events.api.grpc.protobuf.{SubscribeEvent, SubscribeRequest}
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.history.Domain
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, TxHelpers}
import com.wavesplatform.transaction.TxHelpers.{secondAddress, secondSigner}
import com.wavesplatform.transaction.transfer.TransferTransaction

class Prepare(d: Domain, repo: Repo, fee: Long) {
  val recipient: KeyPair           = TxHelpers.signer(1234)
  val recipientAddress: Address    = recipient.toAddress
  var senderBalanceBefore: Long    = 0
  var senderBalanceAfter: Long     = 0
  var recipientBalanceBefore: Long = 0
  var recipientBalanceAfter: Long  = 0

  d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress))
  d.appendKeyBlock()
  d.appendBlock(TxHelpers.transfer())

  def  subscription(from: Int = 0, to: Int = 0): FakeObserver[SubscribeEvent] = repo.createFakeObserver(SubscribeRequest(from, to))

  def appendedSubscriptionState(txId: Array[Byte], subscribeFrom: Int, subscribeTo: Int): SubscribeHandler = {
    val subscribeEvent: FakeObserver[SubscribeEvent] = subscription(subscribeFrom, subscribeTo)
    val subscribeHandler = new SubscribeHandler
    subscribeHandler.subscribeResponseHandler(txId, subscribeEvent.fetchAllEvents(d.blockchain).iterator)
    subscribeHandler
  }

  def createAliasTransaction(aliasName: String): CreateAliasTransaction = {
    val alias: CreateAliasTransaction = TxHelpers.createAlias(aliasName, secondSigner, fee)
    calculateSenderBalances()
    d.appendMicroBlock(alias)
    alias
  }

  def createTransferTransaction(amount: Long, assetId: Asset): TransferTransaction = {
    val transfer = TxHelpers.transfer(secondSigner, recipientAddress, amount, assetId, fee)
    calculateSenderBalances(amount)
    calculateRecipientBalances(recipientAddress, amount)
    d.appendMicroBlock(transfer)
    transfer
  }

  private def calculateRecipientBalances(recipientAddress: Address, amount: Long): Unit = {
    recipientBalanceBefore = d.balance(recipientAddress)
    recipientBalanceAfter = recipientBalanceBefore - amount
  }

  private def calculateSenderBalances(amount: Long = 0): Unit = {
    senderBalanceBefore = d.balance(secondAddress)
    senderBalanceAfter = senderBalanceBefore - fee
    if (amount > 0) senderBalanceAfter - amount
  }
}
