package com.wavesplatform.events.blockchainUpdateTests

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.events.blockchainUpdateTests.fixtures.SubscribeHandler
import com.wavesplatform.events.FakeObserver.UpdatesRepoExt
import com.wavesplatform.events.{FakeObserver, Repo}
import com.wavesplatform.events.api.grpc.protobuf.{SubscribeEvent, SubscribeRequest}
import com.wavesplatform.history.Domain
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, TxHelpers}
import com.wavesplatform.transaction.TxHelpers.{secondAddress, secondSigner}
import com.wavesplatform.transaction.transfer.TransferTransaction

case class Prepare(d: Domain, repo: Repo, fee: Long) {
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
    d.appendMicroBlock(alias)
    alias
  }

  def createTransferTransaction(recipientAddress: Address, amount: Long, assetId: Asset): TransferTransaction = {
    val transfer = TxHelpers.transfer(secondSigner, recipientAddress, amount, assetId, fee)
    d.appendMicroBlock(transfer)
    transfer
  }

  def getBalance(senderAddress: Address = secondAddress): Long = {
    d.balance(senderAddress)
  }
}
