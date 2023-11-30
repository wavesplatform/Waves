package com.wavesplatform.extensions

import akka.actor.ActorSystem
import com.wavesplatform.api.common.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.UtxEvent
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{DiscardedBlocks, Transaction}
import com.wavesplatform.utils.Time
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import monix.eval.Task
import monix.reactive.Observable

trait Context {
  def settings: WavesSettings
  def blockchain: Blockchain
  def rollbackTo(blockId: ByteStr): Task[Either[ValidationError, DiscardedBlocks]]
  def time: Time
  def wallet: Wallet
  def utx: UtxPool

  def transactionsApi: CommonTransactionsApi
  def blocksApi: CommonBlocksApi
  def accountsApi: CommonAccountsApi
  def assetsApi: CommonAssetsApi

  def broadcastTransaction(tx: Transaction): TracedResult[ValidationError, Boolean]
  def utxEvents: Observable[UtxEvent]
  def actorSystem: ActorSystem
}
