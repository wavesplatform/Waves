package com.wavesplatform.extensions

import akka.actor.ActorSystem
import com.wavesplatform.account.Address
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{Blockchain, BlockchainUpdated}
import com.wavesplatform.transaction.{Asset, BlockchainUpdater, Transaction}
import com.wavesplatform.utils.Time
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import monix.reactive.Observable

trait Context {
  def settings: WavesSettings
  def blockchain: Blockchain with BlockchainUpdater
  def time: Time
  def wallet: Wallet
  def utx: UtxPool
  def broadcastTx(tx: Transaction): Unit
  def spendableBalanceChanged: Observable[(Address, Asset)]
  def blockchainUpdated: Option[Observable[BlockchainUpdated]]
  def actorSystem: ActorSystem
}
