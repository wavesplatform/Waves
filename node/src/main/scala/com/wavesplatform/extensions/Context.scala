package com.wavesplatform.extensions

import akka.actor.ActorSystem
import com.wavesplatform.account.Address
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import monix.reactive.Observable

trait Context {
  def settings: WavesSettings
  def blockchain: Blockchain
  def time: Time
  def wallet: Wallet
  def spendableBalanceChanged: Observable[(Address, Asset)]
  def spendableBalance(address: Address, assetId: Asset): Long
  def addToUtx(tx: Transaction): Unit
  def actorSystem: ActorSystem
}
