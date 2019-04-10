package com.wavesplatform.extensions

import akka.actor.ActorSystem
import com.wavesplatform.account.Address
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.utils.Time
import com.wavesplatform.utx.{UtxPool, UtxPoolImpl}
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import monix.reactive.Observable

trait Context {
  def settings: WavesSettings
  def blockchain: Blockchain
  def time: Time
  def wallet: Wallet
  def utx: UtxPool
  def channels: ChannelGroup
  def spendableBalanceChanged: Observable[(Address, Asset)]
  def actorSystem: ActorSystem
}
