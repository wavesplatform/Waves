package com.wavesplatform.history

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.state._
import com.wavesplatform.state.extensions.Distributions
import com.wavesplatform.transaction.{BlockchainUpdater, Transaction}
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.duration.Duration

//noinspection ScalaStyle
case class Domain(blockchainUpdater: BlockchainUpdater with NG) {
  def effBalance(a: Address): Long = blockchainUpdater.effectiveBalance(a, 1000)

  def appendBlock(b: Block) = blockchainUpdater.processBlock(b).explicitGet()

  def removeAfter(blockId: ByteStr) = blockchainUpdater.removeAfter(blockId).explicitGet()

  def lastBlockId = blockchainUpdater.lastBlockId.get

  def portfolio(address: Address) = Distributions(blockchainUpdater).portfolio(address)

  def addressTransactions(address: Address): Seq[(Height, Transaction)] =
    blockchainUpdater.addressTransactionsObservable(address, Set.empty).take(128).toListL.runSyncUnsafe(Duration.Inf)

  def carryFee = blockchainUpdater.carryFee
}
