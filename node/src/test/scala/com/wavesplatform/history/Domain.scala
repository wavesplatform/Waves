package com.wavesplatform.history

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.state._
import com.wavesplatform.transaction.{Asset, BlockchainUpdater, DiscardedBlocks, DiscardedTransactions, Transaction}

case class Domain(blockchainUpdater: Blockchain with BlockchainUpdater with NG, levelDBWriter: LevelDBWriter) {
  def effBalance(a: Address): Long = blockchainUpdater.effectiveBalance(a, 1000)

  def appendBlock(b: Block): Option[DiscardedTransactions] = blockchainUpdater.processBlock(b).explicitGet()

  def removeAfter(blockId: ByteStr): DiscardedBlocks = blockchainUpdater.removeAfter(blockId).explicitGet()

  def lastBlockId: ByteStr = blockchainUpdater.lastBlockId.get

  def carryFee: Long = blockchainUpdater.carryFee

  def balance(address: Address): Long = blockchainUpdater.balance(address)
  def balance(address: Address, asset: Asset): Long = blockchainUpdater.balance(address, asset)

  def addressTransactions(address: Address): Seq[(Int, Transaction)] = ???
}
