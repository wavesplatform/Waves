package com.wavesplatform.history

import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.state2._
import scorex.account.Address
import scorex.block.Block
import scorex.transaction.{BlockchainUpdater, History}

case class Domain(history: History, state: SnapshotStateReader, blockchainUpdater: BlockchainUpdater) {
  def effBalance(a: Address): Long  = state.effectiveBalance(a, state.height, 1000)
  def appendBlock(b: Block)         = blockchainUpdater.processBlock(b).explicitGet()
  def removeAfter(blockId: ByteStr) = blockchainUpdater.removeAfter(blockId).explicitGet()
  def lastBlockId                   = history.lastBlockId.get
  def portfolio(address: Address)   = state.portfolio(address)
}
