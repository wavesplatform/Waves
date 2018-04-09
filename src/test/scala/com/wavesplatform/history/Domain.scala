package com.wavesplatform.history

import com.wavesplatform.state._
import com.wavesplatform.state.reader.SnapshotStateReader
import scorex.account.Address
import scorex.block.Block
import scorex.transaction.BlockchainUpdater

case class Domain(blockchain: Blockchain, state: SnapshotStateReader, blockchainUpdater: BlockchainUpdater) {
  def effBalance(a: Address): Long  = state.effectiveBalance(a, state.height, 1000)
  def appendBlock(b: Block)         = blockchainUpdater.processBlock(b).explicitGet()
  def removeAfter(blockId: ByteStr) = blockchainUpdater.removeAfter(blockId).explicitGet()
  def lastBlockId                   = blockchain.lastBlockId.get
  def portfolio(address: Address)   = state.portfolio(address)
}
