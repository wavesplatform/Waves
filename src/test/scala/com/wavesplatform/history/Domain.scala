package com.wavesplatform.history

import com.wavesplatform.state._
import scorex.account.Address
import scorex.block.Block
import scorex.transaction.BlockchainUpdater

case class Domain(blockchainUpdater: BlockchainUpdater with NG) {
  def effBalance(a: Address): Long  = blockchainUpdater.effectiveBalance(a, blockchainUpdater.height, 1000)
  def appendBlock(b: Block)         = blockchainUpdater.processBlock(b).explicitGet()
  def removeAfter(blockId: ByteStr) = blockchainUpdater.removeAfter(blockId).explicitGet()
  def lastBlockId                   = blockchainUpdater.lastBlockId.get
  def portfolio(address: Address)   = blockchainUpdater.portfolio(address)
}
