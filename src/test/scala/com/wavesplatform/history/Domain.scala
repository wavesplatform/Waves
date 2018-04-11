package com.wavesplatform.history

import com.wavesplatform.state._
import scorex.account.Address
import scorex.block.Block
import scorex.transaction.BlockchainUpdater

case class Domain(blockchain: Blockchain, blockchainUpdater: BlockchainUpdater) {
  def effBalance(a: Address): Long  = blockchain.effectiveBalance(a, blockchain.height, 1000)
  def appendBlock(b: Block)         = blockchainUpdater.processBlock(b).explicitGet()
  def removeAfter(blockId: ByteStr) = blockchainUpdater.removeAfter(blockId).explicitGet()
  def lastBlockId                   = blockchain.lastBlockId.get
  def portfolio(address: Address)   = blockchain.portfolio(address)
}
