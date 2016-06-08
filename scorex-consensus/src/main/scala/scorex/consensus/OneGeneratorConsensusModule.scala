package scorex.consensus

import scorex.account.Account
import scorex.block.Block

trait OneGeneratorConsensusModule {

  /**
   * In most of algorithms there's only one block generator
   */
  def feesDistribution(block: Block): Map[Account, Long] = {
    val forger = block.consensusModule.generators(block).ensuring(_.size == 1).head
    val fee = block.transactions.map(_.fee).sum
    Map(forger -> fee)
  }

}
