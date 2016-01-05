package scorex.consensus

import scorex.account.Account
import scorex.block.Block

/**
  * Data and functions related to a consensus algo
  */

trait LagonakiConsensusModule[ConsensusBlockData] extends ConsensusModule[ConsensusBlockData] {

  /**
    * In Lagonaki, for both consensus modules, there's only one block generator
    * @param block
    * @return
    */
  override def feesDistribution(block: Block): Map[Account, Long] = {
    val forger = block.consensusModule.generators(block).ensuring(_.size == 1).head
    val fee = block.transactions.map(_.fee).sum
    Map(forger -> fee)
  }

  override def generators(block: Block): Seq[Account]
}
