package scorex.consensus

import scorex.account.Account
import scorex.block.Block
import scorex.transaction.AssetAcc

trait OneGeneratorConsensusModule {

  /**
   * In most of algorithms there's only one block generator
   */
  def feesDistribution(block: Block): Map[AssetAcc, Long] = {
    val generator = block.consensusModule.generators(block).ensuring(_.size == 1).head
    val fee = block.transactions.map(_.fee).sum
    //TODO fee in assets
    Map(AssetAcc(generator, None) -> fee)
  }

}
