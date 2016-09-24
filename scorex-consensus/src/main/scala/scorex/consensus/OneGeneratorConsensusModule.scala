package scorex.consensus

import scorex.block.Block
import scorex.transaction.AssetAcc

trait OneGeneratorConsensusModule {

  /**
    * In most of algorithms there's only one block generator
    */
  def feesDistribution(block: Block): Map[AssetAcc, Long] = {
    val generator = block.consensusModule.generators(block).ensuring(_.size == 1).head
    val assetFees = block.transactions.map(_.assetFee)
    assetFees.groupBy(_._1).map(a => AssetAcc(generator, a._1) -> a._2.map(_._2).sum)
  }

}
