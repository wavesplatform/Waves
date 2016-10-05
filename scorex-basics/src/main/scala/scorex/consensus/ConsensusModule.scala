package scorex.consensus

import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.block.{Block, BlockProcessingModule}
import scorex.transaction.{AssetAcc, TransactionModule}


trait ConsensusModule[ConsensusBlockData] extends BlockProcessingModule[ConsensusBlockData] {

  def isValid[TransactionalBlockData](block: Block)(implicit transactionModule: TransactionModule[TransactionalBlockData]): Boolean

  /**
    * Fees could go to a single miner(forger) usually, but can go to many parties, e.g. see
    * Meni Rosenfeld's Proof-of-Activity proposal http://eprint.iacr.org/2014/452.pdf
    */
  def feesDistribution(block: Block): Map[AssetAcc, Long]

  /**
    * Get block producers(miners/forgers). Usually one miner produces a block, but in some proposals not
    * (see e.g. Meni Rosenfeld's Proof-of-Activity paper http://eprint.iacr.org/2014/452.pdf)
    */
  def generators(block: Block): Seq[Account]

  def blockScore(block: Block): BigInt

  def blockOrdering[TransactionalBlockData](implicit transactionModule: TransactionModule[TransactionalBlockData]): Ordering[(Block)] =
    Ordering.by {
      block =>
        val score = blockScore(block)
        val parent = transactionModule.blockStorage.history.blockById(block.referenceField.value).get
        val blockCreationTime = nextBlockGenerationTime(parent, block.signerDataField.value.generator)
          .getOrElse(block.timestampField.value)

        (score, -blockCreationTime)
    }

  def generateNextBlock[TransactionalBlockData](account: PrivateKeyAccount)
                           (implicit transactionModule: TransactionModule[TransactionalBlockData]): Option[Block]

  def generateNextBlocks[TransactionalBlockData](accounts: Seq[PrivateKeyAccount])
                           (implicit transactionModule: TransactionModule[TransactionalBlockData]): Seq[Block] =
    accounts.flatMap(generateNextBlock(_))

  def nextBlockGenerationTime(lastBlock: Block, account: PublicKeyAccount)
                             (implicit transactionModule: TransactionModule[_]): Option[Long]

  def consensusBlockData(block: Block): ConsensusBlockData
}

object ConsensusModule {

  /**
    * A naive but still a way to emphasize that cumulative score is sum of block scores
    */
  def cumulativeBlockScore(previousCumulativeScore: BigInt, blockScore: BigInt): BigInt = previousCumulativeScore + blockScore
}
