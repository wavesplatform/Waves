package scorex.consensus

import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.block.{Block, BlockField}
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.transaction.TransactionModule

trait ConsensusModule {

  def genesisData: BlockField[NxtLikeConsensusBlockData]

  def isValid(block: Block)(implicit transactionModule: TransactionModule): Boolean

  def blockOrdering(implicit transactionModule: TransactionModule): Ordering[(Block)] =
    Ordering.by {
      block =>
        val score = block.blockScore
        val parent = transactionModule.blockStorage.history.blockById(block.referenceField.value).get
        val blockCreationTime = nextBlockGenerationTime(parent, block.signerDataField.value.generator)
          .getOrElse(block.timestampField.value)

        (score, -blockCreationTime)
    }

  def generateNextBlock(account: PrivateKeyAccount)
                           (implicit transactionModule: TransactionModule): Option[Block]

  def generateNextBlocks(accounts: Seq[PrivateKeyAccount])
                           (implicit transactionModule: TransactionModule): Seq[Block] =
    accounts.flatMap(generateNextBlock(_))

  def nextBlockGenerationTime(lastBlock: Block, account: PublicKeyAccount)
                             (implicit transactionModule: TransactionModule): Option[Long]

}
