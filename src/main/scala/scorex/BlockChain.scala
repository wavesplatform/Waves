package scorex

import scorex.account.Account
import scorex.block.Block
import scorex.block.GenesisBlock
import database.DBSet
import scorex.transaction.Transaction
import scala.annotation.tailrec

object BlockChain {
  val MAX_SIGNATURES = 500

  if (!DBSet.getInstance().getBlockMap.contains(GenesisBlock.signature)) GenesisBlock.process()

  def getHeight = {
    val lastBlockSignature = DBSet.getInstance().getBlockMap.getLastBlockSignature
    DBSet.getInstance().getHeightMap.get(lastBlockSignature)
  }

  def getSignatures(parent: Array[Byte]) = {
    if (DBSet.getInstance().getBlockMap.contains(parent)) {

      (0 to MAX_SIGNATURES).foldLeft((DBSet.getInstance().getBlockMap.get(parent).getChild(), List[Array[Byte]]())) {
        case ((block, res), _) => Option(block) match {
          case Some(_) => (block.getChild(), block.signature :: res)
          case None => (block, res)
        }
      }._2
    } else List[Array[Byte]]()
  }

  def getBlock(header: Array[Byte]) = DBSet.getInstance().getBlockMap.get(header)

  def isNewBlockValid(block: Block) =
    block != GenesisBlock &&
      block.isSignatureValid &&
      DBSet.getInstance().getBlockMap.contains(block.reference) &&
      DBSet.getInstance().getBlockMap.getLastBlockSignature.sameElements(block.reference) &&
      block.isValid


  def scanTransactions(block: Block, blockLimit: Int, transactionLimit: Int, txType: Int, service: Int, account: Account) = {

    //todo: don't pass nullable at all
    val startBlock = Option(block).orElse(Some(GenesisBlock)).get

    @tailrec
    def recursiveExtraction(block: Block, accTransactions: List[Transaction], depth: Int): (Block, List[Transaction]) = {
      val txs = accTransactions ++ block.transactions.filter { transaction =>
        (account == null || transaction.isInvolved(account)) &&
          (txType == -1 || transaction.getType == txType)
      }

      Option(block.getChild).isDefined &&
        (txs.size < transactionLimit || transactionLimit == -1) &&
        (depth < blockLimit || blockLimit == -1) match {
        case true => recursiveExtraction(block.getChild, txs, depth + 1)
        case false => block -> txs
      }
    }

    recursiveExtraction(startBlock, List(), 0)
  }

  def getLastBlock = DBSet.getInstance().getBlockMap.getLastBlock
}