package scorex.consensus

import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scala.collection.concurrent.TrieMap
import scala.util.Random


trait BlockGenerationFunctions {

  /*
  def generateBlock(): Option[Block] = {
    val blockStubs = wallet.privateKeyAccounts().foldLeft(TrieMap[PrivateKeyAccount, BlockStub]()) { case (bm, account) =>
      if (blockchainStorage.generationBalance(account) >= 1) {
        generateNextBlock(account, Controller.blockchainStorage.lastBlock).foreach { blockStub =>
          bm += account -> blockStub
        }
      }
      bm
    }
    randomBlockFromStubs(blockStubs)
  }

  protected def randomBlockFromStubs(blockStubs: TrieMap[PrivateKeyAccount, BlockStub]): Option[Block] =
    if (blockStubs.nonEmpty) {
      val generators = blockStubs.keys.toIndexedSeq
      val randomGen = generators(Random.nextInt(generators.size))
      val rndStub = blockStubs(randomGen)
      Some(Block(rndStub, randomGen))
    } else None

  protected def generateNextBlock(account: PrivateKeyAccount, lastBlock: Block): Option[BlockStub]
  */
}
