package scorex.consensus

import scorex.controller.Controller
import scorex.account.PrivateKeyAccount
import scorex.block.{Block, BlockStub}
import scala.collection.concurrent.TrieMap
import scala.util.Random


trait BlockGenerationFunctions {

  import Controller.wallet

  def generateBlock(): Option[Block] = {
    val blockStubs = wallet.privateKeyAccounts().foldLeft(TrieMap[PrivateKeyAccount, BlockStub]()) { case (bm, account) =>
      if (account.generatingBalance >= BigDecimal(1)) {
        generateNextBlock(account, Controller.blockchainStorage.lastBlock).foreach { blockStub =>
          bm += account -> blockStub
        }
      }
      bm
    }
    randomBlockFromStubs(blockStubs)
  }

  protected def randomBlockFromStubs(blockStubs: TrieMap[PrivateKeyAccount, BlockStub]): Option[Block] =
    if (blockStubs.size > 0) {
      val generators = blockStubs.keys.toIndexedSeq
      val randomGen = generators(Random.nextInt(generators.size))
      val rndStub = blockStubs(randomGen)
      Some(Block(rndStub, randomGen))
    } else None

  protected def generateNextBlock(account: PrivateKeyAccount, lastBlock: Block): Option[BlockStub]
}
