package scorex.lagonaki

import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.block.Block._
import scorex.consensus.nxt.{NxtLikeConsensusBlockData, NxtLikeConsensusModule}
import scorex.lagonaki.mocks.ConsensusMock
import scorex.transaction.{PaymentTransaction, SimpleTransactionModule, Transaction}

import scala.util.Random

trait BlockTestingCommons extends TestingCommons {

  val genesis: Block = Block.genesis()
  val gen = new PrivateKeyAccount(Array.fill(32)(Random.nextInt(Byte.MaxValue).toByte))

  protected var lastBlockId: BlockId = genesis.uniqueId

  def genBlock(bt: Long, gs: Array[Byte], seed: Array[Byte], parentId: Option[BlockId] = None,
               transactions: Seq[Transaction] = Seq.empty)
              (implicit consensusModule: NxtLikeConsensusModule, transactionModule: SimpleTransactionModule): Block = {

    val reference = parentId.getOrElse(lastBlockId)

    val tbd = if (transactions.isEmpty) Seq(genTransaction(seed)) else transactions
    val cbd = new NxtLikeConsensusBlockData {
      override val generationSignature: Array[Byte] = gs
      override val baseTarget: Long = math.max(math.abs(bt), 1)
    }

    val version = 1: Byte
    val timestamp = System.currentTimeMillis()

    val block = Block.buildAndSign(version, timestamp, reference, cbd, tbd, gen)
    lastBlockId = block.uniqueId
    block
  }

  def genTransaction(seed: Array[Byte]): Transaction = {
    val sender = new PrivateKeyAccount(seed)
    PaymentTransaction(sender, gen, 1, 1, System.currentTimeMillis() - 5000)
  }
}
