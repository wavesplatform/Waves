package scorex.lagonaki

import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.block.Block._
import scorex.consensus.nxt.{NxtLikeConsensusBlockData, NxtLikeConsensusModule}
import scorex.transaction.{PaymentTransaction, SimpleTransactionModule, Transaction}

import scala.util.Random

trait BlockTestingCommons extends TestingCommons {

  import TestingCommons._

  implicit val consensusModule = new NxtLikeConsensusModule()
  implicit val transactionModule = new SimpleTransactionModule()(application.settings, application)

  val genesis: Block = Block.genesis()
  val gen = new PrivateKeyAccount(Array.fill(32)(Random.nextInt(Byte.MaxValue).toByte))

  protected var lastBlockId: BlockId = genesis.uniqueId

  def genBlock(bt: Long, gs: Array[Byte], seed: Array[Byte], parentId: Option[BlockId] = None)
              (implicit consensusModule: NxtLikeConsensusModule, transactionModule: SimpleTransactionModule): Block = {

    val reference = parentId.getOrElse(lastBlockId)

    val sender = new PrivateKeyAccount(seed)
    val tx: Transaction = PaymentTransaction(sender, gen, 5, bt, System.currentTimeMillis() - 5000)

    val tbd = Seq(tx)
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
}