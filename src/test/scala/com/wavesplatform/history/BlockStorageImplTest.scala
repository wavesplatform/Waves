package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state2.diffs._
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{ByteStr, _}
import org.scalacheck.{Gen, Shrink}
import org.scalatest._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction._

class BlockStorageImplTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  import BlockStorageImplTest._

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndPayments: Gen[(GenesisTransaction, PaymentTransaction, PaymentTransaction)] = for {
    master <- accountGen
    recipient <- otherAccountGen(candidate = master)
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    payment: PaymentTransaction <- paymentGeneratorP(master, recipient)
    payment2: PaymentTransaction <- paymentGeneratorP(master, recipient)
  } yield (genesis, payment, payment2)

  property("can apply valid blocks") {
    forAll(preconditionsAndPayments) { case ((genesis, payment, _)) =>
      val fp = setup()
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment)))
      blocks.foreach(block => fp.blockchainUpdater.processBlock(block).explicitGet())
    }
  }

  property("can apply, rollback and reprocess valid blocks") {
    forAll(preconditionsAndPayments) { case ((genesis, payment, payment2)) =>
      val fp = setup()
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment), Seq(payment2)))
      fp.blockchainUpdater.processBlock(blocks.head) shouldBe 'right
      fp.history.height() shouldBe 1
      fp.stateReader.height shouldBe 1
      fp.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
      fp.history.height() shouldBe 2
      fp.stateReader.height shouldBe 2
      fp.blockchainUpdater.removeAfter(blocks.head.uniqueId) shouldBe true
      fp.history.height() shouldBe 1
      fp.stateReader.height shouldBe 1
      fp.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
      fp.blockchainUpdater.processBlock(blocks(2)) shouldBe 'right
    }
  }

  property("can't apply block with invalid signature") {
    forAll(preconditionsAndPayments) { case ((genesis, payment, _)) =>
      val fp = setup()
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment)))
      fp.blockchainUpdater.processBlock(blocks.head) shouldBe 'right
      fp.blockchainUpdater.processBlock(malformSignature(blocks.last)) should produce("InvalidSignature")
    }
  }

  property("can't apply block with invalid signature after rollback") {
    forAll(preconditionsAndPayments) { case ((genesis, payment, _)) =>
      val fp = setup()
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment)))
      fp.blockchainUpdater.processBlock(blocks.head) shouldBe 'right
      fp.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
      fp.blockchainUpdater.removeAfter(blocks.head.uniqueId) shouldBe true
      fp.blockchainUpdater.processBlock(malformSignature(blocks(1))) should produce("InvalidSignature")
    }
  }
}

object BlockStorageImplTest {

  case class Setup(history: History, stateReader: StateReader, blockchainUpdater: BlockchainUpdater)

  val MinInMemoryDiffSize = 5
  val DefaultBlockchainSettings = BlockchainSettings(
    blockchainFile = None,
    stateFile = None,
    checkpointFile = None,
    addressSchemeCharacter = 'N',
    minimumInMemoryDiffSize = MinInMemoryDiffSize,
    functionalitySettings = TestFunctionalitySettings.Enabled,
    genesisSettings = null)

  def setup(): Setup = {
    val (history, _, stateReader, blockchainUpdater) = BlockStorageImpl(DefaultBlockchainSettings).get
    Setup(history, stateReader, blockchainUpdater)
  }

  def chainBlocks(txs: Seq[Seq[Transaction]]): Seq[Block] = {
    val signer = PrivateKeyAccount(Array.fill(32)(0))

    def chainBlocksR(refTo: ByteStr, txs: Seq[Seq[Transaction]]): Seq[Block] = txs match {
      case (x :: xs) =>
        val block = Block.buildAndSign(
          version = 1: Byte,
          timestamp = 0L,
          reference = refTo,
          consensusData = NxtLikeConsensusBlockData(
            baseTarget = 1L,
            generationSignature = TestBlock.randomOfLength(Block.GeneratorSignatureLength).arr),
          transactionData = x,
          signer = signer)
        block +: chainBlocksR(block.uniqueId, xs)
      case _ => Seq.empty
    }

    chainBlocksR(TestBlock.randomOfLength(Block.BlockIdLength), txs)
  }

  def malformSignature(b: Block): Block = b.copy(signerData = b.signerData.copy(signature = TestBlock.randomSignature()))
}