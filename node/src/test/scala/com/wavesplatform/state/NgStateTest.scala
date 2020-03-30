package com.wavesplatform.state

import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.history._
import com.wavesplatform.state.diffs._
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class NgStateTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  private def mkNg(block: Block) = NgState.KeyBlock(Diff.empty, block, 0, 0, Set.empty, None, block.header.generationSignature, Map.empty)

  def preconditionsAndPayments(amt: Int): Gen[(GenesisTransaction, Seq[TransferTransaction])] =
    for {
      master    <- accountGen
      recipient <- accountGen
      ts        <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      payments: Seq[TransferTransaction] <- Gen.listOfN(amt, wavesTransferGeneratorP(master, recipient))
    } yield (genesis, payments)

  property("can forge correctly signed blocks") {
    forAll(preconditionsAndPayments(10)) {
      case (genesis, payments) =>
        val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))
        val ng = mkNg(block)
        microBlocks.foreach(m => ng.append(m, Diff.empty, 0L, 0L, 0L))
        microBlocks.foreach { m =>
          ng.forId(m.totalResBlockSig).get.totalBlock.signatureValid() shouldBe true
        }
        Seq(microBlocks(4)).map(x => ng.contains(x.totalBlockId) shouldBe true)
    }
  }

  property("can resolve best liquid block") {
    forAll(preconditionsAndPayments(5)) {
      case (genesis, payments) =>
        val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))

        val ng = mkNg(block)
        microBlocks.foreach(m => ng.append(m, Diff.empty, 0L, 0L, 0L))

        ng.totalBlock.id() shouldBe microBlocks.last.totalResBlockSig

        mkNg(block).block.id() shouldBe block.id()
    }
  }

  property("can resolve best last block") {
    forAll(preconditionsAndPayments(5)) {
      case (genesis, payments) =>
        val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))

        val ng = mkNg(block)

        microBlocks.foldLeft(1000) {
          case (thisTime, m) =>
            ng.append(m, Diff.empty, 0L, 0L, thisTime)
            thisTime + 50
        }

        ng.bestLastBlockInfo(0).blockId shouldBe block.id()
        ng.bestLastBlockInfo(1001).blockId shouldBe microBlocks.head.totalResBlockSig
        ng.bestLastBlockInfo(1051).blockId shouldBe microBlocks.tail.head.totalResBlockSig
        ng.bestLastBlockInfo(2000).blockId shouldBe microBlocks.last.totalResBlockSig

        mkNg(block).block.id() shouldBe block.id()
    }
  }

  property("calculates carry fee correctly") {
    forAll(preconditionsAndPayments(5)) {
      case (genesis, payments) =>
        val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))

        val ng = mkNg(block)
        microBlocks.foreach(m => ng.append(m, Diff.empty, 1L, 0L, 0L))

        ng.forId(block.id()).map(_.carry) shouldBe Some(0L)
        microBlocks.zipWithIndex.foreach {
          case (m, i) =>
            val u = ng.forId(m.totalResBlockSig).map(_.carry)
            u shouldBe Some(i + 1)
        }
        ng.carry shouldBe microBlocks.size
    }
  }
}
