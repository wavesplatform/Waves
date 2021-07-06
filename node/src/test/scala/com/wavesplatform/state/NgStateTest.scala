package com.wavesplatform.state

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.history._
import com.wavesplatform.state.diffs._
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.transfer._
import org.scalacheck.Gen

class NgStateTest extends PropSpec {

  def preconditionsAndPayments(amt: Int): Gen[(GenesisTransaction, Seq[TransferTransaction])] =
    for {
      master    <- accountGen
      recipient <- accountGen
      ts        <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      payments: Seq[TransferTransaction] <- Gen.listOfN(amt, wavesTransferGeneratorP(master, recipient.toAddress))
    } yield (genesis, payments)

  property("can forge correctly signed blocks") {
    forAll(preconditionsAndPayments(10)) {
      case (genesis, payments) =>
        val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))

        var ng = NgState(block, Diff.empty, 0L, 0L, Set.empty, None, block.header.generationSignature, Map.empty)
        microBlocks.foreach(m => ng = ng.append(m, Diff.empty, 0L, 0L, 0L))

        ng.totalDiffOf(microBlocks.last.totalResBlockSig)
        microBlocks.foreach { m =>
          val (forged, _, _, _, _) = ng.totalDiffOf(m.totalResBlockSig).get
          forged.signatureValid() shouldBe true
        }
        Seq(microBlocks(4)).map(x => ng.totalDiffOf(x.totalResBlockSig))
    }
  }

  property("can resolve best liquid block") {
    forAll(preconditionsAndPayments(5)) {
      case (genesis, payments) =>
        val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))

        var ng = NgState(block, Diff.empty, 0L, 0L, Set.empty, None, block.header.generationSignature, Map.empty)
        microBlocks.foreach(m => ng = ng.append(m, Diff.empty, 0L, 0L, 0L))

        ng.bestLiquidBlock.id() shouldBe microBlocks.last.totalResBlockSig

        new NgState(block, Diff.empty, 0L, 0L, Set.empty, Some(0), block.header.generationSignature, Map.empty).bestLiquidBlock.id() shouldBe block
          .id()
    }
  }

  property("can resolve best last block") {
    forAll(preconditionsAndPayments(5)) {
      case (genesis, payments) =>
        val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))

        var ng = NgState(block, Diff.empty, 0L, 0L, Set.empty, None, block.header.generationSignature, Map.empty)

        microBlocks.foldLeft(1000) {
          case (thisTime, m) =>
            ng = ng.append(m, Diff.empty, 0L, 0L, thisTime)
            thisTime + 50
        }

        ng.bestLastBlockInfo(0).blockId shouldBe block.id()
        ng.bestLastBlockInfo(1001).blockId shouldBe microBlocks.head.totalResBlockSig
        ng.bestLastBlockInfo(1051).blockId shouldBe microBlocks.tail.head.totalResBlockSig
        ng.bestLastBlockInfo(2000).blockId shouldBe microBlocks.last.totalResBlockSig

        new NgState(block, Diff.empty, 0L, 0L, Set.empty, Some(0), block.header.generationSignature, Map.empty).bestLiquidBlock.id() shouldBe block
          .id()
    }
  }

  property("calculates carry fee correctly") {
    forAll(preconditionsAndPayments(5)) {
      case (genesis, payments) =>
        val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))

        var ng = NgState(block, Diff.empty, 0L, 0L, Set.empty, None, block.header.generationSignature, Map.empty)
        microBlocks.foreach(m => ng = ng.append(m, Diff.empty, 1L, 0L, 0L))

        ng.totalDiffOf(block.id()).map(_._3) shouldBe Some(0L)
        microBlocks.zipWithIndex.foreach {
          case (m, i) =>
            val u = ng.totalDiffOf(m.totalResBlockSig).map(_._3)
            u shouldBe Some(i + 1)
        }
        ng.carryFee shouldBe microBlocks.size
    }
  }
}
