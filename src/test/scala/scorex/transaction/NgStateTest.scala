package scorex.transaction

import com.wavesplatform.history._
import com.wavesplatform.state2.diffs._
import com.wavesplatform.state2.{BlockDiff, NgState}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class NgStateTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  def preconditionsAndPayments(amt: Int): Gen[(GenesisTransaction, Seq[PaymentTransaction])] = for {
    master <- accountGen
    recipient <- accountGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    payments: Seq[PaymentTransaction] <-Gen.listOfN(amt,paymentGeneratorP(master, recipient))
  } yield (genesis, payments)

  property("can forge correctly signed blocks") {

    forAll(preconditionsAndPayments(10)) { case (genesis, payments) =>
      val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))

      val ng = new NgState(block, BlockDiff.empty, Set.empty[Short])
      microBlocks.foreach(m => ng.append(m, BlockDiff.empty, 0L))

      ng.totalDiffOf(microBlocks.last.totalResBlockSig)
      microBlocks.foreach { m =>
        ng.totalDiffOf(m.totalResBlockSig).get match {
          case ((forged, _, _)) =>
            forged.signaturesValid() shouldBe 'right
          case _ => ???
        }
      }
      Seq(microBlocks(4)).map(x => ng.totalDiffOf(x.totalResBlockSig))
    }
  }
  property("can resolve best liquid block") {

    forAll(preconditionsAndPayments(5)) { case (genesis,payments) =>
      val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))

      val ng = new NgState(block, BlockDiff.empty, Set.empty[Short])
      microBlocks.foreach(m => ng.append(m, BlockDiff.empty, 0L))

      ng.bestLiquidBlock.uniqueId shouldBe microBlocks.last.totalResBlockSig

      new NgState(block, BlockDiff.empty, Set.empty[Short]).bestLiquidBlock.uniqueId shouldBe block.uniqueId
    }
  }

  property("can resolve best last block") {

    forAll(preconditionsAndPayments(5)) { case (genesis,payments) =>
      val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, payments.map(t => Seq(t)))

      val ng = new NgState(block, BlockDiff.empty, Set.empty[Short])

      microBlocks.foldLeft(1000) { case (thisTime, m) =>
        ng.append(m, BlockDiff.empty, thisTime)
        thisTime + 50
      }

      ng.bestLastBlockInfo(0).blockId shouldBe block.uniqueId
      ng.bestLastBlockInfo(1001).blockId shouldBe microBlocks.head.totalResBlockSig
      ng.bestLastBlockInfo(1051).blockId shouldBe microBlocks.tail.head.totalResBlockSig
      ng.bestLastBlockInfo(2000).blockId shouldBe microBlocks.last.totalResBlockSig

      new NgState(block, BlockDiff.empty, Set.empty[Short]).bestLiquidBlock.uniqueId shouldBe block.uniqueId
    }
  }
}
