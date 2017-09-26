package scorex.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.history._
import com.wavesplatform.state2.NgState._
import com.wavesplatform.state2.diffs._
import com.wavesplatform.state2.{BlockDiff, NgState}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class NgStateTest extends PropSpec with GeneratorDrivenPropertyChecks with PropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndPayments: Gen[(GenesisTransaction, PaymentTransaction, PaymentTransaction, PaymentTransaction)] = for {
    master <- accountGen
    recipient <- accountGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    payment: PaymentTransaction <- paymentGeneratorP(master, recipient)
    payment2: PaymentTransaction <- paymentGeneratorP(master, recipient)
    payment3: PaymentTransaction <- paymentGeneratorP(master, recipient)
  } yield (genesis, payment, payment2, payment3)

  property("can forge correctly signed blocks") {

    forAll(preconditionsAndPayments) { case (genesis, payment, payment2, payment3) =>
      val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, Seq(Seq(payment), Seq(payment2), Seq(payment3)))

      val ngState = microBlocks.foldLeft(NgState(block, BlockDiff.empty, 0L)) { case ((ng, m)) => ng + (m, BlockDiff.empty, 0L) }

      microBlocks.foreach { m =>
        ngState.forgeBlock(m.totalResBlockSig).get match {
          case ((forged, _)) =>
            Signed.validateSignatures(forged) shouldBe 'right
          case _ => ???
        }
      }
    }
  }
  property("can resolve best liquid block") {

    forAll(preconditionsAndPayments) { case (genesis, payment, payment2, payment3) =>
      val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, Seq(Seq(payment), Seq(payment2), Seq(payment3)))

      microBlocks.foldLeft(NgState(block, BlockDiff.empty, 0L)) { case ((ng, m)) => ng + (m, BlockDiff.empty, 0L) }
        .bestLiquidBlock.uniqueId shouldBe microBlocks.last.totalResBlockSig

      NgState(block, BlockDiff.empty, 0L).bestLiquidBlock.uniqueId shouldBe block.uniqueId
    }
  }

  property("can resolve best last block") {

    forAll(preconditionsAndPayments) { case (genesis, payment, payment2, payment3) =>
      val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, Seq(Seq(payment), Seq(payment2), Seq(payment3)))

      val ngState = microBlocks.foldLeft((NgState(block, BlockDiff.empty, 0L), 1000)) { case (((ng, thisTime), m)) => (ng + (m, BlockDiff.empty, thisTime), thisTime + 50) }._1

      ngState.bestLastBlock(0).uniqueId shouldBe block.uniqueId
      ngState.bestLastBlock(1001).uniqueId shouldBe microBlocks.head.totalResBlockSig
      ngState.bestLastBlock(1051).uniqueId shouldBe microBlocks.tail.head.totalResBlockSig
      ngState.bestLastBlock(1101).uniqueId shouldBe microBlocks.last.totalResBlockSig

      NgState(block, BlockDiff.empty, 0L).bestLiquidBlock.uniqueId shouldBe block.uniqueId
    }
  }
}
