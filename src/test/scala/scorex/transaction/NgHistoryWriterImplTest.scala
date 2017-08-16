package scorex.transaction

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.TransactionGen
import com.wavesplatform.history._
import com.wavesplatform.state2.BlockDiff
import com.wavesplatform.state2.diffs._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class NgHistoryWriterImplTest extends PropSpec with GeneratorDrivenPropertyChecks with PropertyChecks with Matchers with TransactionGen {

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
      val history = new NgHistoryWriterImpl(HistoryWriterImpl(None, new ReentrantReadWriteLock()).get)
      val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, Seq(Seq(payment), Seq(payment2), Seq(payment3)))
      history.appendBlock(block)(Right(BlockDiff.empty)).explicitGet()
      microBlocks.foreach(mb => history.appendMicroBlock(mb)(_ => Right(BlockDiff.empty)).right.get)

      history.forgeBlock(block.signerData.signature).get match {
        case ((forged, txs)) =>
          Signed.validateSignatures(forged) shouldBe 'right
          txs shouldBe Seq(payment, payment2, payment3)
        case _ => ???
      }

      microBlocks.foreach { m =>
        history.forgeBlock(m.totalResBlockSig).get match {
          case ((forged, txs)) =>
            Signed.validateSignatures(forged) shouldBe 'right
          case _ => ???
        }
      }
    }
  }
}
