package scorex.transaction

import java.nio.ByteBuffer

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.{Account, PrivateKeyAccount}
import scorex.transaction.state.database.state._
import scorex.utils._

class RowSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen {

  property("Row serialize and deserialize") {
    forAll(paymentGenerator, Gen.posNum[Long], Gen.posNum[Int]) { (payment: PaymentTransaction,
                                                                   balance: Long,
                                                                   lastRowHeight: Int) =>

      val txs = Seq(FeesStateChange(123L), payment)
      LagonakiTransaction.parse(payment.bytes).get shouldBe payment


      val row = Row(AccState(balance), txs, lastRowHeight)

      val b = ByteBuffer.allocate(row.bytes.length)
      b.put(row.bytes)
      b.flip()

      val des = Row.deserialize(b.asReadOnlyBuffer())
      des shouldBe row
    }
  }

}