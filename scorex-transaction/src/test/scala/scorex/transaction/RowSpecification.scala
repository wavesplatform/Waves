package scorex.transaction

import java.nio.ByteBuffer

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.{Account, PrivateKeyAccount}
import scorex.transaction.state.database.state._
import scorex.utils._

class RowSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  property("Row serialize and deserialize") {
    forAll { (lastRowHeight: Int,
              amount: Long,
              fee: Long,
              timestamp: Long,
              balance: Long) =>

      val sender = PrivateKeyAccount(randomBytes(32), randomBytes(32), randomBytes(32))
      val recepient = new Account("jACSbUoHi4eWgNu6vzAnEx583NwmUAVfS")
      val payment = PaymentTransaction(sender, recepient, amount, fee, timestamp, randomBytes(64))
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