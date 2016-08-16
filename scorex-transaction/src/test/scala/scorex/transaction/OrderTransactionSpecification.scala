package scorex.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.exchange.Order

class OrderTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Order transaction serialization roundtrip") {
    forAll(orderGenerator) { order: Order =>
      val recovered = Order.parseBytes(order.bytes).get
      recovered.bytes shouldEqual order.bytes
    }
  }
}
