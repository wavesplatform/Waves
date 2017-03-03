package scorex.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.TransactionParser.TransactionType

class CreateAliasTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Transfer serialization roundtrip") {
    forAll(createAliasGenerator) { tx: CreateAliasTransaction =>
      require(tx.bytes.head == TransactionType.CreateAliasTransaction.id)
      val recovered = CreateAliasTransaction.parseTail(tx.bytes.tail).get
      assertTxs(recovered, tx)
    }
  }

  property("Transfer serialization from TypedTransaction") {
    forAll(createAliasGenerator) { tx: CreateAliasTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes).get
      assertTxs(recovered.asInstanceOf[CreateAliasTransaction], tx)
    }
  }

  private def assertTxs(first: CreateAliasTransaction, second: CreateAliasTransaction): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.alias.bytes shouldEqual second.alias.bytes
    first.bytes shouldEqual second.bytes
  }
}
