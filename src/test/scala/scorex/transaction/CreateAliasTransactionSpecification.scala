package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.{Alias, PrivateKeyAccount}
import scorex.transaction.TransactionParser.TransactionType

class CreateAliasTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Transfer serialization roundtrip") {
    forAll(createAliasGen) { tx: CreateAliasTransaction =>
      require(tx.bytes().head == TransactionType.CreateAliasTransaction.id)
      val recovered = CreateAliasTransaction.parseTail(tx.bytes().tail).get
      assertTxs(recovered, tx)
    }
  }

  property("Transfer serialization from TypedTransaction") {
    forAll(createAliasGen) { tx: CreateAliasTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[CreateAliasTransaction], tx)
    }
  }


  property("The same aliases from different senders have the same id") {
    forAll(accountGen, accountGen, aliasGen, timestampGen) { case (a1: PrivateKeyAccount, a2: PrivateKeyAccount, a: Alias, t: Long) =>
      val tx1 = CreateAliasTransaction.create(a1, a, MinIssueFee, t).right.get
      val tx2 = CreateAliasTransaction.create(a2, a, MinIssueFee, t).right.get
      tx1.id() shouldBe tx2.id()
    }
  }

  private def assertTxs(first: CreateAliasTransaction, second: CreateAliasTransaction): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.alias.bytes shouldEqual second.alias.bytes
    first.bytes() shouldEqual second.bytes()
  }
}
