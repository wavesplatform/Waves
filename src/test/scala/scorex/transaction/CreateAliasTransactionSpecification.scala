package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.{Alias, PrivateKeyAccount}

class CreateAliasTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("CreateAliasTransaction serialization roundtrip") {
    forAll(createAliasGen) { tx: CreateAliasTransactionV1 =>
      val recovered = CreateAliasTransactionV1.parseBytes(tx.bytes()).get
      assertTxs(recovered, tx)
    }
  }

  property("CreateAliasTransaction serialization from TypedTransaction") {
    forAll(createAliasGen) { tx: CreateAliasTransactionV1 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[CreateAliasTransactionV1], tx)
    }
  }

  property("The same aliases from different senders have the same id") {
    forAll(accountGen, accountGen, aliasGen, timestampGen) {
      case (a1: PrivateKeyAccount, a2: PrivateKeyAccount, a: Alias, t: Long) =>
        val tx1 = CreateAliasTransactionV1.create(a1, a, MinIssueFee, t).right.get
        val tx2 = CreateAliasTransactionV1.create(a2, a, MinIssueFee, t).right.get
        tx1.id() shouldBe tx2.id()
    }
  }

  private def assertTxs(first: CreateAliasTransactionV1, second: CreateAliasTransactionV1): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.alias.bytes shouldEqual second.alias.bytes
    first.bytes() shouldEqual second.bytes()
  }
}
