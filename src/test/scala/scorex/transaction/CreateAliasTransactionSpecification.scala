package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.{Alias, PrivateKeyAccount}

class CreateAliasTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("CreateAliasTransaction serialization roundtrip") {
    forAll(createAliasGen) { tx: CreateAliasTransaction =>
      val recovered = tx.builder.parseBytes(tx.bytes()).get
      recovered shouldEqual tx
    }
  }

  property("CreateAliasTransaction serialization from TypedTransaction") {
    forAll(createAliasGen) { tx: CreateAliasTransaction =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldEqual tx
    }
  }

  property("The same aliases from different senders have the same id") {
    forAll(accountGen, accountGen, aliasGen, timestampGen) {
      case (a1: PrivateKeyAccount, a2: PrivateKeyAccount, a: Alias, t: Long) =>
        val tx1 = CreateAliasTransactionV1.selfSigned(a1, a, MinIssueFee, t).right.get
        val tx2 = CreateAliasTransactionV1.selfSigned(a2, a, MinIssueFee, t).right.get
        tx1.id() shouldBe tx2.id()
    }
  }
}
