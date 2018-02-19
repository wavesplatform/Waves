package scorex.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.PrivateKeyAccount
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.smart.SetScriptTransaction

class SetScriptTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("SetScriptTransaction serialization roundtrip") {
    forAll(setScriptTransactionGen) { tx: SetScriptTransaction =>
      require(tx.bytes().head == TransactionType.SetScriptTransaction.id)
      val recovered = SetScriptTransaction.parseTail(tx.bytes().tail).get
      assertTxs(recovered, tx)
    }
  }

  property("SetScriptTransaction serialization from TypedTransaction") {
    forAll(setScriptTransactionGen) { tx: SetScriptTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[SetScriptTransaction], tx)
    }
  }


  property("SetScriptTransaction id doesn't depend on proof") {
    forAll(accountGen, byteArrayGen(10), byteArrayGen(15), proofsGen, proofsGen, scriptGen) { case (acc: PrivateKeyAccount, p1, p2, proofs1, proofs2, script) =>
      val tx1 = SetScriptTransaction.create(acc, Some(script), 1, 1, proofs1).explicitGet()
      val tx2 = SetScriptTransaction.create(acc, Some(script), 1, 1, proofs2).explicitGet()
      tx1.id() shouldBe tx2.id()
    }
  }

  private def assertTxs(first: SetScriptTransaction, second: SetScriptTransaction): Unit = {
    first.script shouldEqual second.script
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.version shouldEqual second.version
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }
}
