package scorex.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2.ByteStr
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.PrivateKeyAccount
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.smart.{Script, SetScriptTransaction}

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
    forAll(accountGen, smallFeeGen, timestampGen, byteArrayGen(10), byteArrayGen(15)) { case (acc: PrivateKeyAccount, fee: Long, t: Long, p1, p2) =>
      val tx1 = SetScriptTransaction.create(acc, Script.sigVerify, fee, t, Seq(ByteStr(p1))).right.get
      val tx2 = SetScriptTransaction.create(acc, Script.sigVerify, fee, t, Seq(ByteStr(p2))).right.get
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
