package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.assets.ScriptTransferTransaction

class ScriptTransferTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("ScriptTransferTransaction serialization roundtrip") {
    forAll(scriptTransferGen) { tx: ScriptTransferTransaction =>
      tx.bytes().head shouldBe TransactionType.ScriptTransferTransaction.id
      val recovered = ScriptTransferTransaction.parseTail(tx.bytes().tail).get
      assertTxs(recovered, tx)
    }
  }

  property("ScriptTransferTransaction serialization from TypedTransaction") {
    forAll(scriptTransferGen) { tx: ScriptTransferTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[ScriptTransferTransaction], tx)
    }
  }

  ignore("ScriptTransferTransaction id doesn't depend on proof") {}

  private def assertTxs(first: ScriptTransferTransaction, second: ScriptTransferTransaction): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.amount shouldEqual second.amount
    first.recipient shouldEqual second.recipient
    first.version shouldEqual second.version
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }
}
