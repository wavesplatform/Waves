package scorex.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.assets.VersionedTransferTransaction

class VersionedTransferTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("ScriptTransferTransaction serialization roundtrip") {
    forAll(versionedTransferGen) { tx: VersionedTransferTransaction =>
      tx.bytes().head shouldBe TransactionType.VersionedTransferTransaction.id
      val recovered = VersionedTransferTransaction.parseTail(tx.bytes().tail).get
      assertTxs(recovered, tx)
    }
  }

  property("ScriptTransferTransaction serialization from TypedTransaction") {
    forAll(versionedTransferGen) { tx: VersionedTransferTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[VersionedTransferTransaction], tx)
    }
  }

  property("ScriptTransferTransaction id doesn't depend on proof") {
    forAll(accountGen, accountGen, proofsGen, proofsGen, bytes32gen) { case (acc1, acc2, proofs1, proofs2, attachment) =>
      val tx1 = VersionedTransferTransaction.create(2, None, acc2, acc2.toAddress, 1, 1, 1, attachment, proofs1).explicitGet()
      val tx2 = VersionedTransferTransaction.create(2, None, acc2, acc2.toAddress, 1, 1, 1, attachment, proofs2).explicitGet()
      tx1.id() shouldBe tx2.id()
    }
  }

  private def assertTxs(first: VersionedTransferTransaction, second: VersionedTransferTransaction): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.amount shouldEqual second.amount
    first.recipient shouldEqual second.recipient
    first.version shouldEqual second.version
    first.assetId shouldEqual second.assetId
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }
}
