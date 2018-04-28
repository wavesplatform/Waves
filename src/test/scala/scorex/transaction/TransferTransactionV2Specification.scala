package scorex.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.state._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.transfer._

class TransferTransactionV2Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  private val versionGen: Gen[Byte] = Gen.oneOf(TransferTransactionV2.supportedVersions.toSeq)

  property("VersionedTransferTransactionSpecification serialization roundtrip") {
    forAll(transferV2Gen) { tx: TransferTransactionV2 =>
      val recovered = TransferTransactionV2.parseBytes(tx.bytes()).get
      assertTxs(recovered, tx)
    }
  }

  property("VersionedTransferTransactionSpecification serialization from TypedTransaction") {
    forAll(transferV2Gen) { tx: TransferTransactionV2 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[TransferTransactionV2], tx)
    }
  }

  property("VersionedTransferTransactionSpecification id doesn't depend on proof") {
    forAll(versionGen, accountGen, accountGen, proofsGen, proofsGen, bytes32gen) {
      case (version, acc1, acc2, proofs1, proofs2, attachment) =>
        val tx1 = TransferTransactionV2.create(version, None, acc2, acc2.toAddress, 1, 1, None, 1, attachment, proofs1).explicitGet()
        val tx2 = TransferTransactionV2.create(version, None, acc2, acc2.toAddress, 1, 1, None, 1, attachment, proofs2).explicitGet()
        tx1.id() shouldBe tx2.id()
    }
  }

  private def assertTxs(first: TransferTransactionV2, second: TransferTransactionV2): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.amount shouldEqual second.amount
    first.recipient shouldEqual second.recipient
    first.version shouldEqual second.version
    first.assetId shouldEqual second.assetId
    first.feeAssetId shouldEqual second.feeAssetId
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }
}
