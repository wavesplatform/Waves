package com.wavesplatform.transaction

import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.finalization.BlsPublicKey
import com.wavesplatform.test.*
import com.wavesplatform.transaction.serialization.impl.PBTransactionSerializer
import play.api.libs.json.Json

import scala.util.{Failure, Success}

class CommitToGenerationTransactionsSpec extends PropSpec {
  private val origTx = CommitToGenerationTransaction(
    sender = PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
    fee = TxPositiveAmount.unsafeFrom(100000000),
    timestamp = 1526287561757L,
    endorsementPublicKey = BlsPublicKey(ByteStr.decodeBase58("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").get), // TODO:
    endorsementKeySignature =
      ByteStr.decodeBase58("28kE1uN1pX2bwhzr9UHw5UuB9meTFEDFgeunNgy6nZWpHX4pzkGYotu8DhQ88AdqUG6Yy5wcXgHseKPBUygSgRMJ").get, // TODO:
    proofs = Proofs(ByteStr.decodeBase58("28kE1uN1pX2bwhzr9UHw5UuB9meTFEDFgeunNgy6nZWpHX4pzkGYotu8DhQ88AdqUG6Yy5wcXgHseKPBUygSgRMJ").get),
    chainId = AddressScheme.current.chainId
  )

  property("JSON parsing") {
    val js = Json.parse("""{
      "id": "DU3qmbWH6juRpNa6XePpqUWoE5jBfqE5DmhdnKFjyFH2",
      "type": 20,
      "version": 1,
      "fee": 100000000,
      "feeAssetId": null,
      "timestamp": 1526287561757,
      "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
      "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
      "endorsementPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
      "endorsementKeySignature": "28kE1uN1pX2bwhzr9UHw5UuB9meTFEDFgeunNgy6nZWpHX4pzkGYotu8DhQ88AdqUG6Yy5wcXgHseKPBUygSgRMJ",
      "proofs": [
        "28kE1uN1pX2bwhzr9UHw5UuB9meTFEDFgeunNgy6nZWpHX4pzkGYotu8DhQ88AdqUG6Yy5wcXgHseKPBUygSgRMJ"
      ],
      "chainId": 84
    }""")

    origTx.json() shouldEqual js
  }

  property("PB roundtrip") {
    PBTransactionSerializer.parseBytes(PBTransactionSerializer.bytes(origTx)) match {
      case Success(tx: CommitToGenerationTransaction) =>
        tx shouldBe origTx
        tx.proofs shouldBe origTx.proofs
      case Success(tx)        => fail(s"Unexpected transaction type: ${tx.tpe.transactionName}")
      case Failure(exception) => fail(exception)
    }
  }
}
