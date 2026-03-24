package com.wavesplatform.transaction

import com.wavesplatform.account.{AddressScheme, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsPublicKey}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.state.Height
import com.wavesplatform.test.*
import com.wavesplatform.transaction.serialization.impl.PBTransactionSerializer
import play.api.libs.json.Json

import scala.util.{Failure, Success}

class CommitToGenerationTransactionsSpec extends FreeSpec with WithDomain {
  private val wavesSigner = TxHelpers.signer(0)
  private val blsKp       = BlsKeyPair(wavesSigner.privateKey)
  private val sig         = CommitToGenerationTransaction.mkPopSignature(blsKp, Height(3000))

  private val origTx = CommitToGenerationTransaction(
    version = TxVersion.V1,
    sender = PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
    endorserPublicKey = BlsPublicKey(Base58.decode("6CagLT3FjEcaNHPYCaG2dcfEfzDj6ynVeZbxbLHkHdfzvbfBmBMkkatTYcBXD9cHMU")).explicitGet(),
    generationPeriodStart = Height(3000),
    timestamp = 1526287561757L,
    fee = TxPositiveAmount.unsafeFrom(100000000),
    commitmentSignature = sig,
    proofs = Proofs(ByteStr.decodeBase58("28kE1uN1pX2bwhzr9UHw5UuB9meTFEDFgeunNgy6nZWpHX4pzkGYotu8DhQ88AdqUG6Yy5wcXgHseKPBUygSgRMJ").get),
    chainId = AddressScheme.current.chainId
  )

  "JSON parsing" in {
    val js = Json.parse(s"""{
      "id": "FEjd4wn3HMmEvayqGVoBGHcf7uxn2GhR1zhKxL72935a",
      "type": 19,
      "version": 1,
      "fee": 100000000,
      "feeAssetId": null,
      "timestamp": 1526287561757,
      "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
      "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
      "generationPeriodStart": 3000,
      "endorserPublicKey": "6CagLT3FjEcaNHPYCaG2dcfEfzDj6ynVeZbxbLHkHdfzvbfBmBMkkatTYcBXD9cHMU",
      "commitmentSignature": "$sig",
      "proofs": [
        "28kE1uN1pX2bwhzr9UHw5UuB9meTFEDFgeunNgy6nZWpHX4pzkGYotu8DhQ88AdqUG6Yy5wcXgHseKPBUygSgRMJ"
      ],
      "chainId": 84
    }""")
    origTx.json() shouldEqual js
  }

  "PB roundtrip" in {
    PBTransactionSerializer.parseBytes(PBTransactionSerializer.bytes(origTx)) match {
      case Success(tx: CommitToGenerationTransaction) =>
        tx shouldBe origTx
        tx.proofs shouldBe origTx.proofs
      case Success(tx)        => fail(s"Unexpected transaction type: ${tx.tpe.transactionName}")
      case Failure(exception) => fail(exception)
    }
  }

  "Expected BLS key and PoP" in {
    val wavesPk = PrivateKey(ByteStr.decodeBase58("7UR2CZi6Gv6v1yqmgcPDD98ZtosvtHnNZRxvrHA2Tuyn").get)

    val blsKp = BlsKeyPair(wavesPk)
    blsKp.publicKey.byteStr.base64Raw shouldBe "jrugi0W0es2WxuHoptQtchqwactZsldOGucYObZrEIOpxbWmhL8dodvpnzA+2qUf"

    CommitToGenerationTransaction.mkPopSignature(blsKp, Height(1001)).byteStr.base64Raw shouldBe
      "sOlLZL2RZZ3c98PmUvKSN960aj+VJwyVGEUygI78mGDwGJflJWLHCwuqiYk1fRG7FOCJKOtKbKOG7tBykQ5iTcRu+7eLWhiodJw47YEfDOZHNwkl8dQwgxAam8+3BEvX"
  }
}
