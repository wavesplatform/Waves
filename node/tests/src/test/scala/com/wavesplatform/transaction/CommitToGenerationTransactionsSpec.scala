package com.wavesplatform.transaction

import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.crypto.bls.{BlsPublicKey, BlsSignature}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.Height
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.{DeterministicFinality, WavesSettingsOps}
import com.wavesplatform.transaction.serialization.impl.PBTransactionSerializer
import play.api.libs.json.Json

import scala.util.{Failure, Success}

class CommitToGenerationTransactionsSpec extends FreeSpec with WithDomain {
  private val origTx = CommitToGenerationTransaction(
    sender = PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
    endorsementPublicKey = BlsPublicKey(ByteStr.decodeBase58("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").get),
    generationPeriodStart = Height(3000),
    timestamp = 1526287561757L,
    fee = TxPositiveAmount.unsafeFrom(100000000),
    endorsementKeySignature = BlsSignature(Base64.decode("OLI6mFSZD949zoVKqRt48SDNNnToWO+vUsmtCdeix7wH5RQkOwYQQvQEhQW/fmNCmsrmbW2IMt7SGjnCvPW9gQ==")),
    proofs = Proofs(ByteStr.decodeBase58("28kE1uN1pX2bwhzr9UHw5UuB9meTFEDFgeunNgy6nZWpHX4pzkGYotu8DhQ88AdqUG6Yy5wcXgHseKPBUygSgRMJ").get),
    chainId = AddressScheme.current.chainId
  )

  "JSON parsing" in {
    val js = Json.parse("""{
      "id": "Cwtoj31MRz7Xf7HpDbfUetJwjxbymPJw84bZHZdEmwpj",
      "type": 20,
      "version": 1,
      "fee": 100000000,
      "feeAssetId": null,
      "timestamp": 1526287561757,
      "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
      "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
      "generationPeriodStart": 3000,
      "endorsementPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
      "endorsementKeySignature": "OLI6mFSZD949zoVKqRt48SDNNnToWO+vUsmtCdeix7wH5RQkOwYQQvQEhQW/fmNCmsrmbW2IMt7SGjnCvPW9gQ==",
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

  private val sender = TxHelpers.defaultSigner

  "Accepted after the feature activation" in withDomain(
    DeterministicFinality.setFeaturesHeight(BlockchainFeatures.DeterministicFinality -> 3),
    AddrWithBalance.enoughBalances(sender)
  ) { d =>
    val tx = TxHelpers.commitToGeneration(Height(3000), sender)
    d.appendBlockE(tx) should produce("Deterministic Finality & RIDE V9 feature has not been activated yet")
    d.appendBlock()
    d.appendBlock(tx)
  }

  "Generator deposit taken and returned" in withDomain(
    DeterministicFinality.configure(x => x.copy(generationPeriod = 3)),
    AddrWithBalance.enoughBalances(sender)
  ) { d =>
    info("Deposit for one period")
    val currPeriodTx = TxHelpers.commitToGeneration(Height(3), sender)
    d.appendBlock(currPeriodTx)
    d.blockchain.wavesPortfolio(sender.toAddress).generationDeposit shouldBe CommitToGenerationTransaction.DepositInWavelets

    d.appendBlock()
    d.blockchain.height shouldBe 3

    info("Deposit for two periods")
    val nextPeriodTx = TxHelpers.commitToGeneration(Height(6), sender)
    d.appendBlock(nextPeriodTx)
    val wavesPortfolio = d.blockchain.wavesPortfolio(sender.toAddress)
    wavesPortfolio.generationDeposit shouldBe 2 * CommitToGenerationTransaction.DepositInWavelets
    wavesPortfolio.spendableBalance shouldBe (wavesPortfolio.balance - wavesPortfolio.generationDeposit)

    (5 to 6).foreach(_ => d.appendBlock())

    info("Deposit for one period if not committed for next")
    d.blockchain.wavesPortfolio(sender.toAddress).generationDeposit shouldBe CommitToGenerationTransaction.DepositInWavelets
  }
}
