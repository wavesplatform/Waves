package com.wavesplatform.transaction

import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
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
    endorserPublicKey = BlsPublicKey(Base58.decode("6CagLT3FjEcaNHPYCaG2dcfEfzDj6ynVeZbxbLHkHdfzvbfBmBMkkatTYcBXD9cHMU")),
    generationPeriodStart = Height(3000),
    timestamp = 1526287561757L,
    fee = TxPositiveAmount.unsafeFrom(100000000),
    commitmentSignature = BlsSignature(
      Base58.decode(
        "oJUBPLXnqejpwkkifzBbyQp63mPwypYq9GV7eAYqQGAvsE2LxU6csrrwLWgK1HdW28Ygku7vfkcMW1TCDCFymVXoqi7SpCwWGp3P6gegHusSPBsuVQQiQ5BWTYpUpSJjiBL"
      )
    ).explicitGet(),
    proofs = Proofs(ByteStr.decodeBase58("28kE1uN1pX2bwhzr9UHw5UuB9meTFEDFgeunNgy6nZWpHX4pzkGYotu8DhQ88AdqUG6Yy5wcXgHseKPBUygSgRMJ").get),
    chainId = AddressScheme.current.chainId
  )

  "JSON parsing" in {
    val js = Json.parse("""{
      "id": "55Cy8fzNF8wNQjjtsFhiNCUQkCJL97iaLRYfnEVRpVnr",
      "type": 19,
      "version": 1,
      "fee": 100000000,
      "feeAssetId": null,
      "timestamp": 1526287561757,
      "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
      "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
      "generationPeriodStart": 3000,
      "endorserPublicKey": "6CagLT3FjEcaNHPYCaG2dcfEfzDj6ynVeZbxbLHkHdfzvbfBmBMkkatTYcBXD9cHMU",
      "commitmentSignature": "oJUBPLXnqejpwkkifzBbyQp63mPwypYq9GV7eAYqQGAvsE2LxU6csrrwLWgK1HdW28Ygku7vfkcMW1TCDCFymVXoqi7SpCwWGp3P6gegHusSPBsuVQQiQ5BWTYpUpSJjiBL",
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

  private val sender                 = TxHelpers.defaultSigner
  private val generationPeriodLength = 8
  private val defaultSettings        = DeterministicFinality.configure(_.copy(generationPeriodLength = generationPeriodLength))

  "Accepted on the feature activation height, first period starts at activation_height+generation_period+1" in {
    val activationHeight = Height(3)
    withDomain(
      defaultSettings.setFeaturesHeight(BlockchainFeatures.DeterministicFinality -> activationHeight),
      AddrWithBalance.enoughBalances(sender)
    ) { d =>
      val tx = TxHelpers.commitToGeneration(activationHeight + generationPeriodLength + 1, sender)
      d.appendBlockE(tx) should produce("Deterministic Finality & RIDE V9 feature has not been activated yet")
      d.appendBlock()
      d.appendBlock(tx)
    }
  }

  "Generator deposit taken and returned" in withDomain(
    DeterministicFinality.configure(x => x.copy(generationPeriodLength = 2)), // Periods in test: [3, 4], [5, 6], [7, 8]
    AddrWithBalance.enoughBalances(sender)
  ) { d =>
    log.info("No deposits")
    d.blockchain.wavesPortfolio(sender.toAddress).generationDeposit shouldBe 0L

    log.info("Deposit for one next period")
    val currPeriodTx = TxHelpers.commitToGeneration(Height(3), sender)
    d.appendBlock(currPeriodTx)
    d.blockchain.height shouldBe 2
    d.blockchain.wavesPortfolio(sender.toAddress).generationDeposit shouldBe CommitToGenerationTransaction.DepositInWavelets

    log.info("Deposit for one current period")
    d.appendBlock()
    d.blockchain.height shouldBe 3
    d.blockchain.wavesPortfolio(sender.toAddress).generationDeposit shouldBe CommitToGenerationTransaction.DepositInWavelets

    log.info("Deposit for two periods")
    val nextPeriodTx = TxHelpers.commitToGeneration(Height(5), sender)
    d.appendBlock(nextPeriodTx)
    val wavesPortfolio = d.blockchain.wavesPortfolio(sender.toAddress)
    wavesPortfolio.generationDeposit shouldBe 2 * CommitToGenerationTransaction.DepositInWavelets
    wavesPortfolio.spendableBalance shouldBe (wavesPortfolio.balance - wavesPortfolio.generationDeposit)

    d.appendBlock()
    d.blockchain.height shouldBe 5

    info("Deposit for one period if not committed for next")
    d.blockchain.wavesPortfolio(sender.toAddress).generationDeposit shouldBe CommitToGenerationTransaction.DepositInWavelets
  }

  "Can't commit twice" in withDomain(DeterministicFinality, AddrWithBalance.enoughBalances(sender)) { d =>
    info("First")
    d.appendBlock(TxHelpers.commitToGeneration(Height(3001), sender))

    info("Second")
    d.appendBlockE(TxHelpers.commitToGeneration(Height(3001), sender)) should produce("is already committed")
  }
}
