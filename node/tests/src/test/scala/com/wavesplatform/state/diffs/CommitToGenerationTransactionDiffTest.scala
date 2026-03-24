package com.wavesplatform.state.diffs

import com.wavesplatform.account.KeyPair
import com.wavesplatform.consensus.GeneratingBalanceProvider
import com.wavesplatform.crypto
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsPublicKey}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.Height
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.{DeterministicFinality, WavesSettingsOps}
import com.wavesplatform.transaction.{CommitToGenerationTransaction, Proofs, TxHelpers}

class CommitToGenerationTransactionDiffTest extends FreeSpec with WithDomain {
  private val sender                 = TxHelpers.defaultSigner
  private val generationPeriodLength = 8
  private val defaultSettings        = DeterministicFinality.configure(_.copy(generationPeriodLength = generationPeriodLength))

  "Accepted on the feature activation height, first period starts at activation_height+generation_period+1" in {
    val activationHeight = Height(3)
    withDomain(
      defaultSettings.setFeaturesHeight(BlockchainFeatures.DeterministicFinality -> activationHeight.toInt),
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

    log.info("Deposit for one period if not committed for next")
    d.blockchain.wavesPortfolio(sender.toAddress).generationDeposit shouldBe CommitToGenerationTransaction.DepositInWavelets
  }

  "Can't commit" - {
    "with wrong next period start height" in withDomain(DeterministicFinality, AddrWithBalance.enoughBalances(sender)) { d =>
      d.appendBlockE(TxHelpers.commitToGeneration(Height(1), sender)) should produce("Expected the next period start height")
      d.appendBlockE(TxHelpers.commitToGeneration(Height(3002), sender)) should produce("Expected the next period start height")
    }

    "with invalid endorser public key" in withDomain(DeterministicFinality, AddrWithBalance.enoughBalances(sender)) { d =>
      val unsignedTx = TxHelpers
        .commitToGeneration(Height(3001), sender)
        .copy(endorserPublicKey = BlsPublicKey(Array.fill[Byte](BlsPublicKey.SizeInBytes)(0)).value)
      val signedTx = unsignedTx.copy(proofs = Proofs(crypto.sign(sender.privateKey, unsignedTx.bodyBytes())))

      d.appendBlockE(signedTx) should produce("Invalid endorser public key")
    }

    "with invalid commitment signature" in {
      val newGenerator     = TxHelpers.signer(1006)
      val otherGeneratorKp = BlsKeyPair(TxHelpers.signer(1007).privateKey)
      withDomain(
        DeterministicFinality,
        Seq(
          AddrWithBalance(sender.toAddress, 1000000.waves),
          AddrWithBalance(newGenerator.toAddress, 10000.waves)
        )
      ) { d =>
        val periodStart = Height(3001)
        val unsignedTx = TxHelpers
          .commitToGeneration(periodStart, newGenerator)
          .copy(commitmentSignature = CommitToGenerationTransaction.mkPopSignature(otherGeneratorKp, periodStart))
        val signedTx = unsignedTx.copy(proofs = Proofs(crypto.sign(newGenerator.privateKey, unsignedTx.bodyBytes())))

        d.appendBlockE(unsignedTx) should produce("Proof doesn't validate as signature")
        d.appendBlockE(signedTx) should produce("Invalid commitment signature")
      }
    }

    "twice" in withDomain(DeterministicFinality, AddrWithBalance.enoughBalances(sender)) { d =>
      log.info("First")
      d.appendBlock(TxHelpers.commitToGeneration(Height(3001), sender))

      log.info("Second")
      d.appendBlockE(TxHelpers.commitToGeneration(Height(3001), sender)) should produce("is already committed")
    }

    "public BLS key twice" in withDomain(DeterministicFinality, AddrWithBalance.enoughBalances(sender, TxHelpers.secondSigner)) { d =>
      def mkTx(sender: KeyPair, blsKp: BlsKeyPair): CommitToGenerationTransaction = {
        val baseTx = TxHelpers.commitToGeneration(Height(3001), sender)
        val withPop = baseTx.copy(
          endorserPublicKey = blsKp.publicKey,
          commitmentSignature = CommitToGenerationTransaction.mkPopSignature(blsKp, baseTx.generationPeriodStart)
        )

        withPop.copy(proofs = Proofs(crypto.sign(sender.privateKey, withPop.bodyBytes())))
      }

      log.debug("First")
      val blsKP = BlsKeyPair(sender.privateKey)
      d.appendBlock(mkTx(sender, blsKP))

      log.debug("Second")
      d.appendBlockE(mkTx(TxHelpers.secondSigner, blsKP)) should produce("is already committed, try another key")
    }

    "with insufficient balance" in {
      val newGenerator = TxHelpers.signer(1005)
      val txFee        = 1.waves
      withDomain(
        DeterministicFinality.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance),
        Seq(
          AddrWithBalance(sender.toAddress, 1000000.waves),
          AddrWithBalance(
            newGenerator.toAddress,
            GeneratingBalanceProvider.MinimalEffectiveBalanceForGenerator2 + CommitToGenerationTransaction.DepositInWavelets + txFee - 1
          )
        )
      ) { d =>
        val tx = TxHelpers.commitToGeneration(Height(3001), newGenerator, fee = txFee)

        d.appendBlockE(tx) should produce(
          s"is less than ${GeneratingBalanceProvider.MinimalEffectiveBalanceForGenerator2} required for block generation"
        )
      }
    }
  }
}
