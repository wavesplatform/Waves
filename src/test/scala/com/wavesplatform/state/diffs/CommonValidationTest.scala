package com.wavesplatform.state.diffs

import com.wavesplatform.db.WithState
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.lang.v1.Terms.Typed
import com.wavesplatform.settings.{Constants, FunctionalitySettings}
import com.wavesplatform.state.EitherExt2
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.account.AddressScheme
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.assets.{IssueTransactionV1, SponsorFeeTransaction}
import scorex.transaction.assets.{IssueTransaction, SmartIssueTransaction, SponsorFeeTransaction, TransferTransaction}
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.smart.script.v1.ScriptV1
import scorex.transaction.transfer._
import scorex.transaction.{GenesisTransaction, Transaction}

class CommonValidationTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with WithState with NoShrink {

  property("disallows double spending") {
    val preconditionsAndPayment: Gen[(GenesisTransaction, TransferTransactionV1)] = for {
      master    <- accountGen
      recipient <- otherAccountGen(candidate = master)
      ts        <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      transfer: TransferTransactionV1 <- wavesTransferGeneratorP(master, recipient)
    } yield (genesis, transfer)

    forAll(preconditionsAndPayment) {
      case ((genesis, transfer)) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, transfer))), TestBlock.create(Seq(transfer))) { blockDiffEi =>
          blockDiffEi should produce("AlreadyInTheState")
        }

        assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(transfer, transfer))) { blockDiffEi =>
          blockDiffEi should produce("AlreadyInTheState")
        }
    }
  }

  property("sponsored transactions should work with smart accounts") {
    val settings = createSettings(
      BlockchainFeatures.FeeSponsorship -> 0,
      BlockchainFeatures.SmartAccounts  -> 0
    )

    forAll(sponsorAndSetScriptGen(sponsorship = true, smartToken = false, smartAccount = true)) {
      case (genesisBlock, transferTx) =>
        withStateAndHistory(settings) { blockchain =>
          val preconditionDiff = BlockDiffer.fromBlock(settings, blockchain, None, genesisBlock).explicitGet()
          blockchain.append(preconditionDiff, genesisBlock)

          val r = CommonValidation.checkFee(blockchain, settings, 1, transferTx)
          r should produce("Transactions from scripted accounts require Waves as fee")
        }
    }
  }

  property("sponsored transactions should work without smart accounts") {
    val settings = createSettings(BlockchainFeatures.FeeSponsorship -> 0)

    forAll(sponsorAndSetScriptGen(sponsorship = true, smartToken = false, smartAccount = false)) {
      case (genesisBlock, transferTx) =>
        withStateAndHistory(settings) { blockchain =>
          val preconditionDiff = BlockDiffer.fromBlock(settings, blockchain, None, genesisBlock).explicitGet()
          blockchain.append(preconditionDiff, genesisBlock)

          val r = CommonValidation.checkFee(blockchain, settings, 1, transferTx)
          r shouldBe 'right
        }
    }
  }

  property("smart accounts should work without sponsored transactions") {
    val settings = createSettings(BlockchainFeatures.SmartAccounts -> 0)

    forAll(sponsorAndSetScriptGen(sponsorship = false, smartToken = false, smartAccount = true)) {
      case (genesisBlock, transferTx) =>
        withStateAndHistory(settings) { blockchain =>
          val preconditionDiff = BlockDiffer.fromBlock(settings, blockchain, None, genesisBlock).explicitGet()
          blockchain.append(preconditionDiff, genesisBlock)

          val r = CommonValidation.checkFee(blockchain, settings, 1, transferTx)
          r should produce("Transactions from scripted accounts require Waves as fee")
        }
    }
  }

  private def sponsorAndSetScriptGen(sponsorship: Boolean, smartToken: Boolean, smartAccount: Boolean) =
    for {
      richAcc      <- accountGen
      recipientAcc <- accountGen
      ts = System.currentTimeMillis()
    } yield {
      val script = ScriptV1(Typed.TRUE).explicitGet()

      val genesisTx = GenesisTransaction.create(richAcc, ENOUGH_AMT, ts).explicitGet()

      val issueTx =
        if (smartToken)
          SmartIssueTransactionV1
            .selfSigned(
              SmartIssueTransactionV1.supportedVersions.head,
              AddressScheme.current.chainId,
              richAcc,
              "test".getBytes(),
              "desc".getBytes(),
              Long.MaxValue,
              2,
              reissuable = false,
              Some(script),
              Constants.UnitsInWave,
              ts
            )
            .explicitGet()
        else
          IssueTransaction
            .create(richAcc, "test".getBytes(), "desc".getBytes(), Long.MaxValue, 2, reissuable = false, Constants.UnitsInWave, ts)
            .explicitGet()

      val transferWavesTx = TransferTransactionV1
        .create(None, richAcc, recipientAcc, 10 * Constants.UnitsInWave, ts, None, 1 * Constants.UnitsInWave, Array.emptyByteArray)
        .explicitGet()

      val transferAssetTx = TransferTransactionV1
        .create(Some(issueTx.id()), richAcc, recipientAcc, 100, ts, None, 1 * Constants.UnitsInWave, Array.emptyByteArray)
        .explicitGet()

      val sponsorTx =
        if (sponsorship)
          Seq(
            SponsorFeeTransaction
              .create(1, richAcc, issueTx.id(), Some(10), Constants.UnitsInWave, ts)
              .explicitGet()
          )
        else Seq.empty

      val setScriptTx =
        if (smartAccount)
          Seq(
            SetScriptTransaction
              .selfSigned(
                SetScriptTransaction.supportedVersions.head,
                recipientAcc,
                Some(script),
                1 * Constants.UnitsInWave,
                ts
              )
              .explicitGet()
          )
        else Seq.empty

      val transferAssetsBackTx = TransferTransactionV1
        .create(
          Some(issueTx.id()),
          recipientAcc,
          richAcc,
          1,
          ts,
          Some(issueTx.id()),
          10,
          Array.emptyByteArray
        )
        .explicitGet()

      (TestBlock.create(Vector[Transaction](genesisTx, issueTx, transferWavesTx, transferAssetTx) ++ sponsorTx ++ setScriptTx), transferAssetsBackTx)
    }

  private def createSettings(preActivatedFeatures: (BlockchainFeature, Int)*): FunctionalitySettings =
    TestFunctionalitySettings.Enabled
      .copy(
        preActivatedFeatures = preActivatedFeatures.map { case (k, v) => k.id -> v }(collection.breakOut),
        blocksForFeatureActivation = 1,
        featureCheckBlocksPeriod = 1
      )

}
