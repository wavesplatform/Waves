package com.wavesplatform.state.diffs

import com.wavesplatform.account.{Address, Alias, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock.create as block
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.{Asset, GenesisTransaction, TxHelpers, TxVersion}

class MassTransferTransactionDiffTest extends PropSpec with WithState {

  val fs: FunctionalitySettings =
    TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.MassTransfer.id -> 0))

  val baseSetup: (GenesisTransaction, KeyPair) = {
    val master = TxHelpers.signer(1)

    val genesis = TxHelpers.genesis(master.toAddress)

    (genesis, master)
  }

  property("MassTransfer preserves balance invariant") {
    def testDiff(transferCount: Int): Unit = {
      val setup = {
        val (genesis, master) = baseSetup

        val transfers = (1 to transferCount).map(idx => TxHelpers.address(idx + 1) -> (100000L + idx))
        val issue     = TxHelpers.issue(master, ENOUGH_AMT, version = TxVersion.V1)

        Seq(Some(issue.id()), None).map { issueIdOpt =>
          val maybeAsset = Asset.fromCompatId(issueIdOpt)
          val transfer   = TxHelpers.massTransfer(master, transfers, maybeAsset, version = TxVersion.V1)

          (genesis, issue, transfer)
        }
      }

      setup.foreach { case (genesis, issue, transfer) =>
        assertDiffAndState(Seq(block(Seq(genesis, issue))), block(Seq(transfer)), fs) { case (totalDiff, newState) =>
          assertBalanceInvariant(totalDiff)

          val totalAmount = transfer.transfers.map(_.amount.value).sum
          val fees        = issue.fee.value + transfer.fee.value
          transfer.assetId match {
            case aid @ IssuedAsset(_) =>
              newState.balance(transfer.sender.toAddress) shouldBe ENOUGH_AMT - fees
              newState.balance(transfer.sender.toAddress, aid) shouldBe ENOUGH_AMT - totalAmount
            case Waves =>
              newState.balance(transfer.sender.toAddress) shouldBe ENOUGH_AMT - fees - totalAmount
          }
          for (ParsedTransfer(recipient, amount) <- transfer.transfers) {
            if (transfer.sender.toAddress != recipient) {
              transfer.assetId match {
                case aid @ IssuedAsset(_) =>
                  newState.balance(recipient.asInstanceOf[Address], aid) shouldBe amount.value
                case Waves =>
                  newState.balance(recipient.asInstanceOf[Address]) shouldBe amount.value
              }
            }
          }
        }
      }
    }

    import com.wavesplatform.transaction.transfer.MassTransferTransaction.MaxTransferCount as Max
    Seq(0, 1, Max) foreach testDiff // test edge cases
    testDiff(5)
  }

  property("MassTransfer fails on non-existent alias") {
    val setup = {
      val (genesis, master) = baseSetup
      val recipient         = Alias.create("alias").explicitGet()
      val transfer          = TxHelpers.massTransfer(master, Seq(recipient -> 100000L), version = TxVersion.V1)

      (genesis, transfer)
    }

    val (genesis, transfer) = setup
    assertDiffEi(Seq(block(Seq(genesis))), block(Seq(transfer)), fs) { blockDiffEi =>
      blockDiffEi should produce("does not exist")
    }
  }

  property("MassTransfer fails on non-issued asset") {
    val setup = {
      val (genesis, master) = baseSetup
      val recipient         = TxHelpers.address(2)
      val asset             = IssuedAsset(ByteStr.fill(32)(1))
      val transfer =
        TxHelpers.massTransfer(master, Seq(recipient -> 100000L), asset, version = TxVersion.V1)

      (genesis, transfer)
    }

    val (genesis, transfer) = setup
    assertDiffEi(Seq(block(Seq(genesis))), block(Seq(transfer)), fs) { blockDiffEi =>
      blockDiffEi should produce("Attempt to transfer unavailable funds")
    }
  }

  property("MassTransfer cannot overspend funds") {
    val setup = {
      val (genesis, master) = baseSetup
      val recipients        = Seq(2, 3).map(idx => TxHelpers.address(idx) -> (ENOUGH_AMT / 2 + 1))
      val issue             = TxHelpers.issue(master, ENOUGH_AMT, version = TxVersion.V1)
      Seq(Some(issue.id()), None).map { issueIdOpt =>
        val maybeAsset = Asset.fromCompatId(issueIdOpt)
        val transfer   = TxHelpers.massTransfer(master, recipients, maybeAsset, version = TxVersion.V1)

        (genesis, transfer)
      }
    }

    setup.foreach { case (genesis, transfer) =>
      assertDiffEi(Seq(block(Seq(genesis))), block(Seq(transfer)), fs) { blockDiffEi =>
        blockDiffEi should produce("Attempt to transfer unavailable funds")
      }
    }
  }

  property("validation fails prior to feature activation") {
    val setup = {
      val (genesis, master) = baseSetup
      val transfer          = TxHelpers.massTransfer(master, Seq.empty, version = TxVersion.V1)

      (genesis, transfer)
    }
    val settings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.MassTransfer.id -> 10))

    val (genesis, transfer) = setup
    assertDiffEi(Seq(block(Seq(genesis))), block(Seq(transfer)), settings) { blockDiffEi =>
      blockDiffEi should produce("Mass Transfer Transaction feature has not been activated yet")
    }
  }
}
