package com.wavesplatform.state.diffs

import com.wavesplatform.TestValues
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.Height
import com.wavesplatform.test.*
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.transaction.{CommitToGenerationTransaction, GenesisTransaction, TxHelpers, TxVersion}

class BalanceDiffValidationTest extends PropSpec with WithState {
  val ownLessThatLeaseOut: (GenesisTransaction, TransferTransaction, LeaseTransaction, LeaseTransaction, TransferTransaction) = {
    val master = TxHelpers.signer(1)
    val alice  = TxHelpers.signer(2)
    val bob    = TxHelpers.signer(3)
    val cooper = TxHelpers.signer(4)

    val fee                      = 400000
    val masterTransferAmount     = 1000.waves
    val aliceLeaseToBobAmount    = 500.waves
    val masterLeaseToAliceAmount = 750.waves

    val genesis                = TxHelpers.genesis(master.toAddress)
    val masterTransfersToAlice = TxHelpers.transfer(master, alice.toAddress, masterTransferAmount, fee = fee, version = TxVersion.V1)
    val aliceLeasesToBob       = TxHelpers.lease(alice, bob.toAddress, aliceLeaseToBobAmount)
    val masterLeasesToAlice    = TxHelpers.lease(master, alice.toAddress, masterLeaseToAliceAmount)
    val aliceTransfersMoreThanOwnsMinusLeaseOut =
      TxHelpers.transfer(alice, cooper.toAddress, masterTransferAmount - fee - aliceLeaseToBobAmount, fee = fee, version = TxVersion.V1)

    (genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice, aliceTransfersMoreThanOwnsMinusLeaseOut)
  }

  property("can transfer more than own-leaseOut before allow-leased-balance-transfer-until") {
    val settings = TestFunctionalitySettings.Enabled.copy(blockVersion3AfterHeight = 4)

    val (genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice, aliceTransfersMoreThanOwnsMinusLeaseOut) = ownLessThatLeaseOut
    assertDiffEi(
      Seq(TestBlock.create(Seq(genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice))),
      TestBlock.create(Seq(aliceTransfersMoreThanOwnsMinusLeaseOut)),
      settings
    ) { snapshotEi =>
      snapshotEi.explicitGet()
    }
  }

  property("cannot transfer more than own-leaseOut after allow-leased-balance-transfer-until") {
    val settings = TestFunctionalitySettings.Enabled.copy(blockVersion3AfterHeight = 4)

    val (genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice, aliceTransfersMoreThanOwnsMinusLeaseOut) = ownLessThatLeaseOut
    assertDiffEi(
      Seq(
        TestBlock.create(Seq(genesis)),
        TestBlock.create(Seq()),
        TestBlock.create(Seq()),
        TestBlock.create(Seq()),
        TestBlock.create(Seq(masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice))
      ),
      TestBlock.create(Seq(aliceTransfersMoreThanOwnsMinusLeaseOut)),
      settings
    ) { snapshotEi =>
      snapshotEi should produce("trying to spend leased money")
    }
  }

  property("commit to generation") {
    val settings = DomainPresets.DeterministicFinality.blockchainSettings.functionalitySettings.copy(generationPeriodLength = 3)

    val notBlockedAmount = 100_000.waves
    val initBalance      = notBlockedAmount + CommitToGenerationTransaction.DepositInWavelets + TestValues.commitToGenerationFee

    assertDiffEiTraced(
      Seq(TestBlock.create(Seq(TxHelpers.genesis(TxHelpers.defaultAddress, amount = initBalance)))),
      TestBlock.create(Seq(TxHelpers.commitToGeneration(Height(4)))),
      settings
    ) { snapshotEi =>
      snapshotEi.resultE.explicitGet()
    }
  }

  property("cannot transfer more than own-generationDeposit") {
    val settings = DomainPresets.DeterministicFinality.blockchainSettings.functionalitySettings.copy(generationPeriodLength = 3)

    val notBlockedAmount = 100_000.waves
    val initBalance =
      notBlockedAmount + CommitToGenerationTransaction.DepositInWavelets + TestValues.commitToGenerationFee + TestValues.fee // for transfer

    val transferAmount = notBlockedAmount + 1
    assertDiffEiTraced(
      Seq(
        TestBlock.create(Seq(TxHelpers.genesis(TxHelpers.defaultAddress, amount = initBalance))),
        TestBlock.create(Seq(TxHelpers.commitToGeneration(Height(4))))
      ),
      TestBlock.create(Seq(TxHelpers.transfer(amount = transferAmount))),
      settings
    ) { snapshotEi =>
      snapshotEi.resultE should produce("trying to spend a deposit")
    }
  }
}
