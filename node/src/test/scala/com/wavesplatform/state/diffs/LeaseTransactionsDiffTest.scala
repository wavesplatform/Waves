package com.wavesplatform.state.diffs

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain.*
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.*
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.transaction.{GenesisTransaction, TxHelpers, TxVersion}

class LeaseTransactionsDiffTest extends PropSpec with WithDomain {

  private val allowMultipleLeaseCancelTransactionUntilTimestamp = Long.MaxValue / 2
  private val settings =
    TestFunctionalitySettings.Enabled.copy(lastTimeBasedForkParameter = allowMultipleLeaseCancelTransactionUntilTimestamp)

  def total(l: LeaseBalance): Long = l.in - l.out

  property("can lease/cancel lease preserving waves invariant") {

    val sunnyDayLeaseLeaseCancel: Seq[(GenesisTransaction, LeaseTransaction, LeaseCancelTransaction)] = {
      val master    = TxHelpers.signer(1)
      val recipient = TxHelpers.signer(2)

      val genesis = TxHelpers.genesis(master.toAddress)
      for {
        lease       <- Seq(TxHelpers.lease(master, recipient.toAddress), TxHelpers.lease(master, recipient.toAddress, version = TxVersion.V1))
        leaseCancel <- Seq(TxHelpers.leaseCancel(lease.id(), master), TxHelpers.leaseCancel(lease.id(), master, version = TxVersion.V1))
      } yield (genesis, lease, leaseCancel)
    }

    sunnyDayLeaseLeaseCancel.foreach { case (genesis, lease, leaseCancel) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(lease))) { case (totalDiff, _) =>
        val totalPortfolioDiff = totalDiff.portfolios.values.fold(Portfolio())(_.combine(_).explicitGet())
        totalPortfolioDiff.balance shouldBe 0
        total(totalPortfolioDiff.lease) shouldBe 0
        totalPortfolioDiff.effectiveBalance(false).explicitGet() shouldBe 0
        totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)
      }

      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, lease))), TestBlock.create(Seq(leaseCancel))) { case (totalDiff, _) =>
        val totalPortfolioDiff = totalDiff.portfolios.values.fold(Portfolio())(_.combine(_).explicitGet())
        totalPortfolioDiff.balance shouldBe 0
        total(totalPortfolioDiff.lease) shouldBe 0
        totalPortfolioDiff.effectiveBalance(false).explicitGet() shouldBe 0
        totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)
      }
    }
  }

  private val repeatedCancelAllowed   = allowMultipleLeaseCancelTransactionUntilTimestamp - 1
  private val repeatedCancelForbidden = allowMultipleLeaseCancelTransactionUntilTimestamp + 1

  def cancelLeaseTwice(ts: Long): Seq[(GenesisTransaction, TransferTransaction, LeaseTransaction, LeaseCancelTransaction, LeaseCancelTransaction)] = {
    val master    = TxHelpers.signer(1)
    val recipient = TxHelpers.signer(2)

    val genesis = TxHelpers.genesis(master.toAddress, timestamp = ts)

    for {
      lease <- Seq(
        TxHelpers.lease(master, recipient.toAddress, timestamp = ts),
        TxHelpers.lease(master, recipient.toAddress, timestamp = ts, version = TxVersion.V1)
      )
      leaseCancel <- Seq(
        TxHelpers.leaseCancel(lease.id(), master, timestamp = ts + 1),
        TxHelpers.leaseCancel(lease.id(), master, timestamp = ts + 1, version = TxVersion.V1)
      )
      leaseCancel2 <- Seq(
        TxHelpers.leaseCancel(lease.id(), master, fee = leaseCancel.fee.value + 1, timestamp = ts + 1),
        TxHelpers.leaseCancel(lease.id(), master, fee = leaseCancel.fee.value + 1, timestamp = ts + 1, version = TxVersion.V1)
      )
    } yield {
      // ensure recipient has enough effective balance
      val transfer = TxHelpers.transfer(master, recipient.toAddress, 20.waves, timestamp = ts, version = TxVersion.V1)

      (genesis, transfer, lease, leaseCancel, leaseCancel2)
    }
  }

  private val disallowCancelTwice = {
    val ts = repeatedCancelForbidden

    cancelLeaseTwice(ts).map { case (genesis, payment, lease, unlease, unlease2) =>
      (Seq(TestBlock.create(ts, Seq(genesis, payment, lease, unlease))), TestBlock.create(ts, Seq(unlease2)))
    }
  }

  property("cannot cancel lease twice after allowMultipleLeaseCancelTransactionUntilTimestamp") {
    disallowCancelTwice.foreach { case (preconditions, block) =>
      assertDiffEi(preconditions, block, settings) { totalDiffEi =>
        totalDiffEi should produce("Cannot cancel already cancelled lease")
      }
    }
  }

  private val allowCancelTwice = {
    val ts = repeatedCancelAllowed

    cancelLeaseTwice(ts).map { case (genesis, payment, lease, unlease, unlease2) =>
      (Seq(TestBlock.create(ts, Seq(genesis, payment, lease, unlease))), TestBlock.create(ts, Seq(unlease2)))
    }
  }

  property("can cancel lease twice before allowMultipleLeaseCancelTransactionUntilTimestamp") {
    allowCancelTwice.foreach { case (preconditions, block) =>
      assertDiffEi(preconditions, block, settings) { totalDiffEi =>
        totalDiffEi.explicitGet()
      }
    }
  }

  property("cannot lease more than actual balance(cannot lease forward)") {
    val setup: Seq[(GenesisTransaction, LeaseTransaction, LeaseTransaction, Long)] = {
      val master    = TxHelpers.signer(1)
      val recipient = TxHelpers.signer(2)
      val forward   = TxHelpers.signer(3)

      val genesis = TxHelpers.genesis(master.toAddress)

      for {
        lease        <- Seq(TxHelpers.lease(master, recipient.toAddress), TxHelpers.lease(master, recipient.toAddress, version = TxVersion.V1))
        leaseForward <- Seq(TxHelpers.lease(recipient, forward.toAddress), TxHelpers.lease(recipient, forward.toAddress, version = TxVersion.V1))
      } yield (genesis, lease, leaseForward, leaseForward.timestamp)
    }

    setup.foreach { case (genesis, lease, leaseForward, ts) =>
      assertDiffEi(Seq(TestBlock.create(ts, Seq(genesis, lease))), TestBlock.create(ts, Seq(leaseForward)), settings) { totalDiffEi =>
        totalDiffEi should produce("Cannot lease more than own")
      }
    }
  }

  def cancelLeaseOfAnotherSender(
      unleaseByRecipient: Boolean,
      timestamp: Long
  ): Seq[(Seq[GenesisTransaction], LeaseTransaction, LeaseCancelTransaction, Long)] = {
    val master    = TxHelpers.signer(1)
    val recipient = TxHelpers.signer(2)
    val other     = TxHelpers.signer(3)
    val unleaser  = if (unleaseByRecipient) recipient else other

    val genesis = Seq(master, unleaser).map(acc => TxHelpers.genesis(acc.toAddress, timestamp = timestamp))

    for {
      lease <- Seq(
        TxHelpers.lease(master, recipient.toAddress, timestamp = timestamp),
        TxHelpers.lease(master, recipient.toAddress, timestamp = timestamp, version = TxVersion.V1)
      )
      unleaseOtherOrRecipient <- Seq(
        TxHelpers.leaseCancel(lease.id(), unleaser, timestamp = timestamp + 1),
        TxHelpers.leaseCancel(lease.id(), unleaser, timestamp = timestamp + 1, version = TxVersion.V1)
      )
    } yield (genesis, lease, unleaseOtherOrRecipient, timestamp)
  }

  property("cannot cancel lease of another sender after allowMultipleLeaseCancelTransactionUntilTimestamp") {
    for {
      unleaseByRecipient                                   <- Seq(true, false)
      (genesis, lease, unleaseOtherOrRecipient, blockTime) <- cancelLeaseOfAnotherSender(unleaseByRecipient, repeatedCancelForbidden)
    } yield {
      assertDiffEi(
        Seq(TestBlock.create(blockTime, genesis :+ lease)),
        TestBlock.create(blockTime, Seq(unleaseOtherOrRecipient)),
        settings
      ) { totalDiffEi =>
        totalDiffEi should produce("LeaseTransaction was leased by other sender")
      }
    }
  }

  property("can cancel lease of another sender and acquire leasing power before allowMultipleLeaseCancelTransactionUntilTimestamp") {
    cancelLeaseOfAnotherSender(unleaseByRecipient = false, repeatedCancelAllowed).foreach { case (genesis, lease, unleaseOther, blockTime) =>
      assertDiffAndState(Seq(TestBlock.create(genesis :+ lease)), TestBlock.create(blockTime, Seq(unleaseOther)), settings) { case (totalDiff, _) =>
        totalDiff.portfolios.get(lease.sender.toAddress) shouldBe None
        total(totalDiff.portfolios(lease.recipient.asInstanceOf[Address]).lease) shouldBe -lease.amount.value
        total(totalDiff.portfolios(unleaseOther.sender.toAddress).lease) shouldBe lease.amount.value
      }
    }
  }

  property(
    "if recipient cancels lease, it doesn't change leasing component of mining power before allowMultipleLeaseCancelTransactionUntilTimestamp"
  ) {
    cancelLeaseOfAnotherSender(unleaseByRecipient = true, repeatedCancelAllowed).foreach { case (genesis, lease, unleaseRecipient, blockTime) =>
      assertDiffAndState(
        Seq(TestBlock.create(blockTime, genesis :+ lease)),
        TestBlock.create(blockTime, Seq(unleaseRecipient)),
        settings
      ) { case (totalDiff, _) =>
        totalDiff.portfolios.get(lease.sender.toAddress) shouldBe None
        total(totalDiff.portfolios(unleaseRecipient.sender.toAddress).lease) shouldBe 0
      }
    }
  }

  property(s"can pay for cancel lease from the returning funds (before and after ${BlockchainFeatures.BlockV5})") {
    val scenario = {
      val master    = TxHelpers.signer(1)
      val recipient = TxHelpers.signer(2)

      val fee    = 400000L
      val amount = 1000.waves

      val genesis = TxHelpers.genesis(master.toAddress, fee + amount)

      for {
        lease <- Seq(
          TxHelpers.lease(master, recipient.toAddress, amount, fee = fee),
          TxHelpers.lease(master, recipient.toAddress, amount, fee = fee, version = TxVersion.V1)
        )
        leaseCancel <- Seq(
          TxHelpers.leaseCancel(lease.id(), master, fee = fee),
          TxHelpers.leaseCancel(lease.id(), master, fee = fee, version = TxVersion.V1)
        )
      } yield (genesis, lease, leaseCancel, leaseCancel.timestamp + 1)
    }

    scenario.foreach { case (genesis, lease, leaseCancel, ts) =>
      val beforeFailedTxs = TestFunctionalitySettings.Enabled
      val afterFailedTxs = beforeFailedTxs.copy(
        preActivatedFeatures = beforeFailedTxs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0)
      )

      assertDiffEi(Seq(TestBlock.create(ts, Seq(genesis, lease))), TestBlock.create(ts + 1, Seq(leaseCancel)), beforeFailedTxs) { ei =>
        ei.explicitGet()
      }

      assertDiffEi(Seq(TestBlock.create(ts, Seq(genesis, lease))), TestBlock.create(ts + 1, Seq(leaseCancel)), afterFailedTxs) { ei =>
        ei.explicitGet()
      }
    }
  }

  private val totalBalance = 1000.waves
  private val scenario: (Seq[AddrWithBalance], LeaseTransaction) = {
    val sender    = TxHelpers.signer(1)
    val recipient = TxHelpers.signer(2)

    val balances = Seq(AddrWithBalance(sender.toAddress, totalBalance))
    val lease    = TxHelpers.lease(sender, recipient.toAddress, totalBalance, version = TxVersion.V1)

    (balances, lease)
  }

  property(s"fee is not required prior to ${BlockchainFeatures.SynchronousCalls}") {
    val (balances, lt) = scenario
    withDomain(RideV4.setFeaturesHeight(BlockchainFeatures.SynchronousCalls -> 5), balances) { d =>
      d.appendBlock(lt)
    }
  }

  property(s"fee is not required once ${BlockchainFeatures.SynchronousCalls} is activated") {
    val (balances, lt) = scenario

    withDomain(RideV4.setFeaturesHeight(BlockchainFeatures.SynchronousCalls -> 1), balances) { d =>
      d.blockchainUpdater.processBlock(d.createBlock(Block.PlainBlockVersion, Seq(lt))) should produce("Cannot lease more than own")
    }
  }
}
