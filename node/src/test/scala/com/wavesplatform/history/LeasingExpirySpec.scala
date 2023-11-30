package com.wavesplatform.history

import com.wavesplatform.account.{AddressOrAlias, Alias, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{Constants, FunctionalitySettings}
import com.wavesplatform.state.{Blockchain, LeaseBalance}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import org.scalacheck.Gen
import org.scalactic.source.Position

class LeasingExpirySpec extends FreeSpec with WithDomain {
  private val LeasingExpiryActivationHeight = 4
  private val LeasingValidity               = 2

  private val leasingSettings = settings.copy(
    blockchainSettings = DefaultBlockchainSettings.copy(
      functionalitySettings = FunctionalitySettings(
        featureCheckBlocksPeriod = 100,
        blocksForFeatureActivation = 80,
        preActivatedFeatures = Map(
          BlockchainFeatures.SmartAccounts.id   -> 0,
          BlockchainFeatures.LeaseExpiration.id -> LeasingExpiryActivationHeight
        ),
        doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
        leaseExpiration = LeasingValidity
      )
    )
  )

  private val genesis = for {
    lessor         <- accountGen
    aliasRecipient <- accountGen
    ts = ntpTime.getTimestamp()
    maxFeeAmount <- Gen.choose(100000L, 1 * Constants.UnitsInWave)
    transfer     <- transferGeneratorP(ntpTime.getTimestamp(), lessor, aliasRecipient.toAddress, maxFeeAmount)
    alias        <- aliasGen
    createAlias  <- createAliasGen(aliasRecipient, alias, transfer.amount.value, ntpTime.getTimestamp())
    genesisBlock = TestBlock
      .create(
        ts,
        Seq(
          GenesisTransaction.create(lessor.toAddress, Constants.TotalWaves * Constants.UnitsInWave, ntpTime.getTimestamp()).explicitGet(),
          transfer,
          createAlias
        )
      )
      .block

  } yield (lessor, alias, genesisBlock)

  private def lease(sender: KeyPair, recipient: AddressOrAlias): Gen[LeaseTransaction] =
    for {
      amount <- Gen.choose(1 * Constants.UnitsInWave, 1000 * Constants.UnitsInWave)
      fee    <- smallFeeGen
      l      <- createLease(sender, amount, fee, ntpTime.getTimestamp(), recipient)
    } yield l

  private def lease(sender: KeyPair, recipient: AddressOrAlias, amount: Long): Gen[LeaseTransaction] =
    for {
      fee <- smallFeeGen
      l   <- createLease(sender, amount, fee, ntpTime.getTimestamp(), recipient)
    } yield l

  private def blockWithAliases(ref: ByteStr, lessor: KeyPair, alias: Alias): Gen[Block] =
    for {
      addressRecipient <- accountGen
      l1               <- lease(lessor, addressRecipient.toAddress)
      l2               <- lease(lessor, alias)
    } yield TestBlock.create(ntpTime.getTimestamp(), ref, Seq(l1, l2)).block

  private def ensureNoLeases(b: Blockchain, addresses: Set[AddressOrAlias])(implicit pos: Position): Unit = {
    for (aoa <- addresses) {
      b.leaseBalance(b.resolveAlias(aoa).explicitGet()) shouldBe LeaseBalance.empty
    }
  }

  private def ensureEffectiveBalance(b: Blockchain, address: KeyPair, amount: Long)(implicit pos: Position): Unit =
    b.effectiveBalance(address.toAddress, 0) shouldBe amount

  private def mkEmptyBlock(ref: ByteStr): Block = TestBlock.create(ntpNow, ref, Seq.empty).block

  private def leaseRecipients(blocks: Seq[Block]): Set[AddressOrAlias] =
    blocks
      .flatMap(_.transactionData)
      .collect { case lt: LeaseTransaction =>
        lt.recipient
      }
      .toSet

  private val simpleScenario = for {
    (lessor, alias, genesisBlock) <- genesis
    b2                            <- blockWithAliases(genesisBlock.id(), lessor, alias)
    b3 = mkEmptyBlock(b2.id())
    b4 = mkEmptyBlock(b3.id())
    b5 = mkEmptyBlock(b4.id())
  } yield (lessor, alias, genesisBlock, b2, Seq(b3, b4, b5))

  "Upon feature activation" - {
    "expired leases are cancelled" in forAll(simpleScenario) { case (lessor, alias, genesis, b, emptyBlocks) =>
      withDomain(leasingSettings) { d =>
        d.blockchainUpdater.processBlock(genesis) should beRight
        ensureNoLeases(d.blockchainUpdater, Set(lessor.toAddress, alias))
        d.blockchainUpdater.processBlock(b) should beRight
        val leasesToBeCancelled = b.transactionData.collect { case lt: LeaseTransaction => lt }
        leasesToBeCancelled.foreach {
          case lt: LeaseTransaction => d.blockchainUpdater.leaseDetails(lt.id()).map(_.isActive) shouldBe Some(true)
          case _                    =>
        }
        emptyBlocks.take(2).foreach(b => d.blockchainUpdater.processBlock(b) should beRight)
        // activation height: leases should still be active
        d.blockchainUpdater.height shouldEqual LeasingExpiryActivationHeight
        // balance snapshots, however, already reflect cancelled leases
        for (a <- leasesToBeCancelled.map(lt => d.blockchainUpdater.resolveAlias(lt.recipient).explicitGet())) {
          d.blockchainUpdater.balanceSnapshots(a, 1, d.blockchainUpdater.lastBlockId).last.leaseIn shouldBe 0L
        }
        // once new block is appended, leases become cancelled
        d.blockchainUpdater.processBlock(emptyBlocks.last)
        leasesToBeCancelled.foreach {
          case lt: LeaseTransaction => d.blockchainUpdater.leaseDetails(lt.id()).map(_.isActive) shouldBe Some(false)
          case _                    =>
        }
      }
    }
  }

  "Cancel lease transaction" - {
    val validCancel = for {
      (lessor, alias, genesisBlock) <- genesis
      (l1, c1)                      <- leaseAndCancelGeneratorP(lessor, alias, ntpTime.getTimestamp())
      recipient                     <- accountGen
      (l2, c2)                      <- leaseAndCancelGeneratorP(lessor, recipient.toAddress, ntpTime.getTimestamp())
      b2 = TestBlock.create(ntpNow, genesisBlock.id(), Seq(l1, l2)).block
      b3 = mkEmptyBlock(b2.id())
      b4 = TestBlock.create(ntpNow, b3.id(), Seq(c1, c2)).block
      b5 = mkEmptyBlock(b4.id())
    } yield Seq(genesisBlock, b2, b3, b4, b5)

    "is accepted in a block where lease is cancelled" in forAll(validCancel) { blocks =>
      withDomain(leasingSettings) { d =>
        blocks.foreach(b => d.blockchainUpdater.processBlock(b) should beRight)
        // make sure leasing is not cancelled twice
        for (id <- blocks.flatMap(_.transactionData).collect { case lt: LeaseTransaction => lt.id() }) {
          d.blockchainUpdater.leaseDetails(id).map(_.isActive) shouldBe Some(false)
        }

        ensureNoLeases(d.blockchainUpdater, leaseRecipients(blocks))
      }
    }

    val invalidCancel = for {
      (lessor, alias, genesisBlock) <- genesis
      (l1, c1)                      <- leaseAndCancelGeneratorP(lessor, alias, ntpTime.getTimestamp())
      recipient                     <- accountGen
      (l2, c2)                      <- leaseAndCancelGeneratorP(lessor, recipient.toAddress, ntpTime.getTimestamp())
      b2 = TestBlock.create(ntpNow, genesisBlock.id(), Seq(l1, l2)).block
      b3 = mkEmptyBlock(b2.id())
      b4 = mkEmptyBlock(b3.id())
      b5 = TestBlock.create(ntpNow, b4.id(), Seq(c1, c2)).block
    } yield Seq(genesisBlock, b2, b3, b4, b5)

    "is rejected after lease is cancelled" in forAll(invalidCancel) { blocks =>
      withDomain(leasingSettings) { d =>
        blocks.take(4).foreach(b => d.blockchainUpdater.processBlock(b) should beRight)
        d.blockchainUpdater.processBlock(blocks.last) should produce("Cannot cancel already cancelled lease")
      }
    }
  }

  "Leasing cancellation" - {
    val amount     = 1000 * Constants.UnitsInWave
    val halfAmount = amount / 2

    val manyLeases = for {
      (lessor, _, genesisBlock) <- genesis
      alias                     <- accountGen
      l1                        <- lease(lessor, alias.toAddress, amount)
      l2                        <- lease(lessor, alias.toAddress, amount / 2)
      b2 = mkEmptyBlock(genesisBlock.id())
      b3 = mkEmptyBlock(b2.id())
      b4 = TestBlock.create(ntpNow, b3.id(), Seq(l1)).block
      b5 = TestBlock.create(ntpNow, b4.id(), Seq(l2)).block
      b6 = mkEmptyBlock(b5.id())
      b7 = mkEmptyBlock(b6.id())
    } yield (alias, Seq(genesisBlock, b2, b3, b4, b5, b6, b7))

    "should be applied only for expired leases" ignore forAll(manyLeases) { case (alias, blocks) =>
      withDomain(leasingSettings) { d =>
        import d.blockchainUpdater

        // blocks before activation
        blocks.slice(0, 3).foreach(b => blockchainUpdater.processBlock(b) should beRight)
        ensureEffectiveBalance(blockchainUpdater, alias, 0L)

        // block at activation height with lease
        blockchainUpdater.processBlock(blocks(3)) should beRight
        ensureEffectiveBalance(blockchainUpdater, alias, amount)

        // block after activation and before cancellation, including new lease
        blockchainUpdater.processBlock(blocks(4))
        ensureEffectiveBalance(blockchainUpdater, alias, amount + halfAmount)

        // block at height of first lease cancellation, effective balance reflects it
        blockchainUpdater.processBlock(blocks(5))
        ensureEffectiveBalance(blockchainUpdater, alias, halfAmount)

        // block at height of second lease cancellation, effective balance reflects it
        blockchainUpdater.processBlock(blocks(6))
        ensureEffectiveBalance(blockchainUpdater, alias, 0L)
      }
    }
  }

  "Miner" - {
    val amount = 1000 * Constants.UnitsInWave

    val leaseInTheCancelBlock = for {
      (lessor, _, genesisBlock) <- genesis
      miner                     <- accountGen
      l1                        <- lease(lessor, miner.toAddress, amount)
      l2                        <- lease(lessor, miner.toAddress, amount)
      b2 = mkEmptyBlock(genesisBlock.id())
      b3 = mkEmptyBlock(b2.id())
      b4 = TestBlock.create(ntpNow, b3.id(), Seq(l1)).block
      b5 = mkEmptyBlock(b4.id())
      b6 = TestBlock.create(ntpNow, b5.id(), Seq(l2)).block
      b7 = mkEmptyBlock(b6.id())
    } yield (miner, lessor, Seq(genesisBlock, b2, b3, b4, b5, b6, b7))

    "has correct balance when lease transaction is accepted in a block where previous leases are cancelled" ignore forAll(leaseInTheCancelBlock) {
      case (miner, lessor, blocks) =>
        withDomain(leasingSettings) { d =>
          import d.blockchainUpdater

          // blocks before activation
          blocks.slice(0, 3).foreach(b => blockchainUpdater.processBlock(b) should beRight)
          ensureEffectiveBalance(blockchainUpdater, miner, 0L)
          ensureNoLeases(blockchainUpdater, Set(lessor.toAddress, miner.toAddress))

          // effective balance reflects new leases
          blockchainUpdater.processBlock(blocks(3)) should beRight
          ensureEffectiveBalance(blockchainUpdater, miner, amount)

          // blocks after activation and before cancellation
          blockchainUpdater.processBlock(blocks(4)) should beRight

          // effective balance reflects cancelled and new leases
          blockchainUpdater.processBlock(blocks(5)) should beRight
          ensureEffectiveBalance(blockchainUpdater, miner, amount)

          // effective balance not changed
          blockchainUpdater.processBlock(blocks(6)) should beRight
          ensureEffectiveBalance(blockchainUpdater, miner, amount)
        }
    }

    val blockWhereLeaseCancelled = for {
      (lessor, _, genesisBlock) <- genesis
      miner                     <- accountGen
      lease                     <- lease(lessor, miner.toAddress, amount)
      b2 = mkEmptyBlock(genesisBlock.id())
      b3 = mkEmptyBlock(b2.id())
      b4 = TestBlock.create(ntpNow, b3.id(), Seq(lease)).block
      b5 = mkEmptyBlock(b4.id())
      b6 = mkEmptyBlock(b5.id())
    } yield (miner, Seq(genesisBlock, b2, b3, b4, b5, b6))

    "can generate block where lease is cancelled" ignore forAll(blockWhereLeaseCancelled) { case (miner, blocks) =>
      withDomain(leasingSettings) { d =>
        import d.blockchainUpdater

        // blocks before activation
        blocks.slice(0, 3).foreach(b => blockchainUpdater.processBlock(b) should beRight)
        ensureEffectiveBalance(blockchainUpdater, miner, 0L)

        // effective balance reflects new leases
        blockchainUpdater.processBlock(blocks(3)) should beRight
        ensureEffectiveBalance(blockchainUpdater, miner, amount)

        // blocks after activation and before cancellation
        blockchainUpdater.processBlock(blocks(4)) should beRight

        // miner allowed to generate block at cancellation height
        ensureEffectiveBalance(blockchainUpdater, miner, amount)
        blockchainUpdater.processBlock(blocks(5)) should beRight

        // miner not allowed to generate block after cancellation
        ensureEffectiveBalance(blockchainUpdater, miner, 0L)
      }
    }
  }
}
