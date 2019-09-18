package com.wavesplatform.history

import com.wavesplatform.account.{AddressOrAlias, Alias, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{Constants, FunctionalitySettings}
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.state.{Blockchain, LeaseBalance}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalactic.source.Position
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class LeasingExpirySpec extends FreeSpec with ScalaCheckPropertyChecks with WithDomain with Matchers with TransactionGen with NoShrink {
  private val LeasingExpiryActivationHeight = 4
  private val LeasingValidity               = 2

  private val leasingSettings = settings.copy(
    blockchainSettings = DefaultBlockchainSettings.copy(
      functionalitySettings = FunctionalitySettings(
        featureCheckBlocksPeriod = 100,
        blocksForFeatureActivation = 80,
        doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
        leaseExpiration = LeasingValidity,
        preActivatedFeatures = Map(
          BlockchainFeatures.SmartAccounts.id -> 0,
          BlockchainFeatures.LeaseExpiration.id -> LeasingExpiryActivationHeight
        )
      )
    )
  )

  private val genesis = for {
    lessor         <- accountGen
    aliasRecipient <- accountGen
    ts = ntpTime.getTimestamp()
    maxFeeAmount <- Gen.choose(100000L, 1 * Constants.UnitsInWave)
    transfer     <- transferGeneratorP(ntpTime.getTimestamp(), lessor, aliasRecipient, maxFeeAmount)
    alias        <- aliasGen
    createAlias  <- createAliasGen(aliasRecipient, alias, transfer.amount, ntpTime.getTimestamp())
    genesisBlock = TestBlock.create(
      ts,
      Seq(
        GenesisTransaction.create(lessor, Constants.TotalWaves * Constants.UnitsInWave, ntpTime.getTimestamp()).explicitGet(),
        transfer,
        createAlias
      )
    )

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
      l1               <- lease(lessor, addressRecipient)
      l2               <- lease(lessor, alias)
    } yield TestBlock.create(ntpTime.getTimestamp(), ref, Seq(l1, l2))

  private def ensureNoLeases(b: Blockchain, addresses: Set[AddressOrAlias])(implicit pos: Position): Unit = {
    for (aoa <- addresses) {
      b.leaseBalance(b.resolveAlias(aoa).explicitGet()) shouldBe LeaseBalance.empty
    }
  }

  private def ensureEffectiveBalance(b: Blockchain, address: KeyPair, amount: Long)(implicit pos: Position): Unit =
    b.effectiveBalance(address, 0) shouldBe amount

  private def mkEmptyBlock(ref: ByteStr): Block = TestBlock.create(ntpNow, ref, Seq.empty)

  private def activeLeaseIds(b: Blockchain): Set[ByteStr] = b.allActiveLeases.map(_.id()).toSet

  private def leaseRecipients(blocks: Seq[Block]): Set[AddressOrAlias] =
    blocks
      .flatMap(_.transactionData)
      .collect {
        case lt: LeaseTransaction => lt.recipient
      }
      .toSet

  private val simpleScenario = for {
    (lessor, alias, genesisBlock) <- genesis
    b2                            <- blockWithAliases(genesisBlock.uniqueId, lessor, alias)
    b3 = mkEmptyBlock(b2.uniqueId)
    b4 = mkEmptyBlock(b3.uniqueId)
    b5 = mkEmptyBlock(b4.uniqueId)
  } yield (lessor, alias, genesisBlock, b2, Seq(b3, b4, b5))

  "Upon feature activation" - {
    "expired leases are cancelled" in forAll(simpleScenario) {
      case (lessor, alias, genesis, b, emptyBlocks) =>
        withDomain(leasingSettings) { d =>
          d.blockchainUpdater.processBlock(genesis).explicitGet()
          ensureNoLeases(d.blockchainUpdater, Set(lessor.toAddress, alias))
          d.blockchainUpdater.processBlock(b).explicitGet()
          val activeLeases = activeLeaseIds(d.blockchainUpdater)
          activeLeases.size shouldBe 2
          emptyBlocks.take(2).foreach(b => d.blockchainUpdater.processBlock(b).explicitGet())
          // activation height: leases should still be active
          d.blockchainUpdater.height shouldEqual LeasingExpiryActivationHeight
          val leasesToBeCancelled = d.blockchainUpdater.allActiveLeases
          leasesToBeCancelled.map(_.id()).toSet shouldEqual activeLeases
          // balance snapshots, however, already reflect cancelled leases
          for (a <- leasesToBeCancelled.map(lt => d.blockchainUpdater.resolveAlias(lt.recipient).explicitGet())) {
            d.blockchainUpdater.balanceSnapshots(a, 1, d.blockchainUpdater.lastBlockId.get).last.leaseIn shouldBe 0L
          }
          // once new block is appended, leases become cancelled
          d.blockchainUpdater.processBlock(emptyBlocks.last)
          d.blockchainUpdater.allActiveLeases shouldBe 'empty
        }
    }
  }

  "Cancel lease transaction" - {
    val validCancel = for {
      (lessor, alias, genesisBlock) <- genesis
      (l1, c1)                      <- leaseAndCancelGeneratorP(lessor, alias, ntpTime.getTimestamp())
      recipient                     <- accountGen
      (l2, c2)                      <- leaseAndCancelGeneratorP(lessor, recipient.toAddress, ntpTime.getTimestamp())
      b2 = TestBlock.create(ntpNow, genesisBlock.uniqueId, Seq(l1, l2))
      b3 = mkEmptyBlock(b2.uniqueId)
      b4 = TestBlock.create(ntpNow, b3.uniqueId, Seq(c1, c2))
      b5 = mkEmptyBlock(b4.uniqueId)
    } yield Seq(genesisBlock, b2, b3, b4, b5)

    "is accepted in a block where lease is cancelled" in forAll(validCancel) { blocks =>
      withDomain(leasingSettings) { d =>
        blocks.foreach(b => d.blockchainUpdater.processBlock(b).explicitGet())
        // make sure leasing is not cancelled twice
        d.blockchainUpdater.allActiveLeases shouldBe 'empty
        ensureNoLeases(d.blockchainUpdater, leaseRecipients(blocks))
      }
    }

    val invalidCancel = for {
      (lessor, alias, genesisBlock) <- genesis
      (l1, c1)                      <- leaseAndCancelGeneratorP(lessor, alias, ntpTime.getTimestamp())
      recipient                     <- accountGen
      (l2, c2)                      <- leaseAndCancelGeneratorP(lessor, recipient.toAddress, ntpTime.getTimestamp())
      b2 = TestBlock.create(ntpNow, genesisBlock.uniqueId, Seq(l1, l2))
      b3 = mkEmptyBlock(b2.uniqueId)
      b4 = mkEmptyBlock(b3.uniqueId)
      b5 = TestBlock.create(ntpNow, b4.uniqueId, Seq(c1, c2))
    } yield Seq(genesisBlock, b2, b3, b4, b5)

    "is rejected after lease is cancelled" in forAll(invalidCancel) { blocks =>
      withDomain(leasingSettings) { d =>
        blocks.take(4).foreach(b => d.blockchainUpdater.processBlock(b).explicitGet())
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
      l1                        <- lease(lessor, alias, amount)
      l2                        <- lease(lessor, alias, amount / 2)
      b2 = mkEmptyBlock(genesisBlock.uniqueId)
      b3 = mkEmptyBlock(b2.uniqueId)
      b4 = TestBlock.create(ntpNow, b3.uniqueId, Seq(l1))
      b5 = TestBlock.create(ntpNow, b4.uniqueId, Seq(l2))
      b6 = mkEmptyBlock(b5.uniqueId)
      b7 = mkEmptyBlock(b6.uniqueId)
    } yield (alias, Seq(genesisBlock, b2, b3, b4, b5, b6, b7))

    "should be applied only for expired leases" ignore forAll(manyLeases) {
      case (alias, blocks) =>
        withDomain(leasingSettings) {
          case Domain(blockchainUpdater, _) =>
            // blocks before activation
            blocks.slice(0, 3).foreach(b => blockchainUpdater.processBlock(b).explicitGet())
            ensureEffectiveBalance(blockchainUpdater, alias, 0L)

            // block at activation height with lease
            blockchainUpdater.processBlock(blocks(3)).explicitGet()
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
      l1                        <- lease(lessor, miner, amount)
      l2                        <- lease(lessor, miner, amount)
      b2 = mkEmptyBlock(genesisBlock.uniqueId)
      b3 = mkEmptyBlock(b2.uniqueId)
      b4 = TestBlock.create(ntpNow, b3.uniqueId, Seq(l1))
      b5 = mkEmptyBlock(b4.uniqueId)
      b6 = TestBlock.create(ntpNow, b5.uniqueId, Seq(l2))
      b7 = mkEmptyBlock(b6.uniqueId)
    } yield (miner, lessor, Seq(genesisBlock, b2, b3, b4, b5, b6, b7))

    "has correct balance when lease transaction is accepted in a block where previous leases are cancelled" ignore forAll(leaseInTheCancelBlock) {
      case (miner, lessor, blocks) =>
        withDomain(leasingSettings) {
          case Domain(blockchainUpdater, _) =>
            // blocks before activation
            blocks.slice(0, 3).foreach(b => blockchainUpdater.processBlock(b).explicitGet())
            ensureEffectiveBalance(blockchainUpdater, miner, 0L)
            ensureNoLeases(blockchainUpdater, Set(lessor.toAddress, miner))

            // effective balance reflects new leases
            blockchainUpdater.processBlock(blocks(3)).explicitGet()
            ensureEffectiveBalance(blockchainUpdater, miner, amount)

            // blocks after activation and before cancellation
            blockchainUpdater.processBlock(blocks(4)).explicitGet()

            // effective balance reflects cancelled and new leases
            blockchainUpdater.processBlock(blocks(5)).explicitGet()
            ensureEffectiveBalance(blockchainUpdater, miner, amount)

            // effective balance not changed
            blockchainUpdater.processBlock(blocks(6)).explicitGet()
            ensureEffectiveBalance(blockchainUpdater, miner, amount)
        }
    }

    val blockWhereLeaseCancelled = for {
      (lessor, _, genesisBlock) <- genesis
      miner                     <- accountGen
      lease                     <- lease(lessor, miner, amount)
      b2 = mkEmptyBlock(genesisBlock.uniqueId)
      b3 = mkEmptyBlock(b2.uniqueId)
      b4 = TestBlock.create(ntpNow, b3.uniqueId, Seq(lease))
      b5 = mkEmptyBlock(b4.uniqueId)
      b6 = mkEmptyBlock(b5.uniqueId)
    } yield (miner, Seq(genesisBlock, b2, b3, b4, b5, b6))

    "can generate block where lease is cancelled" ignore forAll(blockWhereLeaseCancelled) {
      case (miner, blocks) =>
        withDomain(leasingSettings) {
          case Domain(blockchainUpdater, _) =>
            // blocks before activation
            blocks.slice(0, 3).foreach(b => blockchainUpdater.processBlock(b).explicitGet())
            ensureEffectiveBalance(blockchainUpdater, miner, 0L)

            // effective balance reflects new leases
            blockchainUpdater.processBlock(blocks(3)).explicitGet()
            ensureEffectiveBalance(blockchainUpdater, miner, amount)

            // blocks after activation and before cancellation
            blockchainUpdater.processBlock(blocks(4)).explicitGet()

            // miner allowed to generate block at cancellation height
            ensureEffectiveBalance(blockchainUpdater, miner, amount)
            blockchainUpdater.processBlock(blocks(5)).explicitGet()

            // miner not allowed to generate block after cancellation
            ensureEffectiveBalance(blockchainUpdater, miner, 0L)
        }
    }
  }
}
