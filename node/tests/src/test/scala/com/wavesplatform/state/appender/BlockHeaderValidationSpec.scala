package com.wavesplatform.state.appender

import com.wavesplatform.block.Block
import com.wavesplatform.crypto
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.Height
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.TxValidationError.GenericError

class BlockHeaderValidationSpec extends FlatSpec with WithDomain {
  private val miner   = TxHelpers.signer(0)
  private val feature = 100.toShort

  private def blockWithVotes(d: Domain, featureVotes: Seq[Short]): Block = {
    val b         = d.createBlock(strictTime = true, generator = miner)
    val withVotes = b.copy(header = b.header.copy(featureVotes = featureVotes))
    withVotes.copy(signature = crypto.sign(miner.privateKey, withVotes.bodyBytes()))
  }

  private val notEnforcing: WavesSettings = DomainPresets.ConsensusImprovements

  private def enforcingAt(activationHeight: Int): WavesSettings =
    notEnforcing.setFeaturesHeight(BlockchainFeatures.FinalityImprovements -> activationHeight)

  "Block header validation" should "reject a proto block with duplicate feature votes" in
    withDomain(enforcingAt(0), AddrWithBalance.enoughBalances(miner)) { d =>
      val b = blockWithVotes(d, Seq(feature, feature))
      b.header.version shouldBe Block.ProtoBlockVersion
      b.signatureValid() shouldBe true

      d.appender.appendBlockWithoutFallback(b) should matchPattern {
        case Left(GenericError(err)) if err.contains("Duplicates in feature votes") =>
      }
      d.lastBlockId should not be b.id()
    }

  it should "reject a proto block with unsorted feature votes" in
    withDomain(enforcingAt(0), AddrWithBalance.enoughBalances(miner)) { d =>
      d.appender.appendBlockWithoutFallback(blockWithVotes(d, Seq(feature, (feature - 1).toShort))) should matchPattern {
        case Left(GenericError(err)) if err.contains("Unsorted feature votes") =>
      }
    }

  it should "accept a proto block with valid feature votes" in
    withDomain(enforcingAt(0), AddrWithBalance.enoughBalances(miner)) { d =>
      val b = blockWithVotes(d, Seq((feature - 1).toShort, feature))
      d.appender.appendBlockWithoutFallback(b) shouldBe a[Right[?, ?]]
      d.lastBlockId shouldBe b.id()
    }

  it should "not be enforced before Finality Improvements activation" in {
    val enforceAfter = 3
    withDomain(enforcingAt(enforceAfter + 1), AddrWithBalance.enoughBalances(miner)) { d =>
      d.blockchain.height shouldBe 1
      d.appender.appendBlock(d.createBlock(strictTime = true, generator = miner))

      // Height 3: not activated yet, the duplicate vote is counted twice
      val atEnforceHeight = blockWithVotes(d, Seq(feature, feature))
      d.appender.appendBlockWithoutFallback(atEnforceHeight) shouldBe a[Right[?, ?]]
      d.blockchain.height shouldBe enforceAfter

      // Height 4: enforced
      d.appender.appendBlockWithoutFallback(blockWithVotes(d, Seq(feature, feature))) should matchPattern {
        case Left(GenericError(err)) if err.contains("Duplicates in feature votes") =>
      }

      d.appender.appendBlock(d.createBlock(strictTime = true, generator = miner)) // Persists the block at height 3
      d.rocksDBWriter.featureVotes(Height(enforceAfter)).get(feature) shouldBe Some(2)
    }
  }

  it should "allow a rollback of a persisted block with duplicate feature votes accepted before the enforcement" in
    withDomain(notEnforcing, AddrWithBalance.enoughBalances(miner)) { d =>
      d.appender.appendBlock(d.createBlock(strictTime = true, generator = miner))
      val rollbackTarget = d.lastBlockId
      d.blockchain.height shouldBe 2

      val duplicateVotes = blockWithVotes(d, Seq(feature, feature))
      d.appender.appendBlockWithoutFallback(duplicateVotes) shouldBe a[Right[?, ?]]
      d.appender.appendBlock(d.createBlock(strictTime = true, generator = miner)) // Persists the block at height 3
      val liquid = d.lastBlockId

      d.rocksDBWriter.height shouldBe 3
      d.rocksDBWriter.featureVotes(Height(3)).get(feature) shouldBe Some(2)

      val discarded = d.rollbackTo(rollbackTarget)
      discarded.map(_.block.id()) shouldBe Seq(duplicateVotes.id(), liquid)
      discarded.head.block.header.featureVotes shouldBe Seq(feature, feature)

      d.blockchain.height shouldBe 2
      d.rocksDBWriter.height shouldBe 2
      d.lastBlockId shouldBe rollbackTarget
      d.blockchain.featureVotes(Height(2)).get(feature) shouldBe None

      // Same as ExtensionAppender does when it fails to apply a fork
      discarded.foreach { x =>
        d.blockchainUpdater.processBlock(x.block, x.hitSource, x.snapshot, x.generatorSet) shouldBe a[Right[?, ?]]
      }
      d.lastBlockId shouldBe liquid
      d.rocksDBWriter.featureVotes(Height(3)).get(feature) shouldBe Some(2)
    }
}
