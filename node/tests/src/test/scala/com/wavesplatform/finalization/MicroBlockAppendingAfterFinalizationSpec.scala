package com.wavesplatform.finalization

import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.{GeneratorIndex, GenesisBlockHeight, Height}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{NumericExt, produce}
import com.wavesplatform.transaction.TxHelpers

class MicroBlockAppendingAfterFinalizationSpec extends BaseFinalizationSpec {
  private val generator1     = TxHelpers.signer(0)
  private val generator1Addr = generator1.toAddress
  private val generator1Idx  = GeneratorIndex(0)

  private val generator2     = TxHelpers.signer(1)
  private val generator2Addr = generator2.toAddress
  private val generator2Idx  = GeneratorIndex(1)

  private val baseSettings = DomainPresets.DeterministicFinality.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  private val defaultSettings = baseSettings.configure(
    _.copy(
      generationPeriodLength = 2,
      lightNodeBlockFieldsAbsenceInterval = 0,
      maxValidEndorsers = 1
    )
  )

  private val generators = Seq(generator1, generator2)

  "second microblock appended if first is invalid" - {
    "invalid endorsement" in withDomain(defaultSettings, AddrWithBalance.enoughBalances(generators*)) { d =>
      log.debug("Append block 2 with commitments")
      val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
      val block2WithCommitments = d.createBlock(txs, generator = generator1, strictTime = true)
      d.appender.appendBlock(block2WithCommitments)

      val block3 = d.createBlock(generator = generator1, strictTime = true)
      log.debug("Append block 3")
      d.appender.appendBlock(block3)

      log.debug("Append microblock with conflicting endorsement")
      val microBlockWithTxn1 = d.createMicroBlock(
        signer = Some(generator1),
        finalizationVoting = Some(
          mkFinalizationVoting(valid = Seq(generator1Idx)).signed(
            endorsedId = d.blockchain.blockId(2).value,
            finalizedId = d.blockchain.blockId(GenesisBlockHeight.toInt).value,
            validEndorsers = generator1 // Should not sign its block
          )
        )
      )(TxHelpers.transfer(generator2, generator1Addr))
      d.appendMicroBlockE(microBlockWithTxn1) should produce("Miner can't endorse its own block")

      log.debug("Append microblock without endorsements")
      d.appendMicroBlockE(TxHelpers.transfer(generator2, generator1Addr)) should beRight
    }

    "more than max endorsements" in {
      val generator3    = TxHelpers.signer(2)
      val generator3Idx = GeneratorIndex(2)
      val generators    = Seq(generator1, generator2, generator3)
      withDomain(defaultSettings, AddrWithBalance.enoughBalances(generators*)) { d =>
        log.debug("Append block 2 with commitments")
        val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
        val block2WithCommitments = d.createBlock(txs, generator = generator1, strictTime = true)
        d.appender.appendBlock(block2WithCommitments)

        val block3 = d.createBlock(generator = generator1, strictTime = true)
        log.debug("Append block 3")
        d.appender.appendBlock(block3)

        log.debug("Append microblock with conflicting endorsement")
        val microBlockWithTxn1 = d.createMicroBlock(
          signer = Some(generator1),
          finalizationVoting = Some(
            mkFinalizationVoting(valid = Seq(generator1Idx, generator2Idx, generator3Idx)).signed(
              endorsedId = d.blockchain.blockId(2).value,
              finalizedId = d.blockchain.blockId(GenesisBlockHeight.toInt).value,
              validEndorsers = generator2,
              generator3
            )
          )
        )(TxHelpers.transfer(generator2, generator1Addr))
        d.appendMicroBlockE(microBlockWithTxn1) should produce("Too many valid endorsements")

        log.debug("Append microblock without endorsements")
        d.appendMicroBlockE(TxHelpers.transfer(generator2, generator1Addr)) should beRight
      }
    }

    "duplicate conflicting endorsement" in withDomain(defaultSettings, AddrWithBalance.enoughBalances(generators*)) { d =>
      log.debug("Append block 2 with commitments")
      val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
      val block2WithCommitments = d.createBlock(txs, generator = generator1, strictTime = true)
      d.appender.appendBlock(block2WithCommitments)

      val block3 = d.createBlock(generator = generator1, strictTime = true)
      log.debug("Append block 3")
      d.appender.appendBlock(block3)

      log.debug("Append microblock with conflicting endorsement")
      val microBlockWithTxn1 = d.createMicroBlock(
        signer = Some(generator1),
        finalizationVoting = Some(mkFinalizationVoting().withConflict(generator2, generator2Idx, block2WithCommitments.id()))
      )(TxHelpers.transfer(generator2, generator1Addr))
      d.appendMicroBlock(microBlockWithTxn1)

      log.debug("Can't append microblock with same conflicting endorsement")
      val microBlockWithTxn2 = d.createMicroBlock(
        signer = Some(generator1),
        finalizationVoting = Some(mkFinalizationVoting().withConflict(generator2, generator2Idx, block2WithCommitments.id()))
      )(TxHelpers.transfer(generator2, generator1Addr))
      d.appendMicroBlockE(microBlockWithTxn2) should produce("Duplicate conflicting endorser indexes")

      log.debug("Append microblock without endorsements")
      d.appendMicroBlockE(TxHelpers.transfer(generator2, generator1Addr)) should beRight
    }
  }

  "reaching, losing and reaching again finalization, calculation in keyblock" in {
    val generator3    = TxHelpers.signer(2)
    val generator3Idx = GeneratorIndex(2)

    val generators = Seq(generator1, generator2, generator3)
    val initBalances = Seq(
      AddrWithBalance(generator1.toAddress, 5000.waves),
      AddrWithBalance(generator2.toAddress, 2000.waves),
      AddrWithBalance(generator3.toAddress, 3000.waves)
    )

    withDomain(defaultSettings, initBalances) { d =>
      val genesisBlockId = d.blockchain.lastBlockId.value

      log.debug("Append block 2 with commitments")
      val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
      val block2WithCommitments = d.createBlock(txs, generator = generator2, strictTime = true)
      d.appender.appendBlock(block2WithCommitments)

      log.debug("Append block 3")
      val block3 = d.createBlock(generator = generator2, strictTime = true)
      d.appender.appendBlock(block3)

      log.debug("Append microblock with valid endorsements, reaching finalization")
      val microBlockWithTxn1 = d.createMicroBlock(
        signer = Some(generator2),
        finalizationVoting = Some(
          mkFinalizationVoting(valid = Seq(generator1Idx))
            .signed(endorsedId = block2WithCommitments.id(), finalizedId = genesisBlockId, validEndorsers = generator1)
        )
      )(TxHelpers.transfer(generator1, generator2Addr))
      d.appendMicroBlockE(microBlockWithTxn1) should beRight

      log.debug("Append microblock with conflicting endorsement, losing finalization")
      val microBlockWithTxn2 = d.createMicroBlock(
        signer = Some(generator2),
        finalizationVoting = Some(mkFinalizationVoting().withConflict(generator1, generator1Idx, block2WithCommitments.id()))
      )(TxHelpers.transfer(generator1, generator2Addr))
      d.appendMicroBlockE(microBlockWithTxn2) should beRight

      log.debug("Append microblock with valid endorsement, reaching finalization again")
      val microBlockWithTxn3 = d.createMicroBlock(
        signer = Some(generator2),
        finalizationVoting = Some(
          mkFinalizationVoting(valid = Seq(generator3Idx))
            .signed(endorsedId = block2WithCommitments.id(), finalizedId = genesisBlockId, validEndorsers = generator3)
        )
      )(TxHelpers.transfer(generator1, generator2Addr))
      d.appendMicroBlockE(microBlockWithTxn3) should beRight

      log.debug("Append block 4")
      d.appender.appendBlock(d.createBlock(generator = generator2, strictTime = true))
      d.allFinalizedHeightIs(2)
    }
  }

  "checks BLS signature in microblock" in {
    val generators = Seq(generator1, generator2)
    val initBalances = Seq(
      AddrWithBalance(generator1.toAddress, 5000.waves),
      AddrWithBalance(generator2.toAddress, 2000.waves)
    )

    withDomain(defaultSettings, initBalances) { d =>
      val genesisBlockId = d.blockchain.lastBlockId.value

      log.debug("Append block 2 with commitments")
      val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
      val block2WithCommitments = d.createBlock(txs, generator = generator2, strictTime = true)
      d.appender.appendBlock(block2WithCommitments)

      log.debug("Append block 3")
      val block3 = d.createBlock(generator = generator2, strictTime = true)
      d.appender.appendBlock(block3)

      log.debug("Append microblock with wrong BLS signature")
      val unknownGenerator = TxHelpers.signer(1000)
      val microBlock1 = d.createMicroBlock(
        signer = Some(generator2),
        finalizationVoting = Some(
          mkFinalizationVoting(valid = Seq(generator1Idx))
            .signed(endorsedId = block3.id(), finalizedId = genesisBlockId, validEndorsers = unknownGenerator)
        )
      )(TxHelpers.transfer(generator1, generator2Addr))
      d.appendMicroBlockE(microBlock1) should produce("Wrong BLS signature")
    }
  }
}
