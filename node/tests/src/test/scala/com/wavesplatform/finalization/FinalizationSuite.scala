package com.wavesplatform.finalization

import com.wavesplatform.block.{Block, FinalizationVoting}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.state.{Blockchain, GeneratorIndex, GenesisBlockHeight, Height}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{FreeSpec, NumericExt}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.wallet.Wallet
import org.scalactic.source.Position

// TODO: move to valid
class FinalizationSuite extends BaseFinalizationSpec {
  private val seed          = ByteStr("finality-test".getBytes())
  private val thisNodeAcc   = Wallet.generateNewAccount(seed.arr, nonce = 0)
  private val otherNode1Acc = TxHelpers.signer(0)
  private val otherNode2Acc = TxHelpers.signer(1)
  private val otherNode3Acc = TxHelpers.signer(2)

  private val baseSettings = DomainPresets.DeterministicFinality.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  private val defaultSettings = baseSettings
    .copy(
      minerSettings = baseSettings.minerSettings.copy(quorum = 0),
      walletSettings = baseSettings.walletSettings.copy(seed = Some(seed))
    )
    .configure(_.copy(generationPeriodLength = 3))

  "finalized height doesn't decrease" - {
    "increased if voted" - {
      "block" in withDomain(defaultSettings, AddrWithBalance.enoughBalances(otherNode1Acc, thisNodeAcc)) { d =>
        val genesisBlockId = d.blockchain.lastBlockId.value
        d.blockchain.finalizedHeightAt().value shouldBe GenesisBlockHeight
        d.blockchain.finalizedHeight.value shouldBe GenesisBlockHeight

        d.appendBlock()
        d.checkFinalizedHeight()

        log.debug(s"Append block 3 with commitments")
        val endorsers = Seq(otherNode1Acc, thisNodeAcc)
        val block3 = d.createBlock(
          version = Block.ProtoBlockVersion,
          txs = endorsers.map(x => TxHelpers.commitToGeneration(Height(4), x)),
          generator = otherNode1Acc
        )
        d.appendBlock(block3)
        d.checkFinalizedHeight()

        log.debug(s"Append block 4 with votes")
        val votingBlock = d.createBlock(
          version = Block.ProtoBlockVersion,
          txs = Nil,
          generator = otherNode1Acc,
          strictTime = true,
          finalizationVoting = Some(
            mkFinalizationVoting(valid = Seq(GeneratorIndex(1)))
              .signed(endorsedId = block3.id(), finalizedId = genesisBlockId, validEndorsers = thisNodeAcc)
          )
        )
        d.appender.appendBlock(votingBlock)
        d.checkFinalizedHeight(3)

        log.debug("Append block 5")
        d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = otherNode1Acc, strictTime = true))
        d.checkFinalizedHeight(3)
      }

      "microblock" in withDomain(defaultSettings, AddrWithBalance.enoughBalances(otherNode1Acc, otherNode2Acc, thisNodeAcc)) { d =>
        val genesisBlockId = d.blockchain.lastBlockId.value
        d.blockchain.finalizedHeightAt().value shouldBe GenesisBlockHeight
        d.blockchain.finalizedHeight.value shouldBe GenesisBlockHeight

        d.appendBlock()
        d.checkFinalizedHeight()

        log.debug(s"Append block 3 with commitments")
        val endorsers = Seq(otherNode1Acc, thisNodeAcc)
        val block3 = d.createBlock(
          version = Block.ProtoBlockVersion,
          txs = endorsers.map(x => TxHelpers.commitToGeneration(Height(4), x)),
          generator = otherNode1Acc
        )
        d.appendBlock(block3)
        d.checkFinalizedHeight()

        log.debug(s"Append block 4")
        d.appender.appendBlock(
          d.createBlock(
            version = Block.ProtoBlockVersion,
            txs = Nil,
            generator = otherNode1Acc,
            strictTime = true
          )
        )

        log.debug(s"Append microblock with votes")
        val microBlockWithTxn = d.createMicroBlock(
          signer = Some(otherNode1Acc),
          finalizationVoting = Some(
            mkFinalizationVoting(valid = Seq(GeneratorIndex(1)))
              .signed(endorsedId = block3.id(), finalizedId = genesisBlockId, validEndorsers = thisNodeAcc)
          )
        )(TxHelpers.transfer(otherNode2Acc, otherNode3Acc.toAddress))
        d.appendMicroBlock(microBlockWithTxn)
        d.checkFinalizedHeight(3)

        log.debug("Append block 5")
        d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = otherNode1Acc, strictTime = true))
        d.checkFinalizedHeight(3)
      }
    }

    "spending balance after voting doesn't affect finalization" in withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(otherNode1Acc, otherNode2Acc, thisNodeAcc)
    ) { d =>
      val genesisBlockId = d.blockchain.lastBlockId.value
      d.appendBlock()

      log.debug(s"Append block 3 with commitments")
      val endorsers = Seq(otherNode1Acc, otherNode2Acc, thisNodeAcc)
      val block3 = d.createBlock(
        version = Block.ProtoBlockVersion,
        txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(4), x)),
        generator = otherNode1Acc
      )
      d.appendBlock(block3)

      log.debug(s"Append block 4 with votes and spending")
      d.appender.appendBlock(
        d.createBlock(
          version = Block.ProtoBlockVersion,
          txs = Seq(
            TxHelpers.transfer(
              thisNodeAcc,
              to = otherNode2Acc.toAddress,
              amount = d.blockchain.wavesPortfolio(thisNodeAcc.toAddress).spendableBalance - 1.waves,
              fee = 1.waves
            )
          ),
          generator = otherNode1Acc,
          strictTime = true,
          finalizationVoting = Some(
            mkFinalizationVoting(valid = Seq(GeneratorIndex(2)))
              .signed(endorsedId = block3.id(), finalizedId = genesisBlockId, validEndorsers = thisNodeAcc)
          )
        )
      )

      log.debug("Append block 5")
      d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = otherNode1Acc, strictTime = true))
      d.checkFinalizedHeight(3)
    }

    "same finalized height if not voted" in withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(otherNode1Acc, thisNodeAcc)
    ) { d =>
      d.appendBlock()

      log.debug(s"Append block 3 with commitments")
      val endorsers = Seq(otherNode1Acc, thisNodeAcc)
      val block3 = d.createBlock(
        version = Block.ProtoBlockVersion,
        txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(4), x)),
        generator = otherNode1Acc
      )
      d.appendBlock(block3)

      log.debug(s"Append block 4 without votes (only miner committed)")
      d.appender.appendBlock(
        d.createBlock(
          version = Block.ProtoBlockVersion,
          txs = Nil,
          generator = otherNode1Acc,
          strictTime = true,
          finalizationVoting = None
        )
      )

      log.debug("Append block 5")
      d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = otherNode1Acc, strictTime = true))
      d.checkFinalizedHeight()
    }

    "same finalized height if mines a generator not from generator set" ignore {} // TODO: implement

    "increased if surpass maxRollback blocks even no votes" in withDomain(
      defaultSettings.copy(synchronizationSettings = defaultSettings.synchronizationSettings.copy(maxRollback = 2)),
      AddrWithBalance.enoughBalances(otherNode1Acc, thisNodeAcc)
    ) { d =>
      d.appendBlock()

      log.debug(s"Append block 3 with commitments")
      val endorsers = Seq(otherNode1Acc, thisNodeAcc)
      val block3 = d.createBlock(
        version = Block.ProtoBlockVersion,
        txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(4), x)),
        generator = otherNode1Acc
      )
      d.appendBlock(block3)

      log.debug(s"Append block 4 without votes (only miner committed)")
      d.appender.appendBlock(
        d.createBlock(
          version = Block.ProtoBlockVersion,
          txs = Nil,
          generator = otherNode1Acc,
          strictTime = true,
          finalizationVoting = None
        )
      )

      log.debug("Append block 5")
      d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = otherNode1Acc, strictTime = true))
      d.checkFinalizedHeight(3) // 5 - maxRollback = 3
    }

    "increased with less votes after conflict endorsement" in withDomain(
      defaultSettings,
      Seq(otherNode1Acc, otherNode2Acc, otherNode3Acc, thisNodeAcc).map(kp => AddrWithBalance(kp.toAddress, 200_100.1.waves))
    ) { d =>
      val genesisBlockId = d.blockchain.lastBlockId.value
      d.appendBlock()

      log.debug(s"Append block 3 with commitments")
      val endorsers = Seq(otherNode1Acc, otherNode2Acc, otherNode3Acc, thisNodeAcc)
      val block3 = d.createBlock(
        version = Block.ProtoBlockVersion,
        txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(4), x)),
        generator = otherNode1Acc
      )
      d.appendBlock(block3)
      val endorsedBlock = block3
      val endorsedId    = endorsedBlock.id()

      log.debug(s"Append block 4 with conflict vote")
      d.appender.appendBlock(
        d.createBlock(
          version = Block.ProtoBlockVersion,
          txs = Nil,
          generator = otherNode3Acc,
          strictTime = true,
          finalizationVoting = Some(
            mkFinalizationVoting(
              valid = Seq(GeneratorIndex(1)),
              finalizedHeight = GenesisBlockHeight
            )
              .withConflict(otherNode1Acc, GeneratorIndex(0), endorsedBlock.id())
              .signed(endorsedId = endorsedId, finalizedId = genesisBlockId, otherNode2Acc)
          )
        )
      )

      d.checkFinalizedHeight(3)
    }
  }

  extension (d: Domain)(using Position) {
    def checkFinalizedHeight(h: Int = GenesisBlockHeight.toInt): Unit = {
      d.blockchain.finalizedHeightAt().value shouldBe Height(h)
      d.blockchain.finalizedHeight.value shouldBe Height(h)
    }
  }
}
