package com.wavesplatform.finalization

import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{GeneratorIndex, GenesisBlockHeight, Height}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.NumericExt
import com.wavesplatform.transaction.{CommitToGenerationTransaction, TxHelpers}

class FinalizationSuite extends BaseFinalizationSpec {
  private val node0Acc = TxHelpers.signer(0)
  private val node1Acc = TxHelpers.signer(1)
  private val node2Acc = TxHelpers.signer(2)
  private val node3Acc = TxHelpers.signer(3)

  private val baseSettings = DomainPresets.DeterministicFinality.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  private val defaultSettings = baseSettings
    .copy(minerSettings = baseSettings.minerSettings.copy(quorum = 0))
    .configure(_.copy(generationPeriodLength = 3, generationBalanceDepthFrom50To1000AfterHeight = 1000))

  "finalized if got next block referenced votes in" - {
    "block" in withDomain(defaultSettings, AddrWithBalance.enoughBalances(node0Acc, node1Acc)) { d =>
      val genesisBlockId = d.blockchain.lastBlockId.value
      d.finalizedHeightIsEmpty()
        .finalizedHeightAtPrevIsEmpty()

      d.appendBlock()
      d.allFinalizedHeightIs(1)

      log.debug(s"Append block 3 with commitments")
      val endorsers = Seq(node0Acc, node1Acc)
      val block3 = d.createBlock(
        txs = endorsers.map(x => TxHelpers.commitToGeneration(Height(4), x)),
        generator = node1Acc
      )
      d.appendBlock(block3)
      d.allFinalizedHeightIs(1)

      log.debug(s"Append block 4 with votes")
      val votingBlock = d.createBlock(
        generator = node1Acc,
        strictTime = true,
        finalizationVoting = Some(
          mkFinalizationVoting(valid = Seq(GeneratorIndex(0)))
            .signed(endorsedId = block3.id(), finalizedId = genesisBlockId, validEndorsers = node0Acc)
        )
      )
      d.appender.appendBlock(votingBlock)
      d.allFinalizedHeightIs(1)

      log.debug("Append block 5")
      d.appender.appendBlock(d.createBlock(generator = node1Acc, strictTime = true))
      d.allFinalizedHeightIs(3)
    }

    "microblock" in withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(node0Acc, node1Acc, node2Acc)
    ) { d =>
      val genesisBlockId = d.blockchain.lastBlockId.value
      d.appendBlock()

      log.debug(s"Append block 3 with commitments")
      val endorsers = Seq(node0Acc, node1Acc)
      val block3 = d.createBlock(
        txs = endorsers.map(x => TxHelpers.commitToGeneration(Height(4), x)),
        generator = node1Acc
      )
      d.appendBlock(block3)

      log.debug(s"Append block 4")
      d.appender.appendBlock(
        d.createBlock(generator = node1Acc, strictTime = true)
      )

      log.debug(s"Append microblock with votes")
      val microBlockWithTxn = d.createMicroBlock(
        signer = Some(node1Acc),
        finalizationVoting = Some(
          mkFinalizationVoting(valid = Seq(GeneratorIndex(0)))
            .signed(endorsedId = block3.id(), finalizedId = genesisBlockId, validEndorsers = node0Acc)
        )
      )(TxHelpers.transfer(node2Acc, node3Acc.toAddress))
      d.appendMicroBlock(microBlockWithTxn)
      d.allFinalizedHeightIs(1)

      log.debug("Append block 5")
      d.appender.appendBlock(d.createBlock(generator = node1Acc, strictTime = true))
      d.allFinalizedHeightIs(3)
    }
  }

  "finalized if replaced by better block with votes" in withDomain(
    defaultSettings,
    AddrWithBalance.enoughBalances(node0Acc, node1Acc, node2Acc)
  ) { d =>
    val genesisBlockId = d.blockchain.lastBlockId.value
    d.appendBlock()

    log.debug(s"Append block 3 with commitments")
    val endorsers = Seq(node0Acc, node1Acc, node2Acc)
    val block3 = d.createBlock(
      txs = endorsers.map(x => TxHelpers.commitToGeneration(Height(4), x)),
      generator = node0Acc
    )
    d.appendBlock(block3)

    log.debug(s"Append worse key block 4")
    val betterBlock4 = d.createBlock(
      generator = node1Acc,
      strictTime = true,
      finalizationVoting = Some( // voted: node1Acc, node0Acc; not voted: node2Acc
        mkFinalizationVoting(valid = Seq(GeneratorIndex(0)))
          .signed(endorsedId = block3.id(), finalizedId = genesisBlockId, validEndorsers = node0Acc)
      )
    )
    val worseBlock4 = d.createBlock(generator = node2Acc, strictTime = true, timestamp = Some(d.nextBlockTime(node2Acc) + 100))
    d.appender.appendBlock(worseBlock4)
    d.allFinalizedHeightIs(1)

    log.debug(s"Append better key block 4")
    d.appender.appendBlock(betterBlock4)
    d.allFinalizedHeightIs(1)

    log.debug("Append block 5")
    d.appender.appendBlock(d.createBlock(generator = node1Acc, strictTime = true))
    d.allFinalizedHeightIs(3)
  }

  "not finalized if replaced by better block without votes" in withDomain(
    defaultSettings,
    AddrWithBalance.enoughBalances(node0Acc, node1Acc, node2Acc)
  ) { d =>
    val genesisBlockId = d.blockchain.lastBlockId.value
    d.appendBlock()

    log.debug(s"Append block 3 with commitments")
    val endorsers = Seq(node0Acc, node1Acc, node2Acc)
    val block3 = d.createBlock(
      txs = endorsers.map(x => TxHelpers.commitToGeneration(Height(4), x)),
      generator = node1Acc
    )
    d.appendBlock(block3)

    log.debug(s"Append worse key (from worse fork) block 4")
    val betterBlock4 = d.createBlock(generator = node2Acc, strictTime = true)
    val worseBlock4 = d.createBlock(
      generator = node1Acc,
      strictTime = true,
      timestamp = Some(d.nextBlockTime(node1Acc) + 100),
      finalizationVoting = Some( // voted: node1Acc, node0Acc; not voted: node2Acc
        mkFinalizationVoting(valid = Seq(GeneratorIndex(0)))
          .signed(endorsedId = block3.id(), finalizedId = genesisBlockId, validEndorsers = node0Acc)
      )
    )
    d.appender.appendBlock(worseBlock4)
    d.allFinalizedHeightIs(1)

    log.debug(s"Append better key block 4")
    d.appender.appendBlock(betterBlock4)
    d.allFinalizedHeightIs(1)

    log.debug("Append block 5")
    d.appender.appendBlock(d.createBlock(generator = node1Acc, strictTime = true))
    d.allFinalizedHeightIs(1)
  }

  "not finalized if not voted" in withDomain(
    defaultSettings,
    AddrWithBalance.enoughBalances(node0Acc, node1Acc)
  ) { d =>
    d.appendBlock()

    log.debug(s"Append block 3 with commitments")
    val endorsers = Seq(node0Acc, node1Acc)
    val block3 = d.createBlock(
      txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(4), x)),
      generator = node1Acc
    )
    d.appendBlock(block3)

    log.debug(s"Append block 4 without votes (only miner)")
    d.appender.appendBlock(
      d.createBlock(generator = node1Acc, strictTime = true, finalizationVoting = None)
    )

    log.debug("Append block 5")
    d.appender.appendBlock(d.createBlock(generator = node1Acc, strictTime = true))
    d.allFinalizedHeightIs(1)
  }

  "finalized if surpass maxRollback blocks even no votes" in withDomain(
    defaultSettings.copy(synchronizationSettings = defaultSettings.synchronizationSettings.copy(maxRollback = 2)),
    AddrWithBalance.enoughBalances(node0Acc, node1Acc)
  ) { d =>
    d.appendBlock()

    log.debug(s"Append block 3 with commitments")
    val endorsers = Seq(node1Acc, node0Acc)
    val block3 = d.createBlock(
      txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(4), x)),
      generator = node1Acc
    )
    d.appendBlock(block3)

    log.debug(s"Append block 4 without votes (only miner committed)")
    d.appender.appendBlock(
      d.createBlock(generator = node1Acc, strictTime = true, finalizationVoting = None)
    )

    log.debug("Append block 5")
    d.appender.appendBlock(d.createBlock(generator = node1Acc, strictTime = true))
    d.allFinalizedHeightIs(2) // 4 - maxRollback = 2, 4 because we calculate finalization based on votes in a previous block
  }

  "empty generator set" - {
    def test(settings: WavesSettings)(continue: Domain => Unit): Unit = withDomain(
      settings,
      AddrWithBalance.enoughBalances(node0Acc, node1Acc, node2Acc)
    ) { d =>
      d.appendBlock()

      log.debug(s"Append block 3 with commitments")
      val endorsers = Seq(node0Acc, node1Acc)
      val block3 = d.createBlock(
        txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(4), x)),
        generator = node1Acc
      )
      d.appendBlock(block3)

      log.debug(s"Append block 4 without votes (only miner committed)")
      d.appender.appendBlock(
        d.createBlock(
          txs = Seq(
            TxHelpers.transfer(
              node1Acc,
              node2Acc.toAddress,
              amount = d.blockchain.balance(node1Acc.toAddress) - CommitToGenerationTransaction.DepositInWavelets - 1.waves,
              fee = 1.waves
            )
          ),
          generator = node1Acc,
          strictTime = true,
          finalizationVoting = Some(
            mkFinalizationVoting().withConflict(node0Acc, GeneratorIndex(0), d.blockchain.lastBlockId.value)
          )
        )
      )

      log.debug("Append block 5")
      d.appender.appendBlock(d.createBlock(generator = node2Acc, strictTime = true))
      d.allFinalizedHeightIs(3) // Generating balance of node1Acc is enough

      log.debug("Append block 6")
      d.appender.appendBlock(d.createBlock(generator = node2Acc, strictTime = true))

      log.debug("Append block 7")
      d.appender.appendBlock(d.createBlock(generator = node2Acc, strictTime = true))
      continue(d)
    }

    "same finalized height if mines a generator not from generator set" in test(defaultSettings)(_.allFinalizedHeightIs(3)) // Same as after block 5

    "finalized if surpass maxRollback blocks" in test(
      defaultSettings.copy(synchronizationSettings = defaultSettings.synchronizationSettings.copy(maxRollback = 2))
    ) { d => d.allFinalizedHeightIs(4) } // 6 - maxRollback = 4
  }

  "finalized with less votes after conflict endorsement" in withDomain(
    defaultSettings,
    Seq(node0Acc, node1Acc, node2Acc, node3Acc).map(kp => AddrWithBalance(kp.toAddress, 2000.waves))
  ) { d =>
    val genesisBlockId = d.blockchain.lastBlockId.value
    d.appendBlock()

    log.debug(s"Append block 3 with commitments")
    val endorsers = Seq(node0Acc, node1Acc, node2Acc, node3Acc)
    val block3 = d.createBlock(
      txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(4), x)),
      generator = node3Acc
    )
    d.appendBlock(block3)
    val endorsedBlock = block3
    val endorsedId    = endorsedBlock.id()

    log.debug(s"Append block 4 with conflict vote")
    d.appender.appendBlock(
      d.createBlock(
        generator = node3Acc,
        strictTime = true,
        finalizationVoting = Some(
          mkFinalizationVoting(
            valid = Seq(GeneratorIndex(2)),
            finalizedHeight = GenesisBlockHeight
          )
            .withConflict(node0Acc, GeneratorIndex(0), endorsedBlock.id())
            .signed(endorsedId = endorsedId, finalizedId = genesisBlockId, node2Acc)
        )
      )
    )

    d.allFinalizedHeightIs(1)

    log.debug("Append block 5")
    d.appender.appendBlock(d.createBlock(generator = node3Acc, strictTime = true))
    d.allFinalizedHeightIs(3)
  }

  "spending after voting doesn't affect finalization" in withDomain(
    defaultSettings,
    AddrWithBalance.enoughBalances(node0Acc, node1Acc, node2Acc)
  ) { d =>
    val genesisBlockId = d.blockchain.lastBlockId.value
    d.appendBlock()

    log.debug(s"Append block 3 with commitments")
    val endorsers = Seq(node0Acc, node1Acc, node2Acc)
    val block3 = d.createBlock(
      txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(4), x)),
      generator = node1Acc
    )
    d.appendBlock(block3)

    log.debug(s"Append block 4 with votes and spending")
    d.appender.appendBlock(
      d.createBlock(
        txs = Seq(
          TxHelpers.transfer(
            node0Acc,                                                                            // Endorser
            to = node3Acc.toAddress,                                                             // Not endorser
            amount = d.blockchain.wavesPortfolio(node0Acc.toAddress).spendableBalance - 1.waves, // All waves
            fee = 1.waves
          )
        ),
        generator = node1Acc,
        strictTime = true,
        finalizationVoting = Some(
          mkFinalizationVoting(valid = Seq(GeneratorIndex(0)))
            .signed(endorsedId = block3.id(), finalizedId = genesisBlockId, validEndorsers = node0Acc)
        )
      )
    )

    log.debug("Append block 5")
    d.appender.appendBlock(d.createBlock(generator = node1Acc, strictTime = true))
    d.allFinalizedHeightIs(3)
  }

  "increasing generating balance" - {
    "reaching finalization" - {
      "miner" in withDomain(
        defaultSettings,
        AddrWithBalance(node3Acc.toAddress, 6000.waves) +:
          Seq(node0Acc, node1Acc, node2Acc).map(kp => AddrWithBalance(kp.toAddress, 2000.waves))
      ) { d =>
        // This is block #2
        // Generating balance of node1Acc increased on 2 + 50 (generationBalanceDepthFrom50To1000AfterHeight) = 52
        d.appendBlock(
          TxHelpers.transfer(
            node3Acc,                                                                            // Not endorser
            to = node1Acc.toAddress,                                                             // Miner
            amount = d.blockchain.wavesPortfolio(node3Acc.toAddress).spendableBalance - 1.waves, // Enough for finalization
            fee = 1.waves
          )
        )

        log.debug("Append empty blocks to reach the required period")
        (3 to 50).foreach(_ => d.appendBlock())

        log.debug("Append block with commitments")
        val endorsers = Seq(node0Acc, node1Acc, node2Acc)
        d.appendBlock(
          d.createBlock(
            txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(52), x)),
            generator = node1Acc
          )
        ) // 51

        log.debug(s"Append block without votes, but increased miner's generating balance")
        d.appender.appendBlock(d.createBlock(generator = node1Acc, strictTime = true)) // 52

        log.debug("Append block to calculate finalization height")
        d.appender.appendBlock(d.createBlock(generator = node1Acc, strictTime = true)) // 53
        d.allFinalizedHeightIs(51)
      }

      "voter" in {
        withDomain(
          defaultSettings,
          AddrWithBalance(node3Acc.toAddress, 6000.waves) +:
            AddrWithBalance(node2Acc.toAddress, 4000.waves) +:
            Seq(node0Acc, node1Acc).map(kp => AddrWithBalance(kp.toAddress, 2000.waves))
        ) { d =>
          val genesisBlockId = d.blockchain.lastBlockId.value

          // This is block #2
          // Generating balance of node1Acc increased on 2 + 50 (generationBalanceDepthFrom50To1000AfterHeight) = 52
          d.appendBlock(
            TxHelpers.transfer(
              node3Acc,                                                                            // Not endorser
              to = node0Acc.toAddress,                                                             // Endorser
              amount = d.blockchain.wavesPortfolio(node3Acc.toAddress).spendableBalance - 1.waves, // Enough for finalization
              fee = 1.waves
            )
          )

          log.debug("Append empty blocks to reach the required period")
          (3 to 50).foreach(_ => d.appendBlock())

          log.debug("Append block with commitments")
          val endorsers = Seq(node0Acc, node1Acc, node2Acc)
          d.appendBlock(
            d.createBlock(
              txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(52), x)),
              generator = node1Acc
            )
          ) // 51

          log.debug(s"Append block with vote")
          d.appender.appendBlock(
            d.createBlock(
              generator = node1Acc,
              strictTime = true,
              finalizationVoting = Some(
                mkFinalizationVoting(valid = Seq(GeneratorIndex(0)))
                  .signed(endorsedId = d.blockchain.lastBlockId.value, finalizedId = genesisBlockId, validEndorsers = node0Acc)
              )
            )
          ) // 52

          log.debug("Append block to calculate finalization height")
          d.appender.appendBlock(d.createBlock(generator = node1Acc, strictTime = true)) // 53
          d.allFinalizedHeightIs(51)
        }
      }
    }

    "not reaching finalization" in withDomain(
      defaultSettings,
      AddrWithBalance(node3Acc.toAddress, 6000.waves) +:
        Seq(node0Acc, node1Acc, node2Acc).map(kp => AddrWithBalance(kp.toAddress, 2000.waves))
    ) { d =>
      val genesisBlockId = d.blockchain.lastBlockId.value

      // This is block #2
      // Generating balance of node1Acc increased on 2 + 50 (generationBalanceDepthFrom50To1000AfterHeight) = 52
      d.appendBlock(
        TxHelpers.transfer(
          node3Acc,                // Not endorser
          to = node2Acc.toAddress, // Not voting
          amount = d.blockchain.wavesPortfolio(node3Acc.toAddress).spendableBalance - 1.waves,
          fee = 1.waves
        )
      )

      log.debug("Append empty blocks to reach the required period")
      (3 to 50).foreach(_ => d.appendBlock())

      log.debug("Append block with commitments")
      val endorsers = Seq(node0Acc, node1Acc, node2Acc)
      d.appendBlock(
        d.createBlock(
          txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(52), x)),
          generator = node1Acc
        )
      ) // 51

      log.debug(s"Append block with vote, balance of non-voting endorser increased")
      d.appender.appendBlock(
        d.createBlock(
          generator = node1Acc,
          strictTime = true,
          finalizationVoting = Some(
            mkFinalizationVoting(valid = Seq(GeneratorIndex(0)))
              .signed(endorsedId = d.blockchain.lastBlockId.value, finalizedId = genesisBlockId, validEndorsers = node0Acc)
          )
        )
      ) // 52

      log.debug("Append block to calculate finalization height")
      d.appender.appendBlock(d.createBlock(generator = node1Acc, strictTime = true)) // 53
      d.allFinalizedHeightIs(1)
    }
  }
}
