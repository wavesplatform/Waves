package com.wavesplatform.finalization

import com.wavesplatform.block.{Block, FinalizationVoting}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.state.{Blockchain, GeneratorIndex, GenesisBlockHeight, Height}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{FreeSpec, NumericExt}
import com.wavesplatform.transaction.TxHelpers
import org.scalactic.source.Position

class FinalizationSuite extends BaseFinalizationSpec {
  private val otherNode0Acc = TxHelpers.signer(0)
  private val otherNode1Acc = TxHelpers.signer(1)
  private val otherNode2Acc = TxHelpers.signer(2)
  private val otherNode3Acc = TxHelpers.signer(3)

  private val baseSettings = DomainPresets.DeterministicFinality.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  private val defaultSettings = baseSettings
    .copy(minerSettings = baseSettings.minerSettings.copy(quorum = 0))
    .configure(_.copy(generationPeriodLength = 3, generationBalanceDepthFrom50To1000AfterHeight = 1000))

  "finalized if got next block referenced votes in" - {
    "block" in withDomain(defaultSettings, AddrWithBalance.enoughBalances(otherNode0Acc, otherNode1Acc)) { d =>
      val genesisBlockId = d.blockchain.lastBlockId.value
      d.finalizedHeightIsEmpty()
        .finalizedHeightAtPrevIsEmpty()

      d.appendBlock()
      d.allFinalizedHeightIs(1)

      log.debug(s"Append block 3 with commitments")
      val endorsers = Seq(otherNode0Acc, otherNode1Acc)
      val block3 = d.createBlock(
        version = Block.ProtoBlockVersion,
        txs = endorsers.map(x => TxHelpers.commitToGeneration(Height(4), x)),
        generator = otherNode1Acc
      )
      d.appendBlock(block3)
      d.allFinalizedHeightIs(1)

      log.debug(s"Append block 4 with votes")
      val votingBlock = d.createBlock(
        version = Block.ProtoBlockVersion,
        txs = Nil,
        generator = otherNode1Acc,
        strictTime = true,
        finalizationVoting = Some(
          mkFinalizationVoting(valid = Seq(GeneratorIndex(1)))
            .signed(endorsedId = block3.id(), finalizedId = genesisBlockId, validEndorsers = otherNode0Acc)
        )
      )
      d.appender.appendBlock(votingBlock)
      d.allFinalizedHeightIs(1)

      log.debug("Append block 5")
      d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = otherNode1Acc, strictTime = true))
      d.allFinalizedHeightIs(3)
    }

    "microblock" in withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(otherNode0Acc, otherNode1Acc, otherNode2Acc)
    ) { d =>
      val genesisBlockId = d.blockchain.lastBlockId.value
      d.appendBlock()

      log.debug(s"Append block 3 with commitments")
      val endorsers = Seq(otherNode0Acc, otherNode1Acc)
      val block3 = d.createBlock(
        version = Block.ProtoBlockVersion,
        txs = endorsers.map(x => TxHelpers.commitToGeneration(Height(4), x)),
        generator = otherNode1Acc
      )
      d.appendBlock(block3)

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
            .signed(endorsedId = block3.id(), finalizedId = genesisBlockId, validEndorsers = otherNode0Acc)
        )
      )(TxHelpers.transfer(otherNode2Acc, otherNode3Acc.toAddress))
      d.appendMicroBlock(microBlockWithTxn)
      d.allFinalizedHeightIs(1) // Increased only on keyblock

      log.debug("Append block 5")
      d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = otherNode1Acc, strictTime = true))
      d.allFinalizedHeightIs(3)
    }
  }

  "finalized if replaced by better block with votes" in withDomain(
    defaultSettings,
    AddrWithBalance.enoughBalances(otherNode0Acc, otherNode1Acc, otherNode2Acc)
  ) { d =>
    val genesisBlockId = d.blockchain.lastBlockId.value
    d.appendBlock()

    log.debug(s"Append block 3 with commitments")
    val endorsers = Seq(otherNode0Acc, otherNode1Acc, otherNode2Acc)
    val block3 = d.createBlock(
      version = Block.ProtoBlockVersion,
      txs = endorsers.map(x => TxHelpers.commitToGeneration(Height(4), x)),
      generator = otherNode0Acc
    )
    d.appendBlock(block3)

    log.debug(s"Append worse key block 4")
    val betterBlock4 = d.createBlock(
      version = Block.ProtoBlockVersion,
      txs = Nil,
      generator = otherNode1Acc,
      strictTime = true,
      finalizationVoting = Some( // voted: otherNode1Acc, otherNode0Acc; not voted: otherNode2Acc
        mkFinalizationVoting(valid = Seq(GeneratorIndex(2)))
          .signed(endorsedId = block3.id(), finalizedId = genesisBlockId, validEndorsers = otherNode0Acc)
      )
    )
    val worseBlock4 = d.createBlock(
      version = Block.ProtoBlockVersion,
      txs = Nil,
      generator = otherNode2Acc,
      strictTime = true,
      timestamp = Some(d.nextBlockTime(otherNode2Acc) + 100)
    )
    d.appender.appendBlock(worseBlock4)
    d.allFinalizedHeightIs(1)

    log.debug(s"Append better key block 4")
    d.appender.appendBlock(betterBlock4)
    d.allFinalizedHeightIs(1)

    log.debug("Append block 5")
    d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = otherNode1Acc, strictTime = true))
    d.allFinalizedHeightIs(3)
  }

  "not finalized if replaced by better block without votes" in withDomain(
    defaultSettings,
    AddrWithBalance.enoughBalances(otherNode0Acc, otherNode1Acc, otherNode2Acc)
  ) { d =>
    val genesisBlockId = d.blockchain.lastBlockId.value
    d.appendBlock()

    log.debug(s"Append block 3 with commitments")
    val endorsers = Seq(otherNode0Acc, otherNode1Acc, otherNode2Acc)
    val block3 = d.createBlock(
      version = Block.ProtoBlockVersion,
      txs = endorsers.map(x => TxHelpers.commitToGeneration(Height(4), x)),
      generator = otherNode1Acc
    )
    d.appendBlock(block3)

    log.debug(s"Append worse key (from worse fork) block 4")
    val betterBlock4 = d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = otherNode2Acc, strictTime = true)
    val worseBlock4 = d.createBlock(
      version = Block.ProtoBlockVersion,
      txs = Nil,
      generator = otherNode1Acc,
      strictTime = true,
      timestamp = Some(d.nextBlockTime(otherNode1Acc) + 100),
      finalizationVoting = Some( // voted: otherNode1Acc, otherNode0Acc; not voted: otherNode2Acc
        mkFinalizationVoting(valid = Seq(GeneratorIndex(2)))
          .signed(endorsedId = block3.id(), finalizedId = genesisBlockId, validEndorsers = otherNode0Acc)
      )
    )
    d.appender.appendBlock(worseBlock4)
    d.allFinalizedHeightIs(1)

    log.debug(s"Append better key block 4")
    d.appender.appendBlock(betterBlock4)
    d.allFinalizedHeightIs(1)
  }

  "not finalized if not voted" in withDomain(
    defaultSettings,
    AddrWithBalance.enoughBalances(otherNode0Acc, otherNode1Acc)
  ) { d =>
    d.appendBlock()

    log.debug(s"Append block 3 with commitments")
    val endorsers = Seq(otherNode0Acc, otherNode1Acc)
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
    d.allFinalizedHeightIs(1)
  }

  "same finalized height if mines a generator not from generator set" ignore {} // TODO: implement

  "finalized if surpass maxRollback blocks even" - {
    "no votes" in withDomain(
      defaultSettings.copy(synchronizationSettings = defaultSettings.synchronizationSettings.copy(maxRollback = 2)),
      AddrWithBalance.enoughBalances(otherNode0Acc, otherNode1Acc)
    ) { d =>
      d.appendBlock()

      log.debug(s"Append block 3 with commitments")
      val endorsers = Seq(otherNode1Acc, otherNode0Acc)
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
      d.allFinalizedHeightIs(2) // 4 - maxRollback = 2, 4 because we calculate finalization based on votes in a previous block
    }

    "generator set is empty" ignore {}
  }

  "finalized with less votes after conflict endorsement" in withDomain(
    defaultSettings,
    Seq(otherNode0Acc, otherNode1Acc, otherNode2Acc, otherNode3Acc).map(kp => AddrWithBalance(kp.toAddress, 200_100.1.waves))
  ) { d =>
    val genesisBlockId = d.blockchain.lastBlockId.value
    d.appendBlock()

    log.debug(s"Append block 3 with commitments")
    val endorsers = Seq(otherNode0Acc, otherNode1Acc, otherNode2Acc, otherNode3Acc)
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

    d.allFinalizedHeightIs(1)

    log.debug("Append block 5")
    d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = otherNode3Acc, strictTime = true))
    d.allFinalizedHeightIs(3)
  }

  "spending after voting doesn't affect finalization" in withDomain(
    defaultSettings,
    AddrWithBalance.enoughBalances(otherNode0Acc, otherNode1Acc, otherNode2Acc)
  ) { d =>
    val genesisBlockId = d.blockchain.lastBlockId.value
    d.appendBlock()

    log.debug(s"Append block 3 with commitments")
    val endorsers = Seq(otherNode0Acc, otherNode1Acc, otherNode2Acc)
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
            otherNode0Acc,
            to = otherNode3Acc.toAddress,                                                             // Not endorser
            amount = d.blockchain.wavesPortfolio(otherNode0Acc.toAddress).spendableBalance - 1.waves, // All waves
            fee = 1.waves
          )
        ),
        generator = otherNode1Acc,
        strictTime = true,
        finalizationVoting = Some(
          mkFinalizationVoting(valid = Seq(GeneratorIndex(2)))
            .signed(endorsedId = block3.id(), finalizedId = genesisBlockId, validEndorsers = otherNode0Acc)
        )
      )
    )

    log.debug("Append block 5")
    d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = otherNode1Acc, strictTime = true))
    d.allFinalizedHeightIs(3)
  }

  "increasing generating balance" - {
    "leads to finalization" - {
      "miner" in withDomain(
        defaultSettings,
        AddrWithBalance(otherNode3Acc.toAddress, 6000.waves) +:
          Seq(otherNode0Acc, otherNode1Acc, otherNode2Acc).map(kp => AddrWithBalance(kp.toAddress, 2000.waves))
      ) { d =>
        // This is block #2
        // Generating balance of otherNode1Acc increased on 2 + 50 (generationBalanceDepthFrom50To1000AfterHeight) = 52
        d.appendBlock(
          TxHelpers.transfer(
            otherNode3Acc,                                                                            // Not endorser
            to = otherNode1Acc.toAddress,                                                             // Endorser
            amount = d.blockchain.wavesPortfolio(otherNode3Acc.toAddress).spendableBalance - 1.waves, // Enough for finalization
            fee = 1.waves
          )
        )

        log.debug("Append empty blocks to reach the required period")
        (3 to 50).foreach(_ => d.appendBlock())

        log.debug("Append block with commitments")
        val endorsers = Seq(otherNode0Acc, otherNode1Acc, otherNode2Acc)
        d.appendBlock(
          d.createBlock(
            version = Block.ProtoBlockVersion,
            txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(52), x)),
            generator = otherNode1Acc
          )
        ) // 51

        log.debug(s"Append block without votes, but increased miner's generating balance")
        d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = otherNode1Acc, strictTime = true)) // 52

        log.debug("Append block to calculate finalization height")
        d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = otherNode1Acc, strictTime = true)) // 53
        d.allFinalizedHeightIs(51)
      }

      "voter" in {
        withDomain(
          defaultSettings,
          AddrWithBalance(otherNode3Acc.toAddress, 6000.waves) +:
            AddrWithBalance(otherNode2Acc.toAddress, 4000.waves) +:
            Seq(otherNode0Acc, otherNode1Acc).map(kp => AddrWithBalance(kp.toAddress, 2000.waves))
        ) { d =>
          val genesisBlockId = d.blockchain.lastBlockId.value

          // This is block #2
          // Generating balance of otherNode1Acc increased on 2 + 50 (generationBalanceDepthFrom50To1000AfterHeight) = 52
          d.appendBlock(
            TxHelpers.transfer(
              otherNode3Acc,                                                                            // Not endorser
              to = otherNode0Acc.toAddress,                                                             // Endorser
              amount = d.blockchain.wavesPortfolio(otherNode3Acc.toAddress).spendableBalance - 1.waves, // Enough for finalization
              fee = 1.waves
            )
          )

          log.debug("Append empty blocks to reach the required period")
          (3 to 50).foreach(_ => d.appendBlock())

          log.debug("Append block with commitments")
          val endorsers = Seq(otherNode0Acc, otherNode1Acc, otherNode2Acc)
          d.appendBlock(
            d.createBlock(
              version = Block.ProtoBlockVersion,
              txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(52), x)),
              generator = otherNode1Acc
            )
          ) // 51

          log.debug(s"Append block with vote")
          d.appender.appendBlock(
            d.createBlock(
              version = Block.ProtoBlockVersion,
              txs = Nil,
              generator = otherNode1Acc,
              strictTime = true,
              finalizationVoting = Some(
                mkFinalizationVoting(valid = Seq(GeneratorIndex(0)))
                  .signed(endorsedId = d.blockchain.lastBlockId.value, finalizedId = genesisBlockId, validEndorsers = otherNode0Acc)
              )
            )
          ) // 52

          log.debug(s"Append block without votes, but increased endorser's generating balance")
          d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = otherNode1Acc, strictTime = true)) // 53
          d.allFinalizedHeightIs(51)
        }
      }
    }
  }

  extension (d: Domain)(using Position) {
    def finalizedHeightIsEmpty(): Domain = withClue("finalizedHeightIsEmpty: ") {
      d.blockchain.finalizedHeight shouldBe empty
      d
    }

    def finalizedHeightIs(h: Int): Domain = withClue("finalizedHeightIs: ") {
      d.blockchain.finalizedHeight.value.toInt shouldBe h
      d
    }

    def finalizedHeightAtPrevIsEmpty(): Domain = withClue("finalizedHeightAtIsEmpty: ") {
      val prevHeight = Height(d.blockchain.height - 1)
      if (prevHeight >= GenesisBlockHeight) d.blockchain.finalizedHeightAt(prevHeight) shouldBe empty
      d
    }

    def finalizedHeightAtPrevIs(h: Int): Domain = withClue("finalizedHeightAtIs: ") {
      val prevHeight = Height(d.blockchain.height - 1)
      d.blockchain.finalizedHeightAt(prevHeight).value.toInt shouldBe h
      d
    }

    def allFinalizedHeightIs(h: Int): Domain = d
      .finalizedHeightIs(h)
      .finalizedHeightAtPrevIs(h)
  }
}
