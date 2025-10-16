package com.wavesplatform.finalization

import com.wavesplatform.block.{Block, BlockEndorsement, FinalizationVoting}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.{GenesisBlockHeight, Height}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{FreeSpec, NumericExt}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.wallet.Wallet

// TODO: Merge with BlockAppenderSpec tests?
class FinalizationSuite extends FreeSpec with WithDomain {
  private val seed          = ByteStr("finality-test".getBytes())
  private val thisNodeAcc   = Wallet.generateNewAccount(seed.arr, nonce = 0)
  private val otherNode1Acc = TxHelpers.defaultSigner
  private val otherNode2Acc = TxHelpers.secondSigner

  private val baseSettings = DomainPresets.DeterministicFinality.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  private val defaultSettings = baseSettings
    .copy(
      minerSettings = baseSettings.minerSettings.copy(quorum = 0),
      walletSettings = baseSettings.walletSettings.copy(seed = Some(seed))
    )
    .configure(_.copy(generationPeriodLength = 3))

  "finalized height doesn't decrease" - {
    "increased if voted" in withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(otherNode1Acc, thisNodeAcc)
    ) { d =>
      val genesisBlockId = d.blockchain.lastBlockId.value
      d.blockchain.finalizedHeightAt(Height(d.blockchain.height)).value shouldBe GenesisBlockHeight

      log.debug(s"Append block 2 with commitments")
      val endorsers = Seq(otherNode1Acc, thisNodeAcc)
      val block2 = d.createBlock(
        version = Block.ProtoBlockVersion,
        txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = 3, x)),
        generator = otherNode1Acc
      )
      d.appendBlock(block2)
      d.blockchain.finalizedHeightAt(Height(d.blockchain.height)).value shouldBe GenesisBlockHeight

      log.debug(s"Append block 3 with votes")
      val aggSig = BlockEndorsement.sign(
        BlsKeyPair(thisNodeAcc.privateKey),
        finalizedId = genesisBlockId,
        finalizedHeight = GenesisBlockHeight,
        endorsedId = block2.id()
      )
      val votingBlock = d.createBlock(
        version = Block.ProtoBlockVersion,
        txs = Nil,
        generator = otherNode1Acc,
        strictTime = true,
        voting = Some(
          FinalizationVoting(
            endorserIndexes = Seq(1),
            aggregatedEndorsement = aggSig,
            conflict = Nil
          )
        )
      )
      d.appender.appendBlock(votingBlock)
      d.blockchain.finalizedHeightAt(Height(d.blockchain.height)).value shouldBe GenesisBlockHeight

      log.debug("Append block 4")
      d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = otherNode1Acc, strictTime = true))
      d.blockchain.finalizedHeightAt(Height(d.blockchain.height)).value shouldBe Height(2)
    }

    "spending balance after voting doesn't affect finalization" in withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(otherNode1Acc, otherNode2Acc, thisNodeAcc)
    ) { d =>
      val genesisBlockId = d.blockchain.lastBlockId.value

      log.debug(s"Append block 2 with commitments")
      val endorsers = Seq(otherNode1Acc, otherNode2Acc, thisNodeAcc)
      val block2 = d.createBlock(
        version = Block.ProtoBlockVersion,
        txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = 3, x)),
        generator = otherNode1Acc
      )
      d.appendBlock(block2)

      log.debug(s"Append block 3 with votes and spending")
      val aggSig = BlockEndorsement.sign(
        BlsKeyPair(thisNodeAcc.privateKey),
        finalizedId = genesisBlockId,
        finalizedHeight = GenesisBlockHeight,
        endorsedId = block2.id()
      )
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
          voting = Some(
            FinalizationVoting(
              endorserIndexes = Seq(2),
              aggregatedEndorsement = aggSig,
              conflict = Nil
            )
          )
        )
      )

      log.debug("Append block 4")
      d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = otherNode1Acc, strictTime = true))
      d.blockchain.finalizedHeightAt(Height(d.blockchain.height)).value shouldBe Height(2)
    }

    "same finalized height if not voted" in withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(otherNode1Acc, thisNodeAcc)
    ) { d =>
      log.debug(s"Append block 2 with commitments")
      val endorsers = Seq(otherNode1Acc, thisNodeAcc)
      val block2 = d.createBlock(
        version = Block.ProtoBlockVersion,
        txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = 3, x)),
        generator = otherNode1Acc
      )
      d.appendBlock(block2)

      log.debug(s"Append block 3 without votes (only miner committed)")
      d.appender.appendBlock(
        d.createBlock(
          version = Block.ProtoBlockVersion,
          txs = Nil,
          generator = otherNode1Acc,
          strictTime = true,
          voting = None
        )
      )

      log.debug("Append block 4")
      d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = otherNode1Acc, strictTime = true))
      d.blockchain.finalizedHeightAt(Height(d.blockchain.height)).value shouldBe GenesisBlockHeight
    }

    "increased if surpass maxRollback blocks even no votes" in withDomain(
      defaultSettings.copy(synchronizationSettings = defaultSettings.synchronizationSettings.copy(maxRollback = 2)),
      AddrWithBalance.enoughBalances(otherNode1Acc, thisNodeAcc)
    ) { d =>
      log.debug(s"Append block 2 with commitments")
      val endorsers = Seq(otherNode1Acc, thisNodeAcc)
      val block2 = d.createBlock(
        version = Block.ProtoBlockVersion,
        txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = 3, x)),
        generator = otherNode1Acc
      )
      d.appendBlock(block2)

      log.debug(s"Append block 3 without votes (only miner committed)")
      d.appender.appendBlock(
        d.createBlock(
          version = Block.ProtoBlockVersion,
          txs = Nil,
          generator = otherNode1Acc,
          strictTime = true,
          voting = None
        )
      )

      log.debug("Append block 4")
      d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = otherNode1Acc, strictTime = true))
      d.blockchain.finalizedHeightAt(Height(d.blockchain.height)).value shouldBe Height(2) // 4 - maxRollback
    }
  }
}
