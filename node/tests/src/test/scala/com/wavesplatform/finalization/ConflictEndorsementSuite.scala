package com.wavesplatform.finalization

import com.wavesplatform.block.{Block, BlockEndorsement, FinalizationVoting}
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{Blockchain, GenesisBlockHeight, Portfolio}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.{CommitToGenerationTransaction, TxHelpers}

class ConflictEndorsementSuite extends FreeSpec with WithDomain {
  private val generator1 = TxHelpers.signer(0)
  private val generator2 = TxHelpers.signer(1)

  private val baseSettings    = DomainPresets.DeterministicFinality.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  private val defaultSettings = baseSettings.configure(_.copy(generationPeriodLength = 2))

  "conflict endorser lost deposit and removed from generators set" in withDomain(
    defaultSettings,
    AddrWithBalance.enoughBalances(generator1, generator2)
  ) { d =>
    val generator2Addr        = generator2.toAddress
    val generator2InitBalance = ENOUGH_AMT

    log.debug(s"Append block 2 with commitments")
    val endorsers = Seq(generator1, generator2)
    val txs       = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = 3, x))
    val block2    = d.createBlock(version = Block.ProtoBlockVersion, txs = txs, generator = generator1)
    d.appendBlock(block2)

    val generator2BalanceAfterBlock2 = generator2InitBalance - txs(1).fee.value
    d.blockchain.wavesPortfolio(generator2Addr) shouldBe Portfolio(
      balance = generator2BalanceAfterBlock2,
      generationDeposit = CommitToGenerationTransaction.DepositInWavelets
    )

    log.debug(s"Append block 3 with votes")
    val otherFinalizedBlockId = TxHelpers.randomBlockId
    val votingBlock = d.createBlock(
      version = Block.ProtoBlockVersion,
      txs = Nil,
      generator = generator1,
      strictTime = true,
      voting = Some(
        FinalizationVoting(
          conflict = Vector(
            BlockEndorsement.Conflict(
              endorserIndex = 0,
              finalizedId = otherFinalizedBlockId,
              signature = BlockEndorsement.sign(
                kp = BlsKeyPair(generator1.privateKey),
                finalizedId = otherFinalizedBlockId,
                finalizedHeight = GenesisBlockHeight,
                endorsedId = block2.id()
              )
            )
          )
        )
      )
    )
    d.appender.appendBlock(votingBlock)

    val generator2BalanceAfterBlock3 = generator2BalanceAfterBlock2 - CommitToGenerationTransaction.DepositInWavelets
    d.blockchain.wavesPortfolio(generator2Addr) shouldBe Portfolio(balance = generator2BalanceAfterBlock3)
    d.blockchain.committedGenerators(d.blockchain.currentGenerationPeriod.value).map(_._1) shouldNot contain(generator2Addr)

    log.debug("Append block 4")
    d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = generator2, strictTime = true))
    d.blockchain.wavesPortfolio(generator2Addr) shouldBe Portfolio(balance = generator2BalanceAfterBlock3)
    d.blockchain.committedGenerators(d.blockchain.currentGenerationPeriod.value).map(_._1) shouldNot contain(generator2Addr)
  }
}
