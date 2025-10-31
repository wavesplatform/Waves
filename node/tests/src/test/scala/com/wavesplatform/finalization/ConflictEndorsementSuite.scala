package com.wavesplatform.finalization

import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, BlockEndorsement, FinalizationVoting}
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{Blockchain, ConflictGenerators, GeneratorIndex, GenesisBlockHeight, Height, Portfolio}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.CommitToGenerationTransaction.DepositInWavelets
import com.wavesplatform.transaction.TxHelpers
import org.scalactic.source.Position

class ConflictEndorsementSuite extends FreeSpec with WithDomain {
  private val generator1 = TxHelpers.signer(0)
  private val generator2 = TxHelpers.signer(1)

  private val baseSettings = DomainPresets.DeterministicFinality.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  private val defaultSettings = baseSettings.configure(
    _.copy(
      generationPeriodLength = 2,
      lightNodeBlockFieldsAbsenceInterval = 0
    )
  )

  "conflict endorser lost deposit and removed from generators set" in withDomain(
    defaultSettings,
    AddrWithBalance.enoughBalances(generator1, generator2)
  ) { d =>
    val generator2Addr        = generator2.toAddress
    val generator2InitBalance = ENOUGH_AMT

    log.debug(s"Append block 2 with commitments")
    val endorsers     = Seq(generator1, generator2)
    val endorserAddrs = endorsers.map(_.toAddress)
    val txs           = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = 3, x))
    val block2        = d.createBlock(version = Block.ProtoBlockVersion, txs = txs, generator = generator1, strictTime = true)
    d.appender.appendBlock(block2)

    val generator2BalanceAfterBlock2 = generator2InitBalance - txs(1).fee.value
    d.blockchain.wavesPortfolio(generator2Addr) shouldBe Portfolio(
      balance = generator2BalanceAfterBlock2,
      generationDeposit = DepositInWavelets
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
              endorserIndex = GeneratorIndex(1),
              finalizedId = otherFinalizedBlockId,
              signature = BlockEndorsement.sign(
                kp = BlsKeyPair(generator2.privateKey),
                finalizedId = otherFinalizedBlockId,
                finalizedHeight = GenesisBlockHeight,
                endorsedId = block2.id()
              )
            )
          )
        )
      )
    )

    val wavesAmountBeforeVoting = d.blockchain.wavesAmount(d.blockchain.height)
    d.appender.appendBlock(votingBlock)

    val generator2BalanceAfterBlock3 = generator2BalanceAfterBlock2
    d.blockchain.checkCommitted(endorserAddrs*)
    d.blockchain.checkHasConflict(h = 3, 1)
    d.blockchain.wavesPortfolio(generator2Addr) shouldBe Portfolio(balance = generator2BalanceAfterBlock3)
    d.blockchain.checkWavesAmount(wavesAmountBeforeVoting + d.blockchain.lastBlockReward.getOrElse(0L))

    log.debug("Append block 4")
    val wavesAmountBeforeCalculation = d.blockchain.wavesAmount(d.blockchain.height)
    d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = generator1, strictTime = true))

    val generator2BalanceAfterBlock4 = generator2BalanceAfterBlock2 - DepositInWavelets
    d.blockchain.checkCommitted(endorserAddrs*)
    d.blockchain.checkHasConflict(h = 3, 1)
    d.blockchain.wavesPortfolio(generator2Addr) shouldBe Portfolio(balance = generator2BalanceAfterBlock4)
    withClue("WAVES burnt: ") {
      d.blockchain.checkWavesAmount(wavesAmountBeforeCalculation + d.blockchain.lastBlockReward.getOrElse(0L) - DepositInWavelets)
    }
  }

  extension (self: Blockchain) {
    def checkCommitted(addrs: Address*)(using Position): Unit = {
      self.committedGenerators(self.currentGenerationPeriod.value).map(_._1) should contain theSameElementsInOrderAs addrs
    }

    def checkHasConflict(h: Int, idx: Int)(using Position): Unit = {
      self.conflictGenerators(self.currentGenerationPeriod.value) shouldBe mkConflictGenerators(h, idx)
    }

    def checkWavesAmount(x: BigInt)(using Position): Unit = {
      self.wavesAmount(self.height) shouldBe x
    }
  }

  private def mkConflictGenerators(h: Int, idxs: Int*): ConflictGenerators =
    ConflictGenerators.empty.appendAll(Height(h), GeneratorIndex.fromInts(idxs))
}
