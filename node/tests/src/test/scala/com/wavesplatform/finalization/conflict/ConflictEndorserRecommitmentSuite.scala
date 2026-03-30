package com.wavesplatform.finalization.conflict

import com.wavesplatform.TestValues
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.finalization.BaseFinalizationSpec
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{GeneratorIndex, Height, Portfolio}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.transaction.CommitToGenerationTransaction.DepositInWavelets
import com.wavesplatform.transaction.TxHelpers

class ConflictEndorserRecommitmentSuite extends BaseFinalizationSpec {
  private val validGenerator = TxHelpers.signer(0)

  private val conflictGenerator     = TxHelpers.signer(1)
  private val conflictGeneratorAddr = conflictGenerator.toAddress

  private val generators     = Seq(validGenerator, conflictGenerator)
  private val generatorAddrs = generators.map(_.toAddress)

  private val baseSettings = DomainPresets.DeterministicFinality.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  private val defaultSettings = baseSettings.configure(
    _.copy(
      generationPeriodLength = 2,
      lightNodeBlockFieldsAbsenceInterval = 0
    )
  )

  "punished and committed to next" in withDomain(defaultSettings, AddrWithBalance.enoughBalances(validGenerator, conflictGenerator)) { d =>
    log.debug(s"Append block 2 with commitments")
    val block2Txs             = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
    val block2WithCommitments = d.createBlock(block2Txs, generator = validGenerator, strictTime = true)
    d.appender.appendBlock(block2WithCommitments)

    val balanceAfter1 = ENOUGH_AMT
    val balanceAfter2 = balanceAfter1 - TestValues.commitToGenerationFee

    log.debug(s"Append block 3 with votes")
    val block3WithVotes = d.createBlock(
      generator = validGenerator,
      strictTime = true,
      finalizationVoting = Some(mkFinalizationVoting().withConflict(conflictGenerator, GeneratorIndex(1), block2WithCommitments.id()))
    )
    d.appender.appendBlock(block3WithVotes)

    log.debug(s"Append block 3 with punishment and commitment")
    val block4Txs             = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(5), x))
    val block4WithCommitments = d.createBlock(block4Txs, generator = validGenerator, strictTime = true)
    d.appender.appendBlock(block4WithCommitments)

    val balanceAfter4 = ENOUGH_AMT - 2 * TestValues.commitToGenerationFee - DepositInWavelets

    log.debug("Append block 5 of new period")
    d.appender.appendBlock(d.createBlock(generator = validGenerator, strictTime = true))

    withClue(s"checkCommitted: ") {
      d.blockchain.committedGenerators(d.blockchain.currentGenerationPeriod.value).map(_._1) should contain theSameElementsInOrderAs generatorAddrs
    }

    d.blockchain.wavesPortfolio(conflictGeneratorAddr) shouldBe Portfolio(balance = balanceAfter4, generationDeposit = DepositInWavelets)
    d.blockchain.balanceAtHeight(conflictGeneratorAddr, d.blockchain.height).value shouldBe (4, balanceAfter4)

    withClue(s"checkGeneratingBalance: ") {
      d.blockchain.generatingBalance(conflictGeneratorAddr) shouldBe balanceAfter4 - DepositInWavelets
    }

    withClue(s"checkGeneratorBalanceFromApi: ") {
      d.generatorsApi
        .generators(Height(d.blockchain.height))
        .collectFirst { case x if x.address == conflictGeneratorAddr => x.balance }
        .value shouldBe Some(balanceAfter4 - DepositInWavelets)
    }

    d.blockchain.balanceSnapshots(conflictGeneratorAddr, from = 2, to = None) should contain theSameElementsInOrderAs Seq(
      bs(height = 5, regularBalance = balanceAfter4, deposits = 1), // New period
      bs(height = 4, regularBalance = balanceAfter4, deposits = 1), // Punishment and commitment
      // height = 3 // Sent conflict endorsement
      bs(height = 2, regularBalance = balanceAfter2, deposits = 1) // Sent CommitToGeneration
    )
  }
}
