package com.wavesplatform.finalization.conflict

import com.wavesplatform.TestValues
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.finalization.BaseFinalizationSpec
import com.wavesplatform.history.Domain
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{BalanceSnapshot, GeneratorIndex, Height, Portfolio}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.NumericExt
import com.wavesplatform.transaction.CommitToGenerationTransaction.DepositInWavelets
import com.wavesplatform.transaction.TxHelpers
import org.scalactic.source.Position
import org.scalatest.Assertion

/** Blocks:
  * 1. Genesis
  * 2. With commitments from two generators
  * 3. First block at period #1
  *   1. Microblock with one valid endorsement
  * 4. Empty block with punishment applied for a conflict endorser
  * 5. First block at period #2, no one committed
  */
class ConflictEndorserBlocksNgSuite extends BaseFinalizationSpec {
  private val validGenerator = TxHelpers.signer(0)

  private val conflictGenerator     = TxHelpers.signer(1)
  private val conflictGeneratorAddr = conflictGenerator.toAddress

  private val baseSettings = DomainPresets.DeterministicFinality.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  private val defaultSettings = baseSettings.configure(
    _.copy(
      generationPeriodLength = 2,
      lightNodeBlockFieldsAbsenceInterval = 0
    )
  )

  private val generators             = Seq(validGenerator, conflictGenerator)
  private val conflictGeneratorIndex = GeneratorIndex(1)

  "finalization happens in next keyblock" in new Scenario[Height] {
    override def getData = d => d.blockchain.finalizedHeight.value

    override def after2WithCommitmentsCheck                   = _ shouldBe Height(1)
    override def after3KeyBlockWithNewPeriodCheck             = _ shouldBe Height(1)
    override def after3MicroBlockWithConflictEndorsementCheck = _ shouldBe Height(1)
    override def after4WithPunishmentCheck                    = _ shouldBe Height(2)
    override def after5WithNewPeriodCheck                     = _ shouldBe Height(3) // Miner has enough generating balance
  }.run()

  "removed from generator set" in new Scenario[Set[GeneratorIndex]] {
    override def getData = d => d.blockchain.conflictGenerators(d.blockchain.currentGenerationPeriod.value).all

    private val removed: IgnorePositionCheck    = _ shouldBe Set(conflictGeneratorIndex)
    private val notRemoved: IgnorePositionCheck = _ shouldBe empty

    override def after2WithCommitmentsCheck                   = notRemoved
    override def after3KeyBlockWithNewPeriodCheck             = notRemoved
    override def after3MicroBlockWithConflictEndorsementCheck = removed
    override def after4WithPunishmentCheck                    = removed
    override def after5WithNewPeriodCheck                     = notRemoved
  }.run()

  "waves amount" in new Scenario[Long] {
    override def getData = d => d.blockchain.wavesAmount(d.blockchain.height).toLong

    def base(height: Int): IgnorePosition[Long] = 100_000_000.waves + (height - 1) * 6.waves // init + n * mining rewards

    override def after2WithCommitmentsCheck                   = _ shouldBe base(2)
    override def after3KeyBlockWithNewPeriodCheck             = _ shouldBe base(3)
    override def after3MicroBlockWithConflictEndorsementCheck = _ shouldBe base(3)
    override def after4WithPunishmentCheck                    = _ shouldBe (base(4) - DepositInWavelets)
    override def after5WithNewPeriodCheck                     = _ shouldBe (base(5) - DepositInWavelets)
  }.run()

  "waves portfolio" in new Scenario[Portfolio] {
    override def getData = d => d.blockchain.wavesPortfolio(conflictGeneratorAddr)

    val after1          = ENOUGH_AMT
    val after2          = after1 - TestValues.commitToGenerationFee
    val portfolioAfter2 = Portfolio(balance = after2, generationDeposit = DepositInWavelets)

    override def after2WithCommitmentsCheck                   = _ shouldBe portfolioAfter2
    override def after3KeyBlockWithNewPeriodCheck             = _ shouldBe portfolioAfter2
    override def after3MicroBlockWithConflictEndorsementCheck = _ shouldBe portfolioAfter2
    override def after4WithPunishmentCheck                    = _ shouldBe Portfolio(balance = after2 - DepositInWavelets)
    override def after5WithNewPeriodCheck                     = _ shouldBe Portfolio(balance = after2 - DepositInWavelets)
  }.run()

  "balance at height" in new Scenario[(Int, Long)] {
    override def getData = d => d.blockchain.balanceAtHeight(conflictGeneratorAddr, d.blockchain.height).value

    val after1        = ENOUGH_AMT
    val after2        = after1 - TestValues.commitToGenerationFee
    val balanceAfter2 = (2, after2)

    override def after2WithCommitmentsCheck                   = _ shouldBe balanceAfter2
    override def after3KeyBlockWithNewPeriodCheck             = _ shouldBe balanceAfter2
    override def after3MicroBlockWithConflictEndorsementCheck = _ shouldBe balanceAfter2
    override def after4WithPunishmentCheck                    = _ shouldBe (4, after2 - DepositInWavelets)
    override def after5WithNewPeriodCheck                     = _ shouldBe (4, after2 - DepositInWavelets)
  }.run()

  "generating balance" in new Scenario[Long] { // Collected after applying block
    override def getData = d => d.blockchain.generatingBalance(conflictGeneratorAddr)

    val after1 = ENOUGH_AMT
    val after2 = after1 - TestValues.commitToGenerationFee - DepositInWavelets

    override def after2WithCommitmentsCheck                   = _ shouldBe after2
    override def after3KeyBlockWithNewPeriodCheck             = _ shouldBe after2
    override def after3MicroBlockWithConflictEndorsementCheck = _ shouldBe after2
    override def after4WithPunishmentCheck                    = _ shouldBe after2
    override def after5WithNewPeriodCheck                     = _ shouldBe after2 // Punished for deposit, but deposit gone, so no difference
  }.run()

  "balance snapshots" in new Scenario[Seq[BalanceSnapshot]] {
    override def getData = d => d.blockchain.balanceSnapshots(conflictGeneratorAddr, from = 2, to = None)

    val after1 = ENOUGH_AMT
    val after2 = after1 - TestValues.commitToGenerationFee
    val after4 = after2 - DepositInWavelets

    override def after2WithCommitmentsCheck = _ should contain theSameElementsInOrderAs Seq(
      bs(height = 2, regularBalance = after2, deposits = 1) // Sent CommitToGeneration
    )

    override def after3KeyBlockWithNewPeriodCheck = _ should contain theSameElementsInOrderAs Seq(
      bs(height = 3, regularBalance = after2, deposits = 1) // Sent conflict endorsement
    )

    override def after3MicroBlockWithConflictEndorsementCheck = _ should contain theSameElementsInOrderAs Seq(
      bs(height = 3, regularBalance = after2, deposits = 1) // Sent conflict endorsement
    )

    override def after4WithPunishmentCheck = _ should contain theSameElementsInOrderAs Seq(
      bs(height = 4, regularBalance = after4), // Punishment
      // height = 3 // Sent conflict endorsement
      bs(height = 2, regularBalance = after2, deposits = 1) // Sent CommitToGeneration
    )

    override def after5WithNewPeriodCheck = _ should contain theSameElementsInOrderAs Seq(
      bs(height = 5, regularBalance = after4), // New period
      bs(height = 4, regularBalance = after4), // Punishment
      // height = 3 // Sent conflict endorsement
      bs(height = 2, regularBalance = after2, deposits = 1) // Sent CommitToGeneration
    )
  }.run()

  private type IgnorePosition[T] = Position ?=> T

  private trait Scenario[T] {
    type Check               = T => Assertion
    type IgnorePositionCheck = IgnorePosition[Check]

    def getData: IgnorePosition[Domain => T]

    def after2WithCommitmentsCheck: Check
    def after3KeyBlockWithNewPeriodCheck: Check
    def after3MicroBlockWithConflictEndorsementCheck: Check
    def after4WithPunishmentCheck: Check
    def after5WithNewPeriodCheck: Check

    private val otherAcc1 = TxHelpers.signer(1000)
    private val otherAcc2 = TxHelpers.signer(1001)

    def run(): Assertion = withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(validGenerator, conflictGenerator, otherAcc1)
    ) { d =>
      def data(using Position) = getData(d)

      log.debug(s"Append block 2 with commitments")
      val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
      val block2WithCommitments = d.createBlock(txs, generator = validGenerator, strictTime = true)
      d.appender.appendBlock(block2WithCommitments)
      after2WithCommitmentsCheck(data)

      d.appender.appendBlock(block2WithCommitments)
      val block3 = d.createBlock(generator = validGenerator, strictTime = true)
      log.debug(s"Append block 3")
      d.appender.appendBlock(block3)
      after3KeyBlockWithNewPeriodCheck(data)

      log.debug(s"Append microblock with conflict endorsement")
      val microBlockWithTxn = d.createMicroBlock(
        signer = Some(validGenerator),
        finalizationVoting = Some(mkFinalizationVoting().withConflict(conflictGenerator, GeneratorIndex(1), block2WithCommitments.id()))
      )(TxHelpers.transfer(otherAcc1, otherAcc2.toAddress))
      d.appendMicroBlock(microBlockWithTxn)
      after3MicroBlockWithConflictEndorsementCheck(data)

      log.debug("Append block 4")
      val block4 = d.createBlock(generator = validGenerator, strictTime = true)
      d.appender.appendBlock(block4)
      after4WithPunishmentCheck(data)

      log.debug("Append block 5 of new period, apply punishment")
      d.appender.appendBlock(d.createBlock(generator = validGenerator, strictTime = true))
      after5WithNewPeriodCheck(data)
    }
  }
}
