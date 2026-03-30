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
  * 3. First block at period #1 with one valid and one conflict endorsements
  * 4. Empty block with punishment applied for a conflict endorser
  * 5. First block at period #2, no one committed
  */
class ConflictEndorserBlocksBasicSuite extends BaseFinalizationSpec {
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

  "in conflict" in new Scenario[Set[GeneratorIndex]] {
    override def getData = d => d.blockchain.conflictGenerators(d.blockchain.currentGenerationPeriod.value).all

    private val removed: IgnorePositionCheck    = _ shouldBe Set(conflictGeneratorIndex)
    private val notRemoved: IgnorePositionCheck = _ shouldBe empty

    override def after2WithCommitmentsCheck              = notRemoved
    override def after3WithNewPeriodAndEndorsementsCheck = removed
    override def after4WithPunishmentCheck               = removed
    override def after5WithNewPeriodCheck                = notRemoved
  }.run()

  "waves amount" in new Scenario[Long] {
    override def getData = d => d.blockchain.wavesAmount(d.blockchain.height).toLong

    def base(height: Int): IgnorePosition[Long] = 100_000_000.waves + (height - 1) * 6.waves // init + n * mining rewards

    override def after2WithCommitmentsCheck              = _ shouldBe base(2)
    override def after3WithNewPeriodAndEndorsementsCheck = _ shouldBe base(3)
    override def after4WithPunishmentCheck               = _ shouldBe (base(4) - DepositInWavelets)
    override def after5WithNewPeriodCheck                = _ shouldBe (base(5) - DepositInWavelets)
  }.run()

  "waves portfolio" in new Scenario[Portfolio] {
    override def getData = d => d.blockchain.wavesPortfolio(conflictGeneratorAddr)

    val after1          = ENOUGH_AMT
    val after2          = after1 - TestValues.commitToGenerationFee
    val portfolioAfter2 = Portfolio(balance = after2, generationDeposit = DepositInWavelets)

    override def after2WithCommitmentsCheck              = _ shouldBe portfolioAfter2
    override def after3WithNewPeriodAndEndorsementsCheck = _ shouldBe portfolioAfter2
    override def after4WithPunishmentCheck               = _ shouldBe Portfolio(balance = after2 - DepositInWavelets)
    override def after5WithNewPeriodCheck                = _ shouldBe Portfolio(balance = after2 - DepositInWavelets)
  }.run()

  "balance at height" in new Scenario[(Int, Long)] {
    override def getData = d => d.blockchain.balanceAtHeight(conflictGeneratorAddr, d.blockchain.height).value

    val after1 = ENOUGH_AMT
    val after2 = after1 - TestValues.commitToGenerationFee

    override def after2WithCommitmentsCheck              = _ shouldBe (2, after2)
    override def after3WithNewPeriodAndEndorsementsCheck = _ shouldBe (2, after2)
    override def after4WithPunishmentCheck               = _ shouldBe (4, after2 - DepositInWavelets)
    override def after5WithNewPeriodCheck                = _ shouldBe (4, after2 - DepositInWavelets)
  }.run()

  "generator balance from API" in new Scenario[Option[Long]] { // Collected before applying block
    override def getData = d =>
      d.generatorsApi
        .generators(Height(d.blockchain.height))
        .collectFirst { case x if x.address == conflictGeneratorAddr => x.balance }
        .flatten

    override def after2WithCommitmentsCheck              = _ shouldBe None
    override def after3WithNewPeriodAndEndorsementsCheck = _ shouldBe Some(0)
    override def after4WithPunishmentCheck               = _ shouldBe Some(0)
    override def after5WithNewPeriodCheck                = _ shouldBe None // Not committed
  }.run()

  "generating balance" in new Scenario[Long] { // Collected after applying block
    override def getData = d => d.blockchain.generatingBalance(conflictGeneratorAddr)

    val after1 = ENOUGH_AMT
    val after2 = after1 - TestValues.commitToGenerationFee - DepositInWavelets

    override def after2WithCommitmentsCheck              = _ shouldBe after2
    override def after3WithNewPeriodAndEndorsementsCheck = _ shouldBe after2
    override def after4WithPunishmentCheck               = _ shouldBe after2 // Punished for deposit, but deposit gone, so no difference
    override def after5WithNewPeriodCheck                = _ shouldBe after2
  }.run()

  "balance snapshots" in new Scenario[Seq[BalanceSnapshot]] {
    override def getData = d => d.blockchain.balanceSnapshots(conflictGeneratorAddr, from = 2, to = None)

    val after1 = ENOUGH_AMT
    val after2 = after1 - TestValues.commitToGenerationFee
    val after4 = after2 - DepositInWavelets

    override def after2WithCommitmentsCheck = _ should contain theSameElementsInOrderAs Seq(
      bs(height = 2, regularBalance = after2, deposits = 1) // Sent CommitToGeneration
    )

    // Different results for checks after append and rollback because of fix in SnapshotBlockchain (see height2Fix)
    override def after3WithNewPeriodAndEndorsementsCheck = _ => succeed

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
    def after3WithNewPeriodAndEndorsementsCheck: Check
    def after4WithPunishmentCheck: Check
    def after5WithNewPeriodCheck: Check

    def run(): Assertion = withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(validGenerator, conflictGenerator)
    ) { d =>
      def data(using Position) = getData(d)

      log.debug(s"Append block 2 with commitments")
      val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
      val block2WithCommitments = d.createBlock(txs, generator = validGenerator, strictTime = true)
      d.appender.appendBlock(block2WithCommitments)
      after2WithCommitmentsCheck(data)

      log.debug(s"Append block 3 with votes")
      val block3WithVotes = d.createBlock(
        generator = validGenerator,
        strictTime = true,
        finalizationVoting = Some(mkFinalizationVoting().withConflict(conflictGenerator, GeneratorIndex(1), block2WithCommitments.id()))
      )
      d.appender.appendBlock(block3WithVotes)
      after3WithNewPeriodAndEndorsementsCheck(data)

      log.debug("Append block 4")
      val block4 = d.createBlock(generator = validGenerator, strictTime = true)
      d.appender.appendBlock(block4)
      after4WithPunishmentCheck(data)

      log.debug("Append block 5 of new period, apply punishment")
      d.appender.appendBlock(d.createBlock(generator = validGenerator, strictTime = true))
      after5WithNewPeriodCheck(data)

      log.debug("Rollback to 4")
      d.blockchain.removeAfter(block4.id()) should beRight
      after4WithPunishmentCheck(data)

      log.debug("Rollback to 3")
      d.blockchain.removeAfter(block3WithVotes.id()) should beRight
      after3WithNewPeriodAndEndorsementsCheck(data)

      log.debug("Rollback to 2")
      d.blockchain.removeAfter(block2WithCommitments.id()) should beRight
      after2WithCommitmentsCheck(data)
    }
  }
}
