package com.wavesplatform.finalization.conflict

import com.wavesplatform.TestValues
import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, FinalizationVoting}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.finalization.BaseFinalizationSpec
import com.wavesplatform.history.Domain
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{BalanceSnapshot, Blockchain, GeneratorIndex, Height, Portfolio}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{FreeSpec, NumericExt}
import com.wavesplatform.transaction.CommitToGenerationTransaction.DepositInWavelets
import com.wavesplatform.transaction.TxHelpers
import org.scalactic.source.Position
import org.scalatest.Assertion

/** Blocks:
  * 1. Genesis
  * 2. With commitments from two generators
  * 3. First block at epoch #1 with one valid and one conflict endorsements
  * 4. Empty block
  * 5. First block at epoch #2 with punishment applied for a conflict endorser, no one committed
  */
class ConflictEndorserBlocksBasicSuite extends BaseFinalizationSpec {
  private val validGenerator     = TxHelpers.signer(0)
  private val validGeneratorAddr = validGenerator.toAddress

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

    // TODO: value instead of comparison?
    override def after2WithCommitmentsCheck             = notRemoved
    override def after3WithNewEpochAndEndorsementsCheck = removed
    override def after4EmptyCheck                       = removed
    override def after5WithNewEpochAndPunishmentCheck   = notRemoved
  }.run()

  "waves amount" in new Scenario[Long] {
    override def getData = d => d.blockchain.wavesAmount(d.blockchain.height).toLong

    def base(height: Int): IgnorePosition[Long] = 100_000_000.waves + (height - 1) * 6.waves // init + n * mining rewards

    override def after2WithCommitmentsCheck             = _ shouldBe base(2)
    override def after3WithNewEpochAndEndorsementsCheck = _ shouldBe base(3)
    override def after4EmptyCheck                       = _ shouldBe base(4)
    override def after5WithNewEpochAndPunishmentCheck   = _ shouldBe (base(5) - DepositInWavelets)
  }.run()

  "waves portfolio" in new Scenario[Portfolio] {
    override def getData = d => d.blockchain.wavesPortfolio(conflictGeneratorAddr)

    val after1          = ENOUGH_AMT
    val after2          = after1 - TestValues.commitToGenerationFee
    val portfolioAfter2 = Portfolio(balance = after2, generationDeposit = DepositInWavelets)

    override def after2WithCommitmentsCheck             = _ shouldBe portfolioAfter2
    override def after3WithNewEpochAndEndorsementsCheck = _ shouldBe portfolioAfter2
    override def after4EmptyCheck                       = _ shouldBe portfolioAfter2
    override def after5WithNewEpochAndPunishmentCheck   = _ shouldBe Portfolio(balance = after2 - DepositInWavelets)
  }.run()

  "balance at height" in new Scenario[(Int, Long)] {
    override def getData = d => d.blockchain.balanceAtHeight(conflictGeneratorAddr, d.blockchain.height).value

    val after1 = ENOUGH_AMT
    val after2 = after1 - TestValues.commitToGenerationFee

    override def after2WithCommitmentsCheck             = _ shouldBe (2, after2)
    override def after3WithNewEpochAndEndorsementsCheck = _ shouldBe (2, after2)
    override def after4EmptyCheck                       = _ shouldBe (2, after2)
    override def after5WithNewEpochAndPunishmentCheck   = _ shouldBe (5, after2 - DepositInWavelets)
  }.run()

  "current generator balances" in new Scenario[Seq[(Address, Long)]] {
    override def getData = d => d.blockchain.currentGeneratorBalances()

    val after1 = ENOUGH_AMT
    val after2 = after1 - TestValues.commitToGenerationFee - DepositInWavelets

    val blockReward = 2.waves
    val totalTxnFee = 2 * TestValues.commitToGenerationFee

    val balancesAfter2 = Vector(
      validGeneratorAddr    -> (after2 + blockReward + totalTxnFee * 4 / 10),
      conflictGeneratorAddr -> after2
    )

    override def after2WithCommitmentsCheck             = _ shouldBe Nil
    override def after3WithNewEpochAndEndorsementsCheck = _ shouldBe balancesAfter2
    override def after4EmptyCheck                       = _ shouldBe balancesAfter2
    override def after5WithNewEpochAndPunishmentCheck   = _ shouldBe Nil
  }.run()

  "generator balance from API" in new Scenario[Long] { // Collected before applying block
    override def getData = d =>
      d.generatorsApi
        .generators(Height(d.blockchain.height))
        .collectFirst { case x if x.address == conflictGeneratorAddr => x.balance }
        .getOrElse(0L)

    val generatingBalanceAfter1 = ENOUGH_AMT
    val generatingBalanceAfter2 = generatingBalanceAfter1 - TestValues.commitToGenerationFee - DepositInWavelets

    override def after2WithCommitmentsCheck             = _ shouldBe 0
    override def after3WithNewEpochAndEndorsementsCheck = _ shouldBe generatingBalanceAfter2
    override def after4EmptyCheck                       = _ shouldBe generatingBalanceAfter2
    override def after5WithNewEpochAndPunishmentCheck   = _ shouldBe 0 // Not committed
  }.run()

  "generating balance" in new Scenario[Long] { // Collected after applying block
    override def getData = d => d.blockchain.generatingBalance(conflictGeneratorAddr)

    val after1 = ENOUGH_AMT
    val after2 = after1 - TestValues.commitToGenerationFee - DepositInWavelets

    override def after2WithCommitmentsCheck             = _ shouldBe after2
    override def after3WithNewEpochAndEndorsementsCheck = _ shouldBe after2
    override def after4EmptyCheck                       = _ shouldBe after2
    override def after5WithNewEpochAndPunishmentCheck   = _ shouldBe after2 // Punished for deposit, but deposit gone, so no difference
  }.run()

  "balance snapshots" in new Scenario[Seq[BalanceSnapshot]] {
    override def getData = d => d.blockchain.balanceSnapshots(conflictGeneratorAddr, from = 2, to = None)

    val after1 = ENOUGH_AMT
    val after2 = after1 - TestValues.commitToGenerationFee
    val after5 = after2 - DepositInWavelets

    override def after2WithCommitmentsCheck             = _ => succeed
    override def after3WithNewEpochAndEndorsementsCheck = _ => succeed
    override def after4EmptyCheck                       = _ => succeed
    override def after5WithNewEpochAndPunishmentCheck =
      _ should contain theSameElementsInOrderAs Seq(
        bs(height = 5, regularBalance = after5), // Punishment
        // height = 4 // Nothing happened
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
    def after3WithNewEpochAndEndorsementsCheck: Check
    def after4EmptyCheck: Check
    def after5WithNewEpochAndPunishmentCheck: Check

    def run(): Assertion = withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(validGenerator, conflictGenerator)
    ) { d =>
      def data(using Position) = getData(d)

      log.debug(s"Append block 2 with commitments")
      val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
      val block2WithCommitments = d.createBlock(version = Block.ProtoBlockVersion, txs = txs, generator = validGenerator, strictTime = true)
      d.appender.appendBlock(block2WithCommitments)
      after2WithCommitmentsCheck(data)

      log.debug(s"Append block 3 with votes")
      val block3WithVotes = d.createBlock(
        version = Block.ProtoBlockVersion,
        txs = Nil,
        generator = validGenerator,
        strictTime = true,
        finalizationVoting = Some(
          FinalizationVoting(
            conflict = Vector(mkConflictEndorsement(conflictGenerator, GeneratorIndex(1), block2WithCommitments))
          )
        )
      )
      d.appender.appendBlock(block3WithVotes)
      after3WithNewEpochAndEndorsementsCheck(data)

      log.debug("Append block 4")
      val block4 = d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = validGenerator, strictTime = true)
      d.appender.appendBlock(block4)
      after4EmptyCheck(data)

      log.debug("Append block 5 of new epoch, apply punishment")
      d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = validGenerator, strictTime = true))
      after5WithNewEpochAndPunishmentCheck(data)

      log.debug("Rollback to 4")
      d.blockchain.removeAfter(block4.id()) should beRight
      after4EmptyCheck(data)

      log.debug("Rollback to 3")
      d.blockchain.removeAfter(block3WithVotes.id()) should beRight
      after3WithNewEpochAndEndorsementsCheck(data)

      log.debug("Rollback to 2")
      d.blockchain.removeAfter(block2WithCommitments.id()) should beRight
      after2WithCommitmentsCheck(data)
    }
  }
}
