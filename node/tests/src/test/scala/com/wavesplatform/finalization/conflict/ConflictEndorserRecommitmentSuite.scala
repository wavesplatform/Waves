package com.wavesplatform.finalization.conflict

import com.wavesplatform.TestValues
import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, BlockEndorsement, FinalizationVoting}
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.finalization.BaseFinalizationSpec
import com.wavesplatform.history.Domain
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{Blockchain, ConflictGenerators, GeneratorIndex, GenesisBlockHeight, Height, Portfolio}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.CommitToGenerationTransaction.DepositInWavelets
import com.wavesplatform.transaction.TxHelpers
import org.scalactic.source.Position
import org.scalatest.Assertion

class ConflictEndorserRecommitmentSuite extends BaseFinalizationSpec {
  private val validGenerator = TxHelpers.signer(0)

  private val conflictGenerator     = TxHelpers.signer(1)
  private val conflictGeneratorAddr = conflictGenerator.toAddress

  private val endorsers     = Seq(validGenerator, conflictGenerator)
  private val endorserAddrs = endorsers.map(_.toAddress)

  private val baseSettings = DomainPresets.DeterministicFinality.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  private val defaultSettings = baseSettings.configure(
    _.copy(
      generationPeriodLength = 2,
      lightNodeBlockFieldsAbsenceInterval = 0
    )
  )

  "punished and committed to next" in withDomain(defaultSettings, AddrWithBalance.enoughBalances(validGenerator, conflictGenerator)) { d =>
    log.debug(s"Append block 2 with commitments")
    val block2Txs             = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = 3, x))
    val block2WithCommitments = d.createBlock(version = Block.ProtoBlockVersion, txs = block2Txs, generator = validGenerator, strictTime = true)
    d.appender.appendBlock(block2WithCommitments)

    val balanceAfter1 = ENOUGH_AMT
    val balanceAfter2 = balanceAfter1 - TestValues.commitToGenerationFee

    log.debug(s"Append block 3 with votes")
    val otherFinalizedBlockId = TxHelpers.randomBlockId
    val block3WithVotes = d.createBlock(
      version = Block.ProtoBlockVersion,
      txs = Nil,
      generator = validGenerator,
      strictTime = true,
      finalizationVoting = Some(
        FinalizationVoting(
          conflict = Vector(
            BlockEndorsement.signed(
              BlsKeyPair(conflictGenerator.privateKey),
              GeneratorIndex(1),
              otherFinalizedBlockId,
              finalizedHeight = GenesisBlockHeight,
              endorsedId = block2WithCommitments.id()
            )
          )
        )
      )
    )
    d.appender.appendBlock(block3WithVotes)

    val block4Txs             = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = 5, x))
    val block4WithCommitments = d.createBlock(version = Block.ProtoBlockVersion, txs = block4Txs, generator = validGenerator, strictTime = true)
    d.appender.appendBlock(block4WithCommitments)

    val balanceAfter4 = ENOUGH_AMT - 2 * TestValues.commitToGenerationFee

    log.debug("Append block 5 of new epoch with punishment and commitment")
    d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = validGenerator, strictTime = true))

    val balanceAfter5 = balanceAfter4 - DepositInWavelets

    d.checkCommitted(endorserAddrs*)
    d.blockchain.wavesPortfolio(conflictGeneratorAddr) shouldBe Portfolio(balance = balanceAfter5, generationDeposit = DepositInWavelets)
    d.blockchain.balanceAtHeight(conflictGeneratorAddr, d.blockchain.height).value shouldBe (5, balanceAfter5)
    d.checkGeneratingBalance(conflictGeneratorAddr, balanceAfter5 - DepositInWavelets)
    d.checkGeneratorBalanceFromApi(conflictGeneratorAddr, balanceAfter5 - DepositInWavelets)

    d.blockchain.balanceSnapshots(conflictGeneratorAddr, from = 2, to = None) should contain theSameElementsInOrderAs Seq(
      bs(height = 5, regularBalance = balanceAfter5, deposits = 1), // Punished and committed on next
      bs(height = 4, regularBalance = balanceAfter4, deposits = 2), // Commitment
      // height = 3 // Sent conflict endorsement
      bs(height = 2, regularBalance = balanceAfter2, deposits = 1) // Sent CommitToGeneration
    )
  }

  extension (d: Domain)(using Position) {
    def checkCommitted(addresses: Address*): Assertion = withClue(s"checkCommitted: addresses=$addresses, ") {
      d.blockchain.committedGenerators(d.blockchain.currentGenerationPeriod.value).map(_._1) should contain theSameElementsInOrderAs addresses
    }

    def checkHasConflict(h: Int, idx: Int): Assertion = withClue(s"checkHasConflict: h=$h, idx=$idx, ") {
      d.blockchain.conflictGenerators(d.blockchain.generationPeriodOf(Height(h)).value) shouldBe mkConflictGenerators(h, idx)
    }

    def checkHasNoConflict(h: Int): Assertion = withClue(s"checkHasNoConflict: h=$h, ") {
      d.blockchain.conflictGenerators(d.blockchain.generationPeriodOf(Height(h)).value) shouldBe ConflictGenerators.empty
    }

    def checkWavesAmount(x: BigInt): Assertion = withClue("wavesAmount: ") {
      d.blockchain.wavesAmount(d.blockchain.height) shouldBe x
    }

    def checkGeneratingBalance(address: Address, balance: Long = 0L): Assertion = withClue(s"checkGeneratingBalance: address=$address, ") {
      d.blockchain.generatingBalance(address) shouldBe balance
      // GeneratingBalanceProvider.balance(d.blockchain, address) shouldBe balance
    }

    def checkGeneratorBalanceFromApi(address: Address, balance: Long = 0L, h: Int = d.blockchain.height): Assertion =
      withClue(s"checkGeneratorBalanceFromApi: h=$h, address=$address, ") {
        d.generatorsApi
          .generators(Height(h))
          .collectFirst { case x if x.address == address => x.balance }
          .getOrElse(0L) shouldBe balance
      }
  }
}
