package com.wavesplatform.finalization

import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, BlockEndorsement, FinalizationVoting}
import com.wavesplatform.consensus.GeneratingBalanceProvider
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{BalanceSnapshot, Blockchain, ConflictGenerators, GeneratorIndex, GenesisBlockHeight, Height, Portfolio}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.CommitToGenerationTransaction.DepositInWavelets
import com.wavesplatform.transaction.{CommitToGenerationTransaction, TxHelpers}
import org.scalactic.source.Position
import org.scalatest.Assertion

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
    val generator2Addr               = generator2.toAddress
    val generator2WavesBalanceAfter1 = ENOUGH_AMT

    log.debug(s"Append block 2 with commitments")
    val endorsers     = Seq(generator1, generator2)
    val endorserAddrs = endorsers.map(_.toAddress)
    val txs           = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = 3, x))

    val block2WithCommitments = d.createBlock(version = Block.ProtoBlockVersion, txs = txs, generator = generator1, strictTime = true)
    d.appender.appendBlock(block2WithCommitments)

    val wavesAmountAfter2       = d.blockchain.wavesAmount(d.blockchain.height)
    val generator2BalanceAfter2 = generator2WavesBalanceAfter1 - txs(1).fee.value
    d.blockchain.wavesPortfolio(generator2Addr) shouldBe Portfolio(
      balance = generator2BalanceAfter2,
      generationDeposit = DepositInWavelets
    )

    log.debug(s"Append block 3 with votes")
    val otherFinalizedBlockId = TxHelpers.randomBlockId
    val block3WithVotes = d.createBlock(
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
                endorsedId = block2WithCommitments.id()
              )
            )
          )
        )
      )
    )
    d.appender.appendBlock(block3WithVotes)

    val wavesAmountAfter3                = wavesAmountAfter2 + d.blockchain.lastBlockReward.getOrElse(0L)
    val generator2WavesBalanceAfter3     = generator2BalanceAfter2
    val generator2GeneratorBalanceAfter3 = generator2BalanceAfter2 - DepositInWavelets

    def checkAfter3()(using Position): Unit = {
      d.checkCommitted(endorserAddrs*)
      d.checkHasConflict(h = 3, 1)
      d.checkWavesAmount(wavesAmountAfter3)
      d.blockchain.wavesPortfolio(generator2Addr) shouldBe Portfolio(balance = generator2WavesBalanceAfter3)
      d.blockchain.balanceAtHeight(generator2Addr, d.blockchain.height).value shouldBe (2, generator2BalanceAfter2)
      d.checkGeneratorBalance(generator2Addr, generator2GeneratorBalanceAfter3)
      d.checkGeneratorBalanceFromApi(generator2Addr, generator2GeneratorBalanceAfter3)
    }
    checkAfter3()

    log.debug("Append block 4 with punishment")
    val block4WithPunishment = d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = generator1, strictTime = true)
    d.appender.appendBlock(block4WithPunishment)

    val wavesAmountAfter4            = wavesAmountAfter3 + d.blockchain.lastBlockReward.getOrElse(0L) - DepositInWavelets
    val generator2BalanceAfterBlock4 = generator2WavesBalanceAfter3 - DepositInWavelets

    def checkAfter4()(using Position): Unit = {
      d.checkCommitted(endorserAddrs*)
      d.checkHasConflict(h = 3, 1)
      withClue("WAVES burnt: ") {
        d.checkWavesAmount(wavesAmountAfter4)
      }
      d.blockchain.wavesPortfolio(generator2Addr) shouldBe Portfolio(balance = generator2BalanceAfterBlock4)
      d.blockchain.balanceAtHeight(generator2Addr, d.blockchain.height).value shouldBe (4, generator2BalanceAfterBlock4)
      d.checkGeneratorBalance(generator2Addr)                                          // Collected after applying block #4
      d.checkGeneratorBalanceFromApi(generator2Addr, generator2GeneratorBalanceAfter3) // Collected before applying block #4
    }
    checkAfter4()

    log.debug("Append block 5 of new epoch")
    d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = generator1, strictTime = true))

    d.checkCommitted()
    d.checkHasConflict(h = 3, 1)
    d.checkWavesAmount(wavesAmountAfter4 + d.blockchain.lastBlockReward.getOrElse(0L))
    d.blockchain.wavesPortfolio(generator2Addr) shouldBe Portfolio(balance = generator2BalanceAfterBlock4)
    d.blockchain.balanceAtHeight(generator2Addr, d.blockchain.height).value shouldBe (4, generator2BalanceAfterBlock4)
    d.checkGeneratorBalance(generator2Addr)
    // d.checkGeneratorBalanceFromApi(generator2Addr) // Not checking, because no one committed

    d.blockchain.balanceSnapshots(generator2Addr, from = 2, to = None) should contain theSameElementsInOrderAs Seq(
      bs(height = 5, regularBalance = generator2BalanceAfterBlock4),
      bs(height = 4, regularBalance = generator2BalanceAfterBlock4, punished = true), // Processed conflict endorsement
      // height = 3 // Sent conflict endorsement
      bs(height = 2, regularBalance = generator2BalanceAfter2, deposits = 1) // Sent CommitToGeneration
    )

    log.debug("Rollback to 4")
    d.blockchain.removeAfter(block4WithPunishment.id()) should beRight
    checkAfter4()

    log.debug("Rollback to 3")
    d.blockchain.removeAfter(block3WithVotes.id()) should beRight
    checkAfter3()
  }

  extension (d: Domain) {
    def checkCommitted(addresses: Address*)(using Position): Assertion =
      d.blockchain.committedGenerators(d.blockchain.currentGenerationPeriod.value).map(_._1) should contain theSameElementsInOrderAs addresses

    def checkHasConflict(h: Int, idx: Int)(using Position): Assertion =
      d.blockchain.conflictGenerators(d.blockchain.generationPeriodOf(Height(h)).value) shouldBe mkConflictGenerators(h, idx)

    def checkWavesAmount(x: BigInt)(using Position): Assertion =
      d.blockchain.wavesAmount(d.blockchain.height) shouldBe x

    def checkGeneratorBalance(address: Address, balance: Long = 0L)(using Position): Assertion =
      GeneratingBalanceProvider.generatorBalance(d.blockchain, address) shouldBe balance

    def checkGeneratorBalanceFromApi(address: Address, balance: Long = 0L)(using Position): Assertion =
      d.generatorsApi
        .generators(Height(d.blockchain.height))
        .collectFirst { case x if x.address == address => x.balance }
        .value shouldBe balance
  }

  private def mkConflictGenerators(h: Int, idxs: Int*): ConflictGenerators =
    ConflictGenerators.empty.appendAll(Height(h), GeneratorIndex.fromInts(idxs))

  private def bs(height: Int, regularBalance: Long, deposits: Int = 0, punished: Boolean = false): BalanceSnapshot =
    BalanceSnapshot(height, regularBalance, 0L, 0L, CommitToGenerationTransaction.DepositInWavelets * deposits, punished)
}
