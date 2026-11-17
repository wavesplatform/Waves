package com.wavesplatform.finalization

import cats.syntax.option.*
import com.wavesplatform.api.common.CommonGeneratorsApi.GeneratorEntry
import com.wavesplatform.block.{Block, FinalizationVoting}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.network.{ExtensionBlocks, InvalidBlockStorage, PeerDatabase}
import com.wavesplatform.state.*
import com.wavesplatform.state.appender.ExtensionAppender
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.transaction.{Transaction, TxHelpers}
import io.netty.channel.embedded.EmbeddedChannel
import monix.execution.Scheduler.Implicits.global
import org.scalactic.Prettifier

class ExtensionAppenderAfterFinalizationSpec extends BaseFinalizationSpec {
  private val defaultSettings = DomainPresets.DeterministicFinality
    .addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
    .setFeaturesHeight(BlockchainFeatures.DeterministicFinality -> 3)
    .configure(
      _.copy(
        generationPeriodLength = 2,
        lightNodeBlockFieldsAbsenceInterval = 0
      )
    )

  private val committedGenerator1 = TxHelpers.signer(0)

  private val committedGenerator2    = TxHelpers.signer(1)
  private val committedGenerator2Idx = GeneratorIndex(1)

  private val committedGenerator3    = TxHelpers.signer(2)
  private val committedGenerator3Idx = GeneratorIndex(2)

  private val allGenerators = Seq(committedGenerator1, committedGenerator2, committedGenerator3)

  "restores the original branch and its generator info when a better fork fails to apply" in withDomain(
    defaultSettings,
    AddrWithBalance.enoughBalances(allGenerators*)
  ) { d =>
    def mkCommitments(period: GenerationPeriod = d.blockchain.currentGenerationPeriod.value.next): Seq[Transaction] =
      allGenerators.map(x => TxHelpers.commitToGeneration(period.start, x))

    def appendBlock(txs: Seq[Transaction] = Nil): Block = {
      val b = d.createBlock(txs, generator = committedGenerator1, strictTime = true)
      d.appender.appendBlock(b)
      b
    }

    def appendMicroBlock(fv: FinalizationVoting): Unit =
      d.appendMicroBlockE(
        d.createMicroBlock(signer = committedGenerator1.some, finalizationVoting = fv.some)(TxHelpers.transfer(committedGenerator3))
      )

    val genesisBlock = d.lastBlock

    log.debug("Append block 2 - the first common block")
    val altChainBlock1 = d.createBlock(generator = committedGenerator2, strictTime = true)
    appendBlock()

    log.debug("Append block 3 (activation height)")
    // Created while altChainBlock1 is not in the state, so its VRF proof is computed from the wrong hit source and the fork fails on this block,
    // after altChainBlock1 has been applied
    val altChainBlock2 = d.createBlock(ref = altChainBlock1.id().some, generator = committedGenerator2, strictTime = true)
    appendBlock()

    log.debug("Append block 4")
    appendBlock()

    log.debug("Append block 5 with commitments")
    appendBlock(mkCommitments())

    log.debug("Period 1 with generators")
    log.debug("Append block 6")
    val endorsedBlockOfPeriod1 = appendBlock()

    log.debug("Append block 7 with commitments and conflicting endorsement")
    appendBlock(mkCommitments())
    appendMicroBlock(mkFinalizationVoting().withConflict(committedGenerator3, committedGenerator3Idx, endorsedBlockOfPeriod1.id()))

    log.debug("Period 2 without generators")
    log.debug("Append block 8")
    appendBlock()
    log.debug("Append block 9 with commitments")
    appendBlock(mkCommitments())

    log.debug("Period 3 with generators")
    log.debug("Append block 10 with conflicting endorsement")
    val endorsedBlockOfPeriod2 = appendBlock(mkCommitments(d.blockchain.generationPeriodOf(Height(13)).value))
    appendMicroBlock(mkFinalizationVoting().withConflict(committedGenerator2, committedGenerator2Idx, endorsedBlockOfPeriod2.id()))
    val lastBlockId = d.lastBlockId

    def getGenerators            = (1 to d.blockchain.height).map(i => d.generatorsApi.generators(Height(i)))
    val mainChainBlockGenerators = getGenerators

    log.debug("Try to append an extension with a wrong block")
    val channel = new EmbeddedChannel()
    val extensionAppender =
      ExtensionAppender(d.blockchain, d.utxPool, d.posSelector, d.testTime, InvalidBlockStorage.NoOp, PeerDatabase.NoOp, global)(channel, _)

    val altChain    = Seq(genesisBlock, altChainBlock1, altChainBlock2)
    val appendError = extensionAppender(ExtensionBlocks(d.blockchain.score + 1, altChain, Map.empty, channel)).runSyncUnsafe().left.value.toString
    appendError should (include(altChainBlock2.id().toString) and include("Could not verify VRF proof"))

    log.debug("Checks")
    withClue("Restored: ") {
      d.lastBlockId shouldBe lastBlockId
    }

    val mainChainBlockGeneratorsAfterRestore = getGenerators

    {
      given Prettifier = {
        case o: IndexedSeq[?] => o.mkString("\n")
        case o                => o.toString
      }

      mainChainBlockGeneratorsAfterRestore shouldBe mainChainBlockGenerators
    }
  }
}
