package com.wavesplatform.finalization

import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.*
import com.wavesplatform.state.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{FreeSpec, TestSchedulerOps}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.wallet.Wallet
import org.scalatest.time.SpanSugar.convertLongToGrainOfTime

class ChallengingAfterFinalizationSuite extends BaseFinalizationSpec, TestSchedulerOps {
  private val thisNodeAcc  = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 0)
  private val otherNodeAcc = TxHelpers.defaultSigner

  private val baseSettings = DomainPresets.DeterministicFinality.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  private val defaultSettings = baseSettings
    .copy(minerSettings = baseSettings.minerSettings.copy(quorum = 0, microBlockInterval = 100.millis))
    .configure(_.copy(generationPeriodLength = 2))

  "Anyone can challenge" in withDomain(
    defaultSettings,
    AddrWithBalance.enoughBalances(otherNodeAcc) // thisNodeAcc has no WAVES
  ) { d =>
    d.wallet.generateNewAccounts(1)

    log.debug("Append block2")
    d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, strictTime = true, generator = otherNodeAcc))
    d.appendMicroBlock(TxHelpers.commitToGeneration(Height(3), sender = otherNodeAcc))

    log.debug("Append block3 with invalid state hash and challenge")
    val invalidStateHash = ByteStr.fill(DigestLength)(1)
    val invalidBlock = d.createBlock(
      Block.ProtoBlockVersion,
      txs = Nil,
      strictTime = true,
      generator = otherNodeAcc,
      stateHash = Some(Some(invalidStateHash)),
      timestamp = Some(d.nextBlockTime(otherNodeAcc) + 1L) // HACK: challenger block timestamp will be better
    )
    d.appender.appendBlock(invalidBlock, requireAppended = false)

    withClue("Challenged: ") {
      d.blockchain.height shouldBe 3
      d.lastBlockId should not be invalidBlock.id()
      d.lastBlock.header.generator.toAddress shouldBe thisNodeAcc.toAddress
      d.lastBlock.header.challengedHeader should not be empty
    }

    withClue("Empty finalization header: ") {
      d.lastBlock.header.finalizationVoting shouldBe empty
    }
  }
}
