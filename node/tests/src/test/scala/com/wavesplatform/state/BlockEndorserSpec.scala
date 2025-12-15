package com.wavesplatform.state

import cats.syntax.either.*
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, FinalizationVoting}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.network.EndorseBlock
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{FreeSpec, WithResourceManager}
import com.wavesplatform.transaction.TxHelpers
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import org.scalactic.source.Position

class BlockEndorserSpec extends FreeSpec, WithDomain, WithResourceManager {
  private val defaultSettings = DomainPresets.DeterministicFinality
    .copy(synchronizationSettings = DomainPresets.DeterministicFinality.synchronizationSettings.copy(maxRollback = 2))
    .configure(
      _.copy(
        generationPeriodLength = 2,
        lightNodeBlockFieldsAbsenceInterval = 0
      )
    )

  "vote" - {
    "starts voting with increased height" in withManager { manager =>
      val generator1 = TxHelpers.signer(0)
      val generator2 = TxHelpers.signer(1)
      val generators = Seq(generator1, generator2)

      var actualFilter = Option.empty[EndorsementFilter]
      withDomain(defaultSettings, AddrWithBalance.enoughBalances(generator1, generator2)) { d =>
        val endorsementStorage = new EndorsementStorage {
          override def tryAdd(msg: EndorseBlock): Either[String, Boolean] = false.asRight
          override def startVoting(filter: EndorsementFilter): Boolean = {
            actualFilter = Some(filter)
            true
          }
          override def tryCollectAndClear(endorsedId: BlockId): Option[FinalizationVoting] = None
        }

        val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
        val endorser = new BlockEndorser.InMemory(d.blockchain, d.wallet, endorsementStorage, channels)

        log.debug(s"Append block 2 with commitments")
        val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
        val block2WithCommitments = d.createBlock(version = Block.ProtoBlockVersion, txs = txs, generator = generator1, strictTime = true)
        d.appender.appendBlock(block2WithCommitments)

        log.debug(s"Append blocks 3 and 4 of new period")
        (3 to 4).foreach { _ =>
          val block = d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = generator1, strictTime = true)
          d.appender.appendBlock(block)
        }

        endorser.vote(d.blockchain.currentGeneratorBalances.getOrElse(Seq.empty))
        actualFilter.value.finalizedHeight shouldBe Height(2) // 4 - maxRollback
      }
    }
  }
}
