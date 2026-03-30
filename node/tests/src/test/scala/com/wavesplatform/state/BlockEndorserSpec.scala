package com.wavesplatform.state

import cats.syntax.either.*
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.FinalizationVoting
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.network.{EndorseBlock, MessageCodec, PeerDatabase}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{FreeSpec, NumericExt, WithResourceManager}
import com.wavesplatform.transaction.{CommitToGenerationTransaction, TxHelpers}
import com.wavesplatform.utils.EmbeddedChannelOps
import com.wavesplatform.wallet.Wallet
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor

class BlockEndorserSpec extends FreeSpec, WithDomain, WithResourceManager, EmbeddedChannelOps {
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
        val endorser =
          new BlockEndorser.InMemory(d.settings.synchronizationSettings.maxRollback, d.blockchain, d.wallet, endorsementStorage, channels)

        log.debug("Append block 2 with commitments")
        val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
        val block2WithCommitments = d.createBlock(txs, generator = generator1, strictTime = true)
        d.appender.appendBlock(block2WithCommitments)

        log.debug("Append blocks 3 and 4 of new period")
        (3 to 4).foreach { _ =>
          val block = d.createBlock(generator = generator1, strictTime = true)
          d.appender.appendBlock(block)
        }

        endorser.vote(d.blockchain.currentGeneratorSet.getOrElse(Seq.empty))
        actualFilter.value.finalizedHeight shouldBe Height(2) // 4 - maxRollback
      }
    }

    "don't broadcast" - {
      "if not enough generating balance" in withManager { manager =>
        val generator1         = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 0)
        val generator2         = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 1)
        val otherNodeGenerator = TxHelpers.signer(0)
        val generators         = Seq(generator1, generator2, otherNodeGenerator)
        val generator2Index    = 1

        withDomain(defaultSettings, AddrWithBalance.enoughBalances(generators*)) { d =>
          d.wallet.generateNewAccounts(2)

          val endorsementStorage = new EndorsementStorage {
            override def tryAdd(msg: EndorseBlock): Either[String, Boolean]                  = true.asRight
            override def startVoting(filter: EndorsementFilter): Boolean                     = true
            override def tryCollectAndClear(endorsedId: BlockId): Option[FinalizationVoting] = None
          }

          val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
          val channel1 = manager(new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp)))
          channels.add(channel1)
          val endorser =
            new BlockEndorser.InMemory(d.settings.synchronizationSettings.maxRollback, d.blockchain, d.wallet, endorsementStorage, channels)

          log.debug("Append block 2 with commitments")
          val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
          val block2WithCommitments = d.createBlock(txs, generator = generator1, strictTime = true)
          d.appender.appendBlock(block2WithCommitments)

          log.debug("Append block 3 of new period with spending all WAVES by generator2")
          d.appender.appendBlock(
            d.createBlock(
              txs = Seq(
                TxHelpers.transfer(
                  from = generator2,
                  to = generator1.toAddress,
                  amount = d.blockchain.balance(generator2.toAddress) - CommitToGenerationTransaction.DepositInWavelets - 1.waves,
                  fee = 1.waves
                )
              ),
              generator = generator1,
              strictTime = true
            )
          )

          log.debug("Append block 4")
          d.appender.appendBlock(d.createBlock(generator = otherNodeGenerator, strictTime = true))

          endorser.vote(d.blockchain.currentGeneratorSet.getOrElse(Nil))
          val xs = channel1.sentEndorsements
          xs should not be empty
          withClue("generator2 didn't endorse: ") {
            xs.count(_.endorserIndex == generator2Index) shouldBe 0
          }
        }
      }
    }
  }
}
