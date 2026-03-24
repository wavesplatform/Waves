package com.wavesplatform.state

import com.typesafe.scalalogging.StrictLogging
import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.network.{ChannelGroupExt, EndorseBlock}
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.ChannelGroup

trait BlockEndorser {

  /** Voting happens
    *   for block at endorserHeight
    *   with finalizedBlock at votingHeight
    *   by generators, committed at votingHeight
    */
  def vote(generatorSet: GeneratorSet): Unit
}

object BlockEndorser {
  object Disabled extends BlockEndorser {
    override def vote(generatorSet: GeneratorSet): Unit = {}
  }

  class InMemory(
      maxSyncRollbackLength: Int,
      blockchain: Blockchain,
      wallet: Wallet,
      endorsementStorage: EndorsementStorage,
      allChannels: ChannelGroup
  ) extends BlockEndorser,
        StrictLogging {
    override def vote(generatorSet: GeneratorSet): Unit = {
      val votingHeight   = Height(blockchain.height)
      val endorsedHeight = votingHeight - 1
      if (endorsedHeight > GenesisBlockHeight) for {
        votingPeriod <- blockchain.generationPeriodOf(votingHeight).toSeq

        votingBlockHeader   <- blockchain.blockHeader(votingHeight.toInt).toSeq
        endorsedBlockHeader <- blockchain.blockHeader(endorsedHeight.toInt).toSeq

        finalizedHeight = blockchain.finalizedHeightOrFallback(maxSyncRollbackLength)
        if endorsedHeight > finalizedHeight

        finalizedId <- blockchain
          .blockId(finalizedHeight.toInt)
          .toSeq

        endorsedId = endorsedBlockHeader.id()

        committed        = blockchain.committedGenerators(votingPeriod)
        votingBlockMiner = votingBlockHeader.header.generator.toAddress
        minerIndex       = committed.indexWhere { case (addr, _) => addr == votingBlockMiner }
        if minerIndex >= 0 // -1 means no miner among committed, impossible

        balances = generatorSet.collect {
          case x if blockchain.isGeneratingBalanceValid(votingHeight, votingBlockHeader.header, x.balance) => x.address -> x.balance
        }.toMap

        filter = {
          val normalizedEndorsers = committed.map { case (address, blsPk) =>
            (address, blsPk, balances.getOrElse(address, 0L))
          }.toVector

          val conflict = blockchain.conflictGenerators(votingPeriod).upTo(votingHeight)
          EndorsementFilter(
            blockchain.settings.functionalitySettings.maxValidEndorsers,
            GeneratorIndex(minerIndex),
            isMiner = wallet.privateKeyAccount(votingBlockMiner).isRight,
            finalizedId,
            finalizedHeight,
            endorsedId,
            normalizedEndorsers,
            conflict
          )
        }
        if endorsementStorage.startVoting(filter)

        (account, idx) <- for {
          ((committedAddr, _), idx) <- committed.zipWithIndex
          if idx != filter.miner.toInt // A miner doesn’t need to endorse its own blocks - a mining is already an endorsement
          pk <- wallet.privateKeyAccount(committedAddr).toSeq
          if balances.contains(committedAddr)
        } yield (pk, GeneratorIndex(idx))

        endorsement = BlockEndorsement.signed(BlsKeyPair(account.privateKey), idx, finalizedId, finalizedHeight, endorsedId)
        networkMsg  = EndorseBlock.from(endorsement)
        broadcast <- endorsementStorage.tryAdd(networkMsg) match {
          case Right(r) => Some(r)
          case Left(err) =>
            logger.warn(s"Can't add endorsement from #$idx ${account.toAddress}: $err")
            None
        }
        if broadcast
      } allChannels.broadcast(networkMsg)
    }
  }
}
