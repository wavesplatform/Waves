package com.wavesplatform.state

import com.typesafe.scalalogging.StrictLogging
import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.network.{ChannelGroupExt, EndorseBlock}
import com.wavesplatform.state.EndorsementFilter
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.ChannelGroup

trait BlockEndorser {

  /** Voting happens
    *   for block at endorserHeight
    *   with finalizedBlock at votingHeight
    *   by generators, committed at votingHeight
    */
  def vote(generatorBalances: GeneratorBalances): Unit
}

object BlockEndorser {
  object Disabled extends BlockEndorser {
    override def vote(generatorBalances: GeneratorBalances): Unit = {}
  }

  class InMemory(blockchain: Blockchain, wallet: Wallet, endorsementStorage: EndorsementStorage, allChannels: ChannelGroup)
      extends BlockEndorser,
        StrictLogging {
    override def vote(generatorBalances: GeneratorBalances): Unit = {
      val votingHeight   = Height(blockchain.height)
      val endorsedHeight = votingHeight - 1
      if (endorsedHeight > GenesisBlockHeight) for {
        votingPeriod <- blockchain.generationPeriodOf(votingHeight).toSeq

        votingBlockHeader   <- blockchain.blockHeader(votingHeight.toInt).toSeq
        endorsedBlockHeader <- blockchain.blockHeader(endorsedHeight.toInt).toSeq

        finalizedHeight = blockchain.finalizedHeightAtOrFallback(votingHeight.toInt)
        finalizedId <- blockchain
          .blockId(finalizedHeight.toInt)
          .toSeq

        endorsedId = endorsedBlockHeader.id()

        committed        = blockchain.committedGenerators(votingPeriod)
        votingBlockMiner = votingBlockHeader.header.generator.toAddress
        filter = {
          val isMiner    = wallet.privateKeyAccount(votingBlockMiner).isRight
          val balances   = generatorBalances.map(x => x.address -> x.balance).toMap
          val minerIndex = if (isMiner) committed.indexWhere { case (addr, _) => addr == votingBlockMiner } else -1
          val endorsers = committed.map { case (address, blsPk) =>
            (address, blsPk, balances.getOrElse(address, 0L))
          }.toVector

          val conflict = blockchain.conflictGenerators(votingPeriod).upTo(votingHeight)
          EndorsementFilter(GeneratorIndex.checked(minerIndex), finalizedId, finalizedHeight, endorsedId, endorsers, conflict)
        }
        if endorsementStorage.startVoting(filter)

        (account, idx) <- for {
          ((committedAddr, _), idx) <- committed.zipWithIndex
          if !filter.miner.contains(idx) // A miner doesn’t need to endorse its own blocks - a mining is already an endorsement
          pk <- wallet.privateKeyAccount(committedAddr).toSeq
        } yield (pk, GeneratorIndex(idx))
        _ = logger.debug(s"Found ${account.toAddress} in generator set") // TODO: remove from prod

        endorsement = BlockEndorsement.signed(BlsKeyPair(account.privateKey), idx, finalizedId, finalizedHeight, endorsedId)
        networkMsg  = EndorseBlock.from(endorsement)
        broadcast <- endorsementStorage.tryAdd(networkMsg).toSeq
        _ = logger.debug(s"Will ${if (broadcast) "" else "not "}broadcast endorsement from ${account.toAddress}") // TODO: remove from prod
        if broadcast
      } allChannels.broadcast(networkMsg)
    }
  }
}
