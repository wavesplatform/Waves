package com.wavesplatform.state

import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.network.{ChannelGroupExt, EndorseBlock}
import com.wavesplatform.state.EndorsementStorage.EndorsementFilter
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.ChannelGroup

trait BlockEndorser {

  /** Voting happens
    *   for block on endorserHeight
    *   with finalizedBlock at votingHeight
    *   by generators, committed on votingHeight
    */
  def vote(votingHeight: Height): Unit
}

object BlockEndorser {
  object Disabled extends BlockEndorser {
    override def vote(votingHeight: Height): Unit = {}
  }

  class InMemory(blockchain: Blockchain, wallet: Wallet, endorsementStorage: EndorsementStorage, allChannels: ChannelGroup) extends BlockEndorser {
    override def vote(votingHeight: Height): Unit = {
      val endorsedHeight = Height(votingHeight - 1)
      if (endorsedHeight > GenesisBlockHeight && blockchain.isFeatureActivated(BlockchainFeatures.DeterministicFinality))
        for {
          votingBlockHeader <- blockchain
            .blockHeader(votingHeight)
            .toSeq

          endorsedBlockHeader <- blockchain
            .blockHeader(endorsedHeight)
            .toSeq

          finalizedHeight = blockchain.finalizedHeightAtOrFallback(votingHeight)
          finalizedId <- blockchain
            .blockId(finalizedHeight)
            .toSeq

          endorsedId       = endorsedBlockHeader.id()
          committed        = blockchain.committedGenerators(blockchain.generationPeriodOf(votingHeight))
          votingBlockMiner = votingBlockHeader.header.generator.toAddress
          isMiner          = wallet.privateKeyAccount(votingBlockMiner).isRight
          filter           = EndorsementFilter(isMiner, finalizedId, finalizedHeight, endorsedId, committed.map { case (_, blsPk) => blsPk })
          if endorsementStorage.startVoting(filter)

          (account, idx) <- for {
            ((committedAddr, _), idx) <- committed.zipWithIndex
            if committedAddr != votingBlockMiner // A miner doesn’t need to endorse its own blocks - a mining is already an endorsement
            pk <- wallet.privateKeyAccount(committedAddr).toSeq
          } yield (pk, idx)

          endorsement = BlockEndorsement.full(BlsKeyPair(account.privateKey), idx, finalizedId, finalizedHeight, endorsedId)
          networkMsg  = EndorseBlock.from(endorsement)
          broadcast <- endorsementStorage.tryAddVote(networkMsg).toSeq
          if broadcast
        } allChannels.broadcast(networkMsg)
    }
  }
}
