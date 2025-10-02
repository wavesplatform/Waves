package com.wavesplatform.state

import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.network.{ChannelGroupExt, EndorseBlock}
import com.wavesplatform.state.EndorsementStorage.EndorsementFilter
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.ChannelGroup

trait BlockEndorser {
  def endorse(endorsedHeight: Height): Unit
}

object BlockEndorser {
  object Disabled extends BlockEndorser {
    override def endorse(endorsedHeight: Height): Unit = {}
  }

  class InMemory(blockchain: Blockchain, wallet: Wallet, endorsementStorage: EndorsementStorage, allChannels: ChannelGroup) extends BlockEndorser {
    override def endorse(endorsedHeight: Height): Unit = if (blockchain.isFeatureActivated(BlockchainFeatures.DeterministicFinality))
      for {
        endorsedBlockHeader <- blockchain.blockHeader(endorsedHeight).toSeq
        finalizedHeight     <- blockchain.finalizedHeightAt(endorsedHeight).toSeq
        finalizedId         <- blockchain.blockId(finalizedHeight).toSeq

        endorsedId = endorsedBlockHeader.id()
        committed  = blockchain.committedGenerators(blockchain.generationPeriodOf(endorsedHeight))
        miner      = endorsedBlockHeader.header.generator.toAddress
        isMiner    = wallet.privateKeyAccount(miner).isRight
        filter     = EndorsementFilter(isMiner, finalizedId, finalizedHeight, endorsedId, committed.map { case (_, blsPk) => blsPk })
        if endorsementStorage.startVoting(filter)

        (account, idx) <- for {
          ((committedAddr, _), idx) <- committed.zipWithIndex
          if committedAddr != miner // A miner doesn’t need to endorse its own blocks - a mining is already an endorsement
          pk <- wallet.privateKeyAccount(committedAddr).toSeq
        } yield (pk, idx)

        endorsement = BlockEndorsement.full(BlsKeyPair(account.privateKey), idx, finalizedId, finalizedHeight, endorsedId)
        networkMsg  = EndorseBlock.from(endorsement)
        if endorsementStorage.tryAddVote(networkMsg)
      } allChannels.broadcast(networkMsg)
  }
}
