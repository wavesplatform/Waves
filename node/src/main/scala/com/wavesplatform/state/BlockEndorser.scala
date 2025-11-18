package com.wavesplatform.state

import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.network.{ChannelGroupExt, EndorseBlock}
import com.wavesplatform.state.EndorsementFilter
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.ChannelGroup

trait BlockEndorser {

  /** Voting happens
    *   for block on endorserHeight
    *   with finalizedBlock at votingHeight
    *   by generators, committed on votingHeight
    */
  def vote(): Unit
}

object BlockEndorser {
  object Disabled extends BlockEndorser {
    override def vote(): Unit = {}
  }

  class InMemory(blockchain: Blockchain, wallet: Wallet, endorsementStorage: EndorsementStorage, allChannels: ChannelGroup) extends BlockEndorser {
    override def vote(): Unit = {
      val votingHeight   = Height(blockchain.height)
      val endorsedHeight = Height(votingHeight - 1)
      if (endorsedHeight > GenesisBlockHeight) for {
        votingPeriod <- blockchain.generationPeriodOf(votingHeight).toSeq

        votingBlockHeader   <- blockchain.blockHeader(votingHeight).toSeq
        endorsedBlockHeader <- blockchain.blockHeader(endorsedHeight).toSeq

        finalizedHeight = blockchain.finalizedHeightAtOrFallback(votingHeight)
        finalizedId <- blockchain
          .blockId(finalizedHeight)
          .toSeq

        endorsedId = endorsedBlockHeader.id()

        committed        = blockchain.committedGenerators(votingPeriod)
        votingBlockMiner = votingBlockHeader.header.generator.toAddress
        filter = {
          val isMiner  = wallet.privateKeyAccount(votingBlockMiner).isRight
          val balances = blockchain.currentGeneratorBalances()
          require(committed.size == balances.size, s"committed.size=${committed.size} == balances.size=${balances.size}")

          val minerIndex = if (isMiner) committed.indexWhere { case (addr, _) => addr == votingBlockMiner } else -1
          val endorsers = committed
            .zip(balances)
            .map { case ((addr1, blsPk), (addr2, balance)) =>
              require(addr1 == addr2, s"addr1=$addr1 == addr2=$addr2")
              blsPk -> balance
            }
            .to(Vector)

          val conflict = blockchain.conflictGenerators(votingPeriod).upTo(votingHeight)
          EndorsementFilter(GeneratorIndex.checked(minerIndex), finalizedId, finalizedHeight, endorsedId, endorsers, conflict)
        }
        if endorsementStorage.startVoting(filter)

        (account, idx) <- for {
          ((committedAddr, _), idx) <- committed.zipWithIndex
          if committedAddr != votingBlockMiner // A miner doesn’t need to endorse its own blocks - a mining is already an endorsement
          pk <- wallet.privateKeyAccount(committedAddr).toSeq
        } yield (pk, GeneratorIndex(idx))

        endorsement = BlockEndorsement.signed(BlsKeyPair(account.privateKey), idx, finalizedId, finalizedHeight, endorsedId)
        networkMsg  = EndorseBlock.from(endorsement)
        broadcast <- endorsementStorage.tryAdd(networkMsg).toSeq
        if broadcast
      } allChannels.broadcast(networkMsg)
    }
  }
}
