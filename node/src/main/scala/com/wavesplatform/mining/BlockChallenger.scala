package com.wavesplatform.mining

import cats.data.EitherT
import com.wavesplatform.block.{Block, ChallengedHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.*
import com.wavesplatform.network.MicroBlockSynchronizer.MicroblockData
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.BlockchainUpdater
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.wallet.Wallet
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import monix.eval.Task

class BlockChallenger(
    blockchainUpdater: BlockchainUpdater & Blockchain,
    allChannels: ChannelGroup,
    wallet: Wallet,
    settings: WavesSettings,
    appendBlock: Block => Task[Either[ValidationError, Option[BigInt]]]
) extends ScorexLogging {

  def challengeBlock(block: Block, ch: Channel, correctStateHash: Option[ByteStr]): Task[Unit] = {
    log.debug(s"Challenging block $block")
    // TODO: challenging account selection
    wallet.privateKeyAccounts.headOption match {
      case Some(acc) =>
        (for {
          challengeBlock <- EitherT(
            Task(
              Block.buildAndSign(
                block.header.version,
                block.header.timestamp,
                block.header.reference,
                block.header.baseTarget,
                block.header.generationSignature,
                block.transactionData,
                acc,
                blockFeatures(blockchainUpdater, settings),
                blockRewardVote(settings),
                correctStateHash,
                Some(ChallengedHeader(block.header.featureVotes, block.header.generator, block.header.rewardVote, block.signature))
              )
            )
          )
          _ <- EitherT(appendBlock(challengeBlock))
        } yield {
          log.debug(s"Successfully challenged $block with $challengeBlock")
          allChannels.broadcast(BlockForged(challengeBlock), Some(ch))
        }).fold(
          err => log.debug(s"Could not challenge $block: $err"),
          identity
        )
      case None => Task.unit
    }
  }

  def challengeMicroblock(md: MicroblockData, ch: Channel, correctStateHash: Option[ByteStr]): Task[Unit] = {
    val idStr = md.invOpt.map(_.totalBlockId.toString).getOrElse(s"(sig=${md.microBlock.totalResBlockSig})")
    log.debug(s"Challenging microblock $idStr")
    (blockchainUpdater.lastBlockHeader.map(_.header), wallet.privateKeyAccounts.headOption) match {
      case (Some(blockHeader), Some(acc)) =>
        (for {
          discarded <- EitherT(Task(blockchainUpdater.removeAfter(blockHeader.reference)))
          block     <- EitherT(Task(discarded.headOption.map(_._1).toRight(GenericError("Liquid block wasn't discarded"))))
          challengeBlock <- EitherT(
            Task(
              Block.buildAndSign(
                block.header.version,
                block.header.timestamp,
                block.header.reference,
                block.header.baseTarget,
                block.header.generationSignature,
                block.transactionData ++ md.microBlock.transactionData,
                acc,
                blockFeatures(blockchainUpdater, settings),
                blockRewardVote(settings),
                correctStateHash,
                Some(ChallengedHeader(block.header.featureVotes, block.header.generator, block.header.rewardVote, md.microBlock.signature))
              )
            )
          )
          _ <- EitherT(appendBlock(challengeBlock))
        } yield {
          log.debug(s"Successfully challenged microblock $idStr with $challengeBlock")
          allChannels.broadcast(BlockForged(challengeBlock), Some(ch))
        }).fold(
          err => log.debug(s"Could not challenge microblock $idStr: $err"),
          identity
        )
      case _ => Task.unit
    }
  }

  private def blockFeatures(blockchain: Blockchain, settings: WavesSettings): Seq[Short] = {
    val exclude = blockchain.approvedFeatures.keySet ++ settings.blockchainSettings.functionalitySettings.preActivatedFeatures.keySet

    settings.featuresSettings.supported
      .filterNot(exclude)
      .filter(BlockchainFeatures.implemented)
      .sorted
  }

  private def blockRewardVote(settings: WavesSettings): Long =
    settings.rewardsSettings.desired.getOrElse(-1L)
}
