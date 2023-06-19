package com.wavesplatform.mining

import cats.data.EitherT
import com.wavesplatform.account.SeedKeyPair
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

  // TODO: NODE-2594 what if challenged block contains challengeHeader?

  def challengeBlock(block: Block, ch: Channel, correctStateHash: Option[ByteStr]): Task[Unit] = {
    log.debug(s"Challenging block $block")
    getChallengingAccount match {
      case Some(acc) =>
        (for {
          challengingBlock <- EitherT(
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
                Some(
                  ChallengedHeader(
                    block.header.featureVotes,
                    block.header.generator,
                    block.header.rewardVote,
                    block.header.stateHash,
                    block.signature
                  )
                )
              )
            )
          )
          _ <- EitherT(appendBlock(challengingBlock))
        } yield {
          log.debug(s"Successfully challenged $block with $challengingBlock")
          allChannels.broadcast(BlockForged(challengingBlock), Some(ch))
        }).fold(
          err => log.debug(s"Could not challenge $block: $err"),
          identity
        )
      case None => Task(log.debug(s"Could not challenge $block: no suitable account in wallet"))
    }
  }

  def challengeMicroblock(md: MicroblockData, ch: Channel, correctStateHash: Option[ByteStr]): Task[Unit] = {
    val idStr = md.invOpt.map(_.totalBlockId.toString).getOrElse(s"(sig=${md.microBlock.totalResBlockSig})")
    log.debug(s"Challenging microblock $idStr")
    (blockchainUpdater.lastBlockHeader.map(_.header), getChallengingAccount) match {
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
                Some(
                  ChallengedHeader(
                    block.header.featureVotes,
                    block.header.generator,
                    block.header.rewardVote,
                    md.microBlock.stateHash,
                    md.microBlock.signature
                  )
                )
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
      case _ => Task(log.debug(s"Could not challenge microblock $idStr: no suitable account in wallet"))
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

  private def getChallengingAccount: Option[SeedKeyPair] = {
    wallet.privateKeyAccounts
      .map { pk =>
        pk -> blockchainUpdater.generatingBalance(pk.toAddress)
      }
      .filter { case (_, balance) => blockchainUpdater.isMiningAllowed(blockchainUpdater.height, balance) }
      .maxByOption(_._2)
      .map(_._1)
  }
}

object BlockChallenger {
  val MaliciousMinerBanPeriod = 1000
}
