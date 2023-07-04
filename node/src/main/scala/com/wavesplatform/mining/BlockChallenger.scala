package com.wavesplatform.mining

import cats.data.EitherT
import cats.syntax.either.*
import cats.syntax.traverse.*
import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.block.{Block, ChallengedHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.*
import com.wavesplatform.network.MicroBlockSynchronizer.MicroblockData
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.appender.MaxTimeDrift
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.state.{Blockchain, TxStateSnapshotHashBuilder}
import com.wavesplatform.transaction.{BlockchainUpdater, Transaction}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.wallet.Wallet
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import monix.eval.Task

import scala.concurrent.duration.*

class BlockChallenger(
    blockchainUpdater: BlockchainUpdater & Blockchain,
    allChannels: ChannelGroup,
    wallet: Wallet,
    settings: WavesSettings,
    timeService: Time,
    pos: PoSSelector,
    appendBlock: Block => Task[Either[ValidationError, Option[BigInt]]]
) extends ScorexLogging {

  def challengeBlock(block: Block, ch: Channel, prevStateHash: ByteStr, diffHashes: Seq[ByteStr]): Task[Unit] = {
    log.debug(s"Challenging block $block")
    (for {
      challengingBlock <- EitherT(
        createChallengingBlock(block, block.header.stateHash, block.signature, block.transactionData, prevStateHash, diffHashes)
      )
      _ <- EitherT(appendBlock(challengingBlock))
    } yield {
      log.debug(s"Successfully challenged $block with $challengingBlock")
      allChannels.broadcast(BlockForged(challengingBlock), Some(ch))
    }).fold(
      err => log.debug(s"Could not challenge $block: $err"),
      identity
    )
  }

  def challengeMicroblock(md: MicroblockData, ch: Channel, prevStateHash: ByteStr, diffHashes: Seq[ByteStr]): Task[Unit] = {
    val idStr = md.invOpt.map(_.totalBlockId.toString).getOrElse(s"(sig=${md.microBlock.totalResBlockSig})")
    log.debug(s"Challenging microblock $idStr")

    (for {
      discarded <- EitherT(Task(blockchainUpdater.removeAfter(blockchainUpdater.lastBlockHeader.get.header.reference)))
      block     <- EitherT(Task(discarded.headOption.map(_._1).toRight(GenericError("Liquid block wasn't discarded"))))
      challengingBlock <- EitherT(
        createChallengingBlock(
          block,
          md.microBlock.stateHash,
          md.microBlock.totalResBlockSig,
          block.transactionData ++ md.microBlock.transactionData,
          prevStateHash,
          diffHashes
        )
      )
      _ <- EitherT(appendBlock(challengingBlock))
    } yield {
      log.debug(s"Successfully challenged microblock $idStr with $challengingBlock")
      allChannels.broadcast(BlockForged(challengingBlock), Some(ch))
    }).fold(
      err => log.debug(s"Could not challenge microblock $idStr: $err"),
      identity
    )
  }

  private def createChallengingBlock(
      challengedBlock: Block,
      challengedStateHash: Option[ByteStr],
      challengedSignature: ByteStr,
      txs: Seq[Transaction],
      prevStateHash: ByteStr,
      diffHashes: Seq[ByteStr]
  ): Task[Either[ValidationError, Block]] = Task {
    val lastBlockHeader = blockchainUpdater.lastBlockHeader.get.header

    for {
      (acc, delay) <- getChallengingAccount(challengedBlock.sender.toAddress)
      blockTime = lastBlockHeader.timestamp + delay
      consensusData <-
        pos.consensusData(
          acc,
          blockchainUpdater.height,
          blockchainUpdater.settings.genesisSettings.averageBlockDelay,
          lastBlockHeader.baseTarget,
          lastBlockHeader.timestamp,
          blockchainUpdater.parentHeader(lastBlockHeader, 2).map(_.timestamp),
          blockTime
        )

      initialBlockDiff <- BlockDiffer.createInitialBlockDiff(blockchainUpdater, acc.toAddress).leftMap(GenericError(_))
      challengingBlock <-
        Block.buildAndSign(
          challengedBlock.header.version,
          blockTime,
          challengedBlock.header.reference,
          consensusData.baseTarget,
          consensusData.generationSignature,
          txs,
          acc,
          blockFeatures(blockchainUpdater, settings),
          blockRewardVote(settings),
          Some(
            TxStateSnapshotHashBuilder.createBlockStateHash(
              prevStateHash,
              TxStateSnapshotHashBuilder.createHashFromDiff(blockchainUpdater, initialBlockDiff).txStateSnapshotHash +: diffHashes.tail
            )
          ),
          Some(
            ChallengedHeader(
              challengedBlock.header.timestamp,
              challengedBlock.header.baseTarget,
              challengedBlock.header.generationSignature,
              challengedBlock.header.featureVotes,
              challengedBlock.header.generator,
              challengedBlock.header.rewardVote,
              challengedStateHash,
              challengedSignature
            )
          )
        )
    } yield {
      challengingBlock
    }
  }.flatMap {
    case res @ Right(block) => waitForTimeAlign(block.header.timestamp).map(_ => res)
    case err @ Left(_)      => Task(err)
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

  private def getChallengingAccount(challengedMiner: Address): Either[ValidationError, (SeedKeyPair, Long)] =
    wallet.privateKeyAccounts
      .map { pk =>
        pk -> blockchainUpdater.generatingBalance(pk.toAddress)
      }
      .filter { case (_, balance) => blockchainUpdater.isMiningAllowed(blockchainUpdater.height, balance) }
      .traverse { case (acc, initGenBalance) =>
        pos
          .getValidBlockDelay(
            blockchainUpdater.height,
            acc,
            blockchainUpdater.lastBlockHeader.get.header.baseTarget,
            initGenBalance + blockchainUpdater.generatingBalance(challengedMiner)
          )
          .map((acc, _))
      }
      .map(_.minByOption(_._2))
      .flatMap(_.toRight(GenericError("No suitable account in wallet")))

  private def waitForTimeAlign(blockTime: Long): Task[Unit] =
    Task {
      val currentTime = timeService.correctedTime()
      blockTime - currentTime - MaxTimeDrift
    }.flatMap { timeDiff =>
      if (timeDiff > 0) {
        Task.sleep(timeDiff.millis)
      } else Task.unit
    }
}
