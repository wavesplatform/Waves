package com.wavesplatform.mining

import cats.data.EitherT
import cats.syntax.traverse.*
import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.block.{Block, ChallengedHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.BlockStats
import com.wavesplatform.network.*
import com.wavesplatform.network.MicroBlockSynchronizer.MicroblockData
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult.Applied
import com.wavesplatform.state.appender.MaxTimeDrift
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.state.reader.SnapshotBlockchain
import com.wavesplatform.state.{Blockchain, StateSnapshot, TxStateSnapshotHashBuilder}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.{BlockchainUpdater, Transaction}
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.wallet.Wallet
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import monix.eval.Task

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

trait BlockChallenger {
  def challengeBlock(block: Block, ch: Channel): Task[Unit]
  def challengeMicroblock(md: MicroblockData, ch: Channel): Task[Unit]
  def pickBestAccount(accounts: Seq[(SeedKeyPair, Long)]): Either[GenericError, (SeedKeyPair, Long)]
  def getChallengingAccounts(challengedMiner: Address): Either[ValidationError, Seq[(SeedKeyPair, Long)]]
  def getProcessingTx(id: ByteStr): Option[Transaction]
  def allProcessingTxs: Seq[Transaction]
}

class BlockChallengerImpl(
    blockchainUpdater: BlockchainUpdater & Blockchain,
    allChannels: ChannelGroup,
    wallet: Wallet,
    settings: WavesSettings,
    timeService: Time,
    pos: PoSSelector,
    appendBlock: Block => Task[Either[ValidationError, BlockApplyResult]],
    timeDrift: Long = MaxTimeDrift
) extends BlockChallenger
    with ScorexLogging {

  private val processingTxs: ConcurrentHashMap[ByteStr, Transaction] = new ConcurrentHashMap()

  def challengeBlock(block: Block, ch: Channel): Task[Unit] = {
    println(s"Challenging block $block")

    withProcessingTxs(block.transactionData) {
      (for {
        challengingBlock <- EitherT(
          createChallengingBlock(
            block,
            block.header.stateHash,
            block.signature,
            block.transactionData,
            blockchainUpdater.lastStateHash(Some(block.header.reference))
          )
        )
        applyResult <- EitherT(appendBlock(challengingBlock))
      } yield applyResult -> challengingBlock).value
    }.map {
      case Right((Applied(_, _), challengingBlock)) =>
        println(s"Successfully challenged $block with $challengingBlock")
        BlockStats.challenged(challengingBlock, blockchainUpdater.height)
        if (blockchainUpdater.isLastBlockId(challengingBlock.id())) {
          allChannels.broadcast(BlockForged(challengingBlock), Some(ch))
        }
      case Right((_, challengingBlock)) => println(s"Ignored challenging block $challengingBlock")
      case Left(err)                    => println(s"Could not challenge $block: $err")
    }
  }

  def challengeMicroblock(md: MicroblockData, ch: Channel): Task[Unit] = {
    val idStr = md.invOpt.map(_.totalBlockId.toString).getOrElse(s"(sig=${md.microBlock.totalResBlockSig})")
    println(s"Challenging microblock $idStr")

    (for {
      discarded <- EitherT(Task(blockchainUpdater.removeAfter(blockchainUpdater.lastBlockHeader.get.header.reference)))
      block     <- EitherT(Task(discarded.headOption.map(_._1).toRight(GenericError("Liquid block wasn't discarded"))))
      txs = block.transactionData ++ md.microBlock.transactionData
      (applyResult, challengingBlock) <- EitherT(withProcessingTxs(txs) {
        (for {
          challengingBlock <- EitherT(
            createChallengingBlock(
              block,
              md.microBlock.stateHash,
              md.microBlock.totalResBlockSig,
              txs,
              blockchainUpdater.lastStateHash(Some(block.header.reference))
            )
          )
          applyResult <- EitherT(appendBlock(challengingBlock))
        } yield applyResult -> challengingBlock).value
      })
    } yield {
      applyResult match {
        case Applied(_, _) =>
          println(s"Successfully challenged microblock $idStr with $challengingBlock")
          BlockStats.challenged(challengingBlock, blockchainUpdater.height)
          if (blockchainUpdater.isLastBlockId(challengingBlock.id())) {
            allChannels.broadcast(BlockForged(challengingBlock), Some(ch))
          }
        case _ =>
          println(s"Ignored challenging block $challengingBlock")
      }
    }).fold(
      err => println(s"Could not challenge microblock $idStr: $err"),
      identity
    )
  }

  def pickBestAccount(accounts: Seq[(SeedKeyPair, Long)]): Either[GenericError, (SeedKeyPair, Long)] =
    accounts.minByOption(_._2).toRight(GenericError("No suitable account in wallet"))

  def getChallengingAccounts(challengedMiner: Address): Either[ValidationError, Seq[(SeedKeyPair, Long)]] =
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

  def getProcessingTx(id: ByteStr): Option[Transaction] = Option(processingTxs.get(id))

  def allProcessingTxs: Seq[Transaction] = processingTxs.values.asScala.toSeq

  private def withProcessingTxs[A](txs: Seq[Transaction])(body: Task[A]): Task[A] =
    Task(processingTxs.putAll(txs.map(tx => tx.id() -> tx).toMap.asJava))
      .bracket(_ => body)(_ => Task(processingTxs.clear()))

  private def createChallengingBlock(
      challengedBlock: Block,
      challengedStateHash: Option[ByteStr],
      challengedSignature: ByteStr,
      txs: Seq[Transaction],
      prevStateHash: ByteStr
  ): Task[Either[ValidationError, Block]] = Task {
    val prevBlockHeader = blockchainUpdater
      .heightOf(challengedBlock.header.reference)
      .flatMap(blockchainUpdater.blockHeader)
      .map(_.header)
      .getOrElse(blockchainUpdater.lastBlockHeader.get.header)

    for {
      allAccounts  <- getChallengingAccounts(challengedBlock.sender.toAddress)
      (acc, delay) <- pickBestAccount(allAccounts)
      blockTime = prevBlockHeader.timestamp + delay
      _ <- Either.cond(
        blockTime < challengedBlock.header.timestamp,
        (),
        GenericError(s"Challenging block timestamp ($blockTime) is not better than challenged block timestamp (${challengedBlock.header.timestamp})")
      )
      consensusData <-
        pos.consensusData(
          acc,
          blockchainUpdater.height,
          blockchainUpdater.settings.genesisSettings.averageBlockDelay,
          prevBlockHeader.baseTarget,
          prevBlockHeader.timestamp,
          blockchainUpdater.parentHeader(prevBlockHeader, 2).map(_.timestamp),
          blockTime
        )
      blockWithoutChallengeAndStateHash <- Block.buildAndSign(
        challengedBlock.header.version,
        blockTime,
        challengedBlock.header.reference,
        consensusData.baseTarget,
        consensusData.generationSignature,
        txs,
        acc,
        blockFeatures(blockchainUpdater, settings),
        blockRewardVote(settings),
        None,
        None
      )
      hitSource <- pos.validateGenerationSignature(blockWithoutChallengeAndStateHash)
      blockchainWithNewBlock = SnapshotBlockchain(
        blockchainUpdater,
        StateSnapshot.empty,
        blockWithoutChallengeAndStateHash,
        hitSource,
        0,
        blockchainUpdater.computeNextReward,
        None
      )
      initialBlockSnapshot <- BlockDiffer.createInitialBlockSnapshot(blockchainUpdater, challengedBlock.header.reference, acc.toAddress)
      stateHash <- TxStateSnapshotHashBuilder
        .computeStateHash(
          txs,
          TxStateSnapshotHashBuilder.createHashFromSnapshot(initialBlockSnapshot, None).createHash(prevStateHash),
          initialBlockSnapshot,
          acc,
          Some(prevBlockHeader.timestamp),
          blockTime,
          isChallenging = true,
          blockchainWithNewBlock
        )
        .resultE
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
          if (blockchainWithNewBlock.supportsLightNodeBlockFields()) Some(stateHash) else None,
          if (blockchainWithNewBlock.supportsLightNodeBlockFields())
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
          else None
        )
    } yield {
      println(s"Forged challenging block $challengingBlock")
      challengingBlock
    }
  }.flatMap {
    case res @ Right(block) => waitForTimeAlign(block.header.timestamp, timeDrift).map(_ => res)
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

  private def waitForTimeAlign(blockTime: Long, timeDrift: Long): Task[Unit] =
    Task {
      val currentTime = timeService.correctedTime()
      blockTime - currentTime - timeDrift
    }.flatMap { timeDiff =>
      if (timeDiff > 0) {
        Task.sleep(timeDiff.millis)
      } else {
        Task.unit
      }
    }
}
