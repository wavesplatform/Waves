package com.wavesplatform.mining

import cats.syntax.traverse.*
import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.block.{Block, ChallengedHeader, FinalizationVoting}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.BlockStats
import com.wavesplatform.network.*
import com.wavesplatform.network.MicroBlockSynchronizer.MicroblockData
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult.Applied
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.state.{Blockchain, Height, SnapshotBlockchain, StateSnapshot, TxStateSnapshotHashBuilder}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.{BlockchainUpdater, Transaction}
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.wallet.Wallet
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

trait BlockChallenger {
  def challengeBlock(block: Block, ch: Channel): Unit
  def challengeMicroblock(md: MicroblockData): Unit
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
    pos: PoSSelector,
    appendBlock: Block => Either[ValidationError, BlockApplyResult]
) extends BlockChallenger
    with ScorexLogging {

  private val processingTxs: ConcurrentHashMap[ByteStr, Transaction] = new ConcurrentHashMap()

  override def challengeBlock(block: Block, ch: Channel): Unit = {
    log.debug(s"Challenging block $block")

    withProcessingTxs(block.transactionData) {
      (for {
        challengingBlock <- createChallengingBlock(
          block,
          block.header.stateHash,
          block.signature,
          block.transactionData,
          blockchainUpdater.lastStateHash(Some(block.header.reference)),
          block.header.finalizationVoting
        )
        applyResult <- appendBlock(challengingBlock)
      } yield (applyResult -> challengingBlock)) match {
        case Right((_: Applied, challengingBlock)) =>
          log.debug(s"Successfully challenged $block with $challengingBlock")
          BlockStats.challenged(challengingBlock, blockchainUpdater.height)
          if (blockchainUpdater.isLastBlockId(challengingBlock.id())) {
            allChannels.broadcast(BlockForged(challengingBlock), Some(ch))
          }
        case Right((_, challengingBlock)) => log.debug(s"Ignored challenging block $challengingBlock")
        case Left(err)                    => log.debug(s"Could not challenge $block: $err")
      }
    }
  }

  override def challengeMicroblock(md: MicroblockData): Unit = {
    val idStr = md.inv.totalBlockId.toString
    log.debug(s"Challenging microblock $idStr")

    (for {
      discarded <- blockchainUpdater.removeAfter(blockchainUpdater.lastBlockHeader.get.header.reference)
      block     <- discarded.headOption.map(_._1).toRight(GenericError("Liquid block wasn't discarded"))
      txs = block.transactionData ++ md.microBlock.transactionData
      (applyResult, challengingBlock) <- withProcessingTxs(txs) {
        for {
          challengingBlock <-
            createChallengingBlock(
              block,
              md.microBlock.stateHash,
              md.microBlock.totalResBlockSig,
              txs,
              blockchainUpdater.lastStateHash(Some(block.header.reference)),
              FinalizationVoting.combine(block.header.finalizationVoting, md.microBlock.finalizationVoting)
            )
          applyResult <- appendBlock(challengingBlock)
        } yield applyResult -> challengingBlock
      }
    } yield {
      applyResult match {
        case _: Applied =>
          log.debug(s"Successfully challenged microblock $idStr with $challengingBlock")
          BlockStats.challenged(challengingBlock, blockchainUpdater.height)
          if (blockchainUpdater.isLastBlockId(challengingBlock.id())) {
            allChannels.broadcast(BlockForged(challengingBlock))
          }
        case _ =>
          log.debug(s"Ignored challenging block $challengingBlock")
      }
    }).fold(
      err => log.debug(s"Could not challenge microblock $idStr: $err"),
      identity
    )
  }

  override def pickBestAccount(accounts: Seq[(SeedKeyPair, Long)]): Either[GenericError, (SeedKeyPair, Long)] =
    accounts.minByOption(_._2).toRight(GenericError("No suitable account in wallet"))

  override def getChallengingAccounts(challengedMiner: Address): Either[ValidationError, Seq[(SeedKeyPair, Long)]] =
    wallet.privateKeyAccounts
      .map { pk =>
        pk -> blockchainUpdater.generatingBalance(pk.toAddress)
      }
      .filter { case (pk, balance) =>
        blockchainUpdater.isCommitted(Height(blockchainUpdater.height), pk.toAddress) // Only a committed generator can challenge at current height
        && blockchainUpdater.isMiningAllowed(blockchainUpdater.height, balance)
      }
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

  override def getProcessingTx(id: ByteStr): Option[Transaction] = Option(processingTxs.get(id))

  override def allProcessingTxs: Seq[Transaction] = processingTxs.values.asScala.toSeq

  private def withProcessingTxs[A](txs: Seq[Transaction])(body: => A): A = {
    processingTxs.putAll(txs.map(tx => tx.id() -> tx).toMap.asJava)
    val result = body
    processingTxs.clear()
    result
  }

  private def createChallengingBlock(
      challengedBlock: Block,
      challengedStateHash: Option[ByteStr],
      challengedSignature: ByteStr,
      txs: Seq[Transaction],
      prevStateHash: ByteStr,
      challengedFinalizationVoting: Option[FinalizationVoting]
  ): Either[ValidationError, Block] = {
    val prevBlockHeader = blockchainUpdater
      .heightOf(challengedBlock.header.reference)
      .flatMap(blockchainUpdater.blockHeader)
      .map(_.header)
      .getOrElse(blockchainUpdater.lastBlockHeader.get.header)

    for {
      allAccounts               <- getChallengingAccounts(challengedBlock.sender.toAddress)
      (bestMinerAccount, delay) <- pickBestAccount(allAccounts)
      blockTime = prevBlockHeader.timestamp + delay
      _ <- Either.cond(
        blockTime < challengedBlock.header.timestamp,
        (),
        GenericError(s"Challenging block timestamp ($blockTime) is not better than challenged block timestamp (${challengedBlock.header.timestamp})")
      )
      consensusData <- pos.consensusData(
        bestMinerAccount,
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
        bestMinerAccount,
        challengedBlock.header.featureVotes,
        challengedBlock.header.rewardVote,
        stateHash = None,
        challengedHeader = None,
        finalizationVoting = challengedFinalizationVoting
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
      initialBlockSnapshot <- BlockDiffer.createInitialBlockSnapshot(blockchainUpdater, challengedBlock.header.reference, bestMinerAccount.toAddress)
      stateHash <- TxStateSnapshotHashBuilder
        .computeStateHash(
          txs,
          TxStateSnapshotHashBuilder.createHashFromSnapshot(initialBlockSnapshot, None).createHash(prevStateHash),
          initialBlockSnapshot,
          bestMinerAccount,
          Some(prevBlockHeader.timestamp),
          blockTime,
          isChallenging = true,
          blockchainWithNewBlock
        )
        .resultE
      challengingBlock <- Block.buildAndSign(
        challengedBlock.header.version,
        blockTime,
        challengedBlock.header.reference,
        consensusData.baseTarget,
        consensusData.generationSignature,
        txs,
        bestMinerAccount,
        Miner.blockFeatures(blockchainUpdater, settings),
        settings.rewardsSettings.desired.getOrElse(-1L),
        Some(stateHash),
        Some(
          ChallengedHeader(
            challengedBlock.header.timestamp,
            challengedBlock.header.baseTarget,
            challengedBlock.header.generationSignature,
            challengedBlock.header.featureVotes,
            challengedBlock.header.generator,
            challengedBlock.header.rewardVote,
            challengedStateHash,
            challengedSignature,
            challengedFinalizationVoting
          )
        ),
        finalizationVoting = None
      )
    } yield {
      log.debug(s"Forged challenging block $challengingBlock")
      challengingBlock
    }
  }
}
