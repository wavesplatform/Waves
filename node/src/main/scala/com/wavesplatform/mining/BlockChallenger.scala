package com.wavesplatform.mining

import cats.data.EitherT
import cats.syntax.either.*
import cats.syntax.traverse.*
import com.wavesplatform.account.{Address, KeyPair, SeedKeyPair}
import com.wavesplatform.block.{Block, ChallengedHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.*
import com.wavesplatform.network.MicroBlockSynchronizer.MicroblockData
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.appender.MaxTimeDrift
import com.wavesplatform.state.diffs.BlockDiffer.CurrentBlockFeePart
import com.wavesplatform.state.diffs.{BlockDiffer, TransactionDiffer}
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.state.{Blockchain, Diff, Portfolio, TxStateSnapshotHashBuilder}
import com.wavesplatform.transaction.{BlockchainUpdater, Transaction}
import com.wavesplatform.transaction.TxValidationError.{BlockFromFuture, GenericError}
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.wallet.Wallet
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import monix.eval.Task

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

trait BlockChallenger {
  def challengeBlock(block: Block, ch: Channel, prevStateHash: ByteStr, blockReward: Option[Long]): Task[Unit]
  def challengeMicroblock(md: MicroblockData, ch: Channel, prevStateHash: ByteStr, blockReward: Option[Long]): Task[Unit]
  def pickBestAccount(accounts: Seq[(SeedKeyPair, Long)]): Either[GenericError, (SeedKeyPair, Long)]
  def getChallengingAccounts(challengedMiner: Address): Either[ValidationError, Seq[(SeedKeyPair, Long)]]
  def getProcessingTx(id: ByteStr): Option[Transaction]
  def allProcessingTxs: Seq[Transaction]
}

object BlockChallenger {
  val NoOp: BlockChallenger = new BlockChallenger {
    override def challengeBlock(block: Block, ch: Channel, prevStateHash: ByteStr, blockReward: Option[Long]): Task[Unit]            = Task.unit
    override def challengeMicroblock(md: MicroblockData, ch: Channel, prevStateHash: ByteStr, blockReward: Option[Long]): Task[Unit] = Task.unit
    override def pickBestAccount(accounts: Seq[(SeedKeyPair, Long)]): Either[GenericError, (SeedKeyPair, Long)] =
      Left(GenericError("There are no suitable accounts"))
    override def getChallengingAccounts(challengedMiner: Address): Either[ValidationError, Seq[(SeedKeyPair, Long)]] = Right(Seq.empty)
    override def getProcessingTx(id: ByteStr): Option[Transaction]                                                   = None
    override def allProcessingTxs: Seq[Transaction]                                                                  = Seq.empty
  }
}

class BlockChallengerImpl(
    blockchainUpdater: BlockchainUpdater & Blockchain,
    allChannels: ChannelGroup,
    wallet: Wallet,
    settings: WavesSettings,
    timeService: Time,
    pos: PoSSelector,
    appendBlock: Block => Task[Either[ValidationError, Option[BigInt]]]
) extends BlockChallenger
    with ScorexLogging {

  private val processingTxs: ConcurrentHashMap[ByteStr, Transaction] = new ConcurrentHashMap()

  def challengeBlock(block: Block, ch: Channel, prevStateHash: ByteStr, blockReward: Option[Long]): Task[Unit] = {
    log.debug(s"Challenging block $block")
    (for {
      _ <- EitherT.liftF(Task(processingTxs.putAll(block.transactionData.map(tx => tx.id() -> tx).toMap.asJava)))
      challengingBlock <- EitherT(
        createChallengingBlock(block, block.header.stateHash, block.signature, block.transactionData, prevStateHash, blockReward)
      )
      _ <- EitherT(appendBlock(challengingBlock))
      _ <- EitherT.liftF[Task, ValidationError, Unit](Task(processingTxs.clear()))
    } yield {
      log.debug(s"Successfully challenged $block with $challengingBlock")
      allChannels.broadcast(BlockForged(challengingBlock), Some(ch))
    }).fold(
      err => {
        processingTxs.clear()
        log.debug(s"Could not challenge $block: $err")
      },
      identity
    )
  }

  def challengeMicroblock(md: MicroblockData, ch: Channel, prevStateHash: ByteStr, blockReward: Option[Long]): Task[Unit] = {
    val idStr = md.invOpt.map(_.totalBlockId.toString).getOrElse(s"(sig=${md.microBlock.totalResBlockSig})")
    log.debug(s"Challenging microblock $idStr")

    (for {
      discarded <- EitherT(Task(blockchainUpdater.removeAfter(blockchainUpdater.lastBlockHeader.get.header.reference)))
      block     <- EitherT(Task(discarded.headOption.map(_._1).toRight(GenericError("Liquid block wasn't discarded"))))
      _ <- EitherT.liftF(Task(processingTxs.putAll((block.transactionData ++ md.microBlock.transactionData).map(tx => tx.id() -> tx).toMap.asJava)))
      challengingBlock <- EitherT(
        createChallengingBlock(
          block,
          md.microBlock.stateHash,
          md.microBlock.totalResBlockSig,
          block.transactionData ++ md.microBlock.transactionData,
          prevStateHash,
          blockReward
        )
      )
      _ <- EitherT(appendBlock(challengingBlock))
      _ <- EitherT.liftF[Task, ValidationError, Unit](Task(processingTxs.clear()))
    } yield {
      log.debug(s"Successfully challenged microblock $idStr with $challengingBlock")
      allChannels.broadcast(BlockForged(challengingBlock), Some(ch))
    }).fold(
      err => {
        processingTxs.clear()
        log.debug(s"Could not challenge microblock $idStr: $err")
      },
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

  private def createChallengingBlock(
      challengedBlock: Block,
      challengedStateHash: Option[ByteStr],
      challengedSignature: ByteStr,
      txs: Seq[Transaction],
      prevStateHash: ByteStr,
      blockReward: Option[Long]
  ): Task[Either[ValidationError, Block]] = Task {
    val lastBlockHeader = blockchainUpdater.lastBlockHeader.get.header

    for {
      allAccounts  <- getChallengingAccounts(challengedBlock.sender.toAddress)
      (acc, delay) <- pickBestAccount(allAccounts)
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

      initialBlockDiff <- BlockDiffer.createInitialBlockDiff(blockchainUpdater, acc.toAddress, blockReward).leftMap(GenericError(_))
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
            computeStateHash(
              txs,
              TxStateSnapshotHashBuilder.createHashFromDiff(blockchainUpdater, initialBlockDiff).createHash(prevStateHash),
              initialBlockDiff,
              acc,
              blockTime,
              blockchainUpdater
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

  private def waitForTimeAlign(blockTime: Long): Task[Either[ValidationError, Unit]] =
    Task {
      val currentTime = timeService.correctedTime()
      blockTime - currentTime - MaxTimeDrift
    }.flatMap { timeDiff =>
      if (timeDiff > 0 && timeDiff < settings.minerSettings.maxChallengeBlockWait.toMillis) {
        Task.sleep(timeDiff.millis).as(Right(()))
      } else if (timeDiff > 0) {
        Task(Left(BlockFromFuture(blockTime)))
      } else Task(Right(()))
    }

  private def computeStateHash(
      txs: Seq[Transaction],
      initStateHash: ByteStr,
      initDiff: Diff,
      signer: KeyPair,
      timestamp: Long,
      blockchain: Blockchain
  ): ByteStr = {
    val txDiffer = TransactionDiffer(blockchain.lastBlockTimestamp, timestamp) _

    txs
      .foldLeft(initStateHash -> initDiff) { case ((prevStateHash, accDiff), tx) =>
        // TODO: NODE-2594 maybe blockchain with new block
        val compBlockchain = CompositeBlockchain(blockchain, accDiff)
        val minerDiff      = Diff(portfolios = Map(signer.toAddress -> Portfolio.waves(tx.fee).multiply(CurrentBlockFeePart)))
        txDiffer(compBlockchain, tx).resultE match {
          case Right(txDiff) =>
            val stateHash =
              TxStateSnapshotHashBuilder.createHashFromDiff(compBlockchain, txDiff.combineF(minerDiff).explicitGet()).createHash(prevStateHash)
            (stateHash, accDiff.combineF(txDiff).flatMap(_.combineF(minerDiff)).explicitGet())
          case Left(_) => (prevStateHash, accDiff)
        }
      }
      ._1
  }
}
