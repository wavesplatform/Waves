package com.wavesplatform.mining

import java.util.concurrent.atomic.AtomicBoolean

import com.wavesplatform.network._
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.{Coordinator, UtxPool}
import io.netty.channel.group.ChannelGroup
import monix.eval.Task
import monix.execution._
import monix.execution.cancelables.{CompositeCancelable, SerialCancelable}
import scorex.account.PrivateKeyAccount
import scorex.block.{Block, MicroBlock}
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.transaction.PoSCalc._
import scorex.transaction.{BlockchainUpdater, CheckpointService, History, ValidationError}
import scorex.utils.{ScorexLogging, Time}
import scorex.wallet.Wallet

import scala.concurrent.duration._

class Miner(
               allChannels: ChannelGroup,
               blockchainReadiness: AtomicBoolean,
               blockchainUpdater: BlockchainUpdater,
               checkpoint: CheckpointService,
               history: History,
               stateReader: StateReader,
               settings: WavesSettings,
               timeService: Time,
               utx: UtxPool,
               wallet: Wallet) extends ScorexLogging {

  import Miner._

  private implicit val scheduler = Scheduler.fixedPool(name = "miner-pool", poolSize = 2)

  private val minerSettings = settings.minerSettings
  private val blockchainSettings = settings.blockchainSettings
  private lazy val processBlock = Coordinator.processSingleBlock(checkpoint, history, blockchainUpdater, timeService, stateReader, utx, blockchainReadiness, settings) _

  private val scheduledAttempts = SerialCancelable()
  private val microBlockAttempt = SerialCancelable()

  private def checkAge(parentHeight: Int, parentTimestamp: Long): Either[String, Unit] =
    Either.cond(parentHeight == 1, (), (timeService.correctedTime() - parentTimestamp).millis)
      .left.flatMap(blockAge => Either.cond(blockAge <= minerSettings.intervalAfterLastBlockThenGenerationIsAllowed, (),
      s"BlockChain is too old (last block timestamp is $parentTimestamp generated $blockAge ago)"
    ))

  private def generateOneBlockTask(account: PrivateKeyAccount, parentHeight: Int,
                                   greatGrandParent: Option[Block], balance: Long)(delay: FiniteDuration): Task[Either[String, Block]] = Task {
    // should take last block right at the time of mining since microblocks might have been added
    // the rest doesn't change
    val parent = history.lastBlock.get
    val pc = allChannels.size()
    lazy val lastBlockKernelData = parent.consensusData
    lazy val currentTime = timeService.correctedTime()
    lazy val h = calcHit(lastBlockKernelData, account)
    lazy val t = calcTarget(parent, currentTime, balance)
    for {
      _ <- Either.cond(pc >= minerSettings.quorum, (), s"Quorum not available ($pc/${minerSettings.quorum}, not forging block with ${account.address}")
      _ <- Either.cond(h < t, (), s"${System.currentTimeMillis()}: Hit $h was NOT less than target $t, not forging block with ${account.address}")
      _ = log.debug(s"Forging with ${account.address}, H $h < T $t, balance $balance, prev block ${parent.uniqueId}")
      _ = log.debug(s"Previous block ID ${parent.uniqueId} at $parentHeight with target ${lastBlockKernelData.baseTarget}")
      avgBlockDelay = blockchainSettings.genesisSettings.averageBlockDelay
      btg = calcBaseTarget(avgBlockDelay, parentHeight, parent, greatGrandParent, currentTime)
      gs = calcGeneratorSignature(lastBlockKernelData, account)
      consensusData = NxtLikeConsensusBlockData(btg, gs)
      unconfirmed = utx.packUnconfirmed(minerSettings.maxTransactionsInKeyBlock)
      _ = log.debug(s"Adding ${unconfirmed.size} unconfirmed transaction(s) to new block")
      block = Block.buildAndSign(Version, currentTime, parent.uniqueId, consensusData, unconfirmed, account)
    } yield block
  }.delayExecution(delay)


  private def generateOneMicroBlockTask(account: PrivateKeyAccount, accumulatedBlock: Block): Task[Either[ValidationError, Option[Block]]] = Task {
    log.trace(s"Generating microblock for $account")
    val pc = allChannels.size()
    lazy val unconfirmed = utx.packUnconfirmed(MaxTransactionsPerMicroblock)
    if (pc < minerSettings.quorum) {
      log.trace(s"Quorum not available ($pc/${minerSettings.quorum}, not forging microblock with ${account.address}")
      Right(None)
    }
    else if (unconfirmed.isEmpty) {
      log.trace("skipping microBlock because no txs in utx pool")
      Right(None)
    } else {
      val signed = Block.buildAndSign(version = 3,
        timestamp = accumulatedBlock.timestamp,
        reference = accumulatedBlock.reference,
        consensusData = accumulatedBlock.consensusData,
        transactionData = accumulatedBlock.transactionData ++ unconfirmed,
        signer = account)
      val microBlockEi = for {
        micro <- MicroBlock.buildAndSign(account, unconfirmed, accumulatedBlock.signerData.signature, signed.signerData.signature)
        _ <- Coordinator.processMicroBlock(checkpoint, history, blockchainUpdater, utx)(micro)
      } yield micro

      microBlockEi match {
        case Right(mb) =>
          log.trace(s"MicroBlock(id=${trim(mb.uniqueId)}) has been mined for $account}")
          allChannels.broadcast(MicroBlockInv(mb.totalResBlockSig))
          Right(Some(signed))
        case Left(err) =>
          log.trace(s"MicroBlock has NOT been mined for $account} because $err")
          Left(err)
      }

    }
  }.delayExecution(minerSettings.microBlockInterval)

  private def generateMicroBlockSequence(account: PrivateKeyAccount, accumulatedBlock: Block): Task[Unit] =
    generateOneMicroBlockTask(account, accumulatedBlock).flatMap {
      case Left(err) => Task(log.warn("Error mining MicroBlock: " + err.toString))
      case Right(maybeNewTotal) => generateMicroBlockSequence(account, maybeNewTotal.getOrElse(accumulatedBlock))
    }

  private def generateBlockTask(account: PrivateKeyAccount): Task[Unit] = {
    val height = history.height()
    val lastBlock = history.lastBlock.get
    val grandParent = history.parent(lastBlock, 2)
    (for {
      _ <- checkAge(height, history.lastBlockTimestamp().get)
      ts <- nextBlockGenerationTime(height, stateReader, blockchainSettings.functionalitySettings, lastBlock, account)
    } yield ts) match {
      case Right(ts) =>
        val offset = calcOffset(timeService, ts, minerSettings.minimalBlockGenerationOffset)
        log.debug(s"Next attempt for acc=$account in $offset")
        val balance = generatingBalance(stateReader, blockchainSettings.functionalitySettings, account, height)
        generateOneBlockTask(account, height, grandParent, balance)(offset).flatMap {
          case Right(block) => Task.now {
            processBlock(block, true) match {
              case Left(err) => log.warn(err.toString)
              case Right(score) =>
                allChannels.broadcast(LocalScoreChanged(score))
                allChannels.broadcast(BlockForged(block))
                scheduleMining()
                startMicroBlockMining(account, block)
            }
          }
          case Left(err) =>
            log.debug(s"No block generated because $err, retrying")
            generateBlockTask(account)
        }
      case Left(err) =>
        log.debug(s"Not scheduling block mining because $err")
        Task.unit
    }
  }

  def scheduleMining(): Unit = {
    scheduledAttempts := CompositeCancelable.fromSet(
      wallet.privateKeyAccounts().map(generateBlockTask).map(_.runAsync).toSet)
    microBlockAttempt := SerialCancelable()
    log.debug(s"Block mining scheduled")
  }

  private def startMicroBlockMining(account: PrivateKeyAccount, lastBlock: Block): Unit = {
    microBlockAttempt := generateMicroBlockSequence(account, lastBlock).runAsync
    log.trace(s"MicroBlock mining scheduled for $account")
  }

  def shutdown(): Unit = ()
}

object Miner extends ScorexLogging {

  val MaxTransactionsPerMicroblock: Int = 255
  val Version: Byte = 3

  def calcOffset(timeService: Time, calculatedTimestamp: Long, minimalBlockGenerationOffset: FiniteDuration): FiniteDuration = {
    val calculatedGenerationTimestamp = (Math.ceil(calculatedTimestamp / 1000.0) * 1000).toLong
    val calculatedOffset = calculatedGenerationTimestamp - timeService.correctedTime()
    Math.max(minimalBlockGenerationOffset.toMillis, calculatedOffset).millis
  }
}