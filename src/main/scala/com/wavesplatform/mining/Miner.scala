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
import scorex.account.PrivateKeyAccount
import scorex.block.{Block, MicroBlock}
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.transaction.PoSCalc._
import scorex.transaction.{BlockchainUpdater, CheckpointService, History, ValidationError}
import scorex.utils.{ScorexLogging, Time}
import scorex.wallet.Wallet

import scala.collection.concurrent.TrieMap
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

  private def peerCount = allChannels.size()

  private val minerSettings = settings.minerSettings
  private val blockchainSettings = settings.blockchainSettings

  private val scheduledAttempts = TrieMap.empty[ByteStr, Cancelable]
  @volatile private var microBlockMiner: Option[Cancelable] = None

  private def checkAge(parentHeight: Int, parent: Block): Either[String, Unit] =
    Either.cond(parentHeight == 1, (), (timeService.correctedTime() - parent.timestamp).millis)
      .left.flatMap(blockAge => Either.cond(blockAge <= minerSettings.intervalAfterLastBlockThenGenerationIsAllowed, (),
      s"BlockChain is too old (last block ${parent.uniqueId} generated $blockAge ago)"
    ))

  private def generateOneBlockTask(account: PrivateKeyAccount, parentHeight: Int, parent: Block,
                                   greatGrandParent: Option[Block], balance: Long)(delay: FiniteDuration): Task[Either[String, Block]] = Task {
    val pc = peerCount
    lazy val lastBlockKernelData = parent.consensusData
    lazy val currentTime = timeService.correctedTime()
    log.debug(s"${System.currentTimeMillis()}: Corrected time: $currentTime")
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
    log.trace("attempting to generate microblock")
    val pc = peerCount
    lazy val unconfirmed = utx.packUnconfirmed(MaxTransactionsPerMicroblock)
    if (pc < minerSettings.quorum) {
      log.trace(s"Quorum not available ($pc/${minerSettings.quorum}, not forging block with ${account.address}")
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
      for {
        micro <- MicroBlock.buildAndSign(account, unconfirmed, accumulatedBlock.signerData.signature, signed.signerData.signature)
        _ <- Coordinator.processMicroBlock(checkpoint, history, blockchainUpdater, utx)(micro)
      } yield {
        log.trace(s"Locally mined MicroBlock(id=${trim(micro.uniqueId)}")
        allChannels.broadcast(MicroBlockInv(micro.totalResBlockSig))
        Some(signed)
      }
    }
  }.delayExecution(minerSettings.microBlockInterval)

  private def generateMicroBlockSequence(account: PrivateKeyAccount, accumulatedBlock: Block): Task[Unit] =
    generateOneMicroBlockTask(account, accumulatedBlock).flatMap {
      case Left(err) => Task(log.warn("Error generating microblock: " + err.toString))
      case Right(maybeNewTotal) => generateMicroBlockSequence(account, maybeNewTotal.getOrElse(accumulatedBlock))
    }

  private def generateBlockTask(account: PrivateKeyAccount): Task[Unit] = Task {
    val height = history.height()
    val lastBlock = history.lastBlock.get
    val grandParent = history.parent(lastBlock, 2)
    (for {
      _ <- checkAge(height, history.lastBlock.get)
      ts <- nextBlockGenerationTime(height, stateReader, blockchainSettings.functionalitySettings, lastBlock, account)
    } yield ts) match {
      case Left(err) => log.debug(s"NOT scheduling block generation: $err")
      case Right(ts) =>
        val offset = calcOffset(timeService, ts, minerSettings.minimalBlockGenerationOffset)
        val key = ByteStr(account.publicKey)
        scheduledAttempts.remove(key).foreach(_.cancel())
        log.trace(s"Scheduling mining task for account=$account in $offset, referencing ${trim(lastBlock.uniqueId)})")
        val balance = generatingBalance(stateReader, blockchainSettings.functionalitySettings, account, height)
        val blockGenTask = generateOneBlockTask(account, height, lastBlock, grandParent, balance)(offset).flatMap {
          case Right(block) =>
            Coordinator.processBlock(checkpoint, history, blockchainUpdater, timeService, stateReader,
              utx, blockchainReadiness, settings)(block, local = true) match {
              case Left(err) => Task(log.warn(err.toString))
              case Right(score) => Task {
                allChannels.broadcast(LocalScoreChanged(score))
                allChannels.broadcast(BlockForged(block))
                scheduleMining()
                startMicroBlockMining(account, block)
              }
            }
          case Left(err) =>
            scheduledAttempts.remove(key)
            log.debug(s"No block generated because $err, retrying")
            generateBlockTask(account)
        }
        scheduledAttempts.put(key, blockGenTask.runAsync)
    }
  }

  def scheduleMining(): Unit = {
    log.debug(s"Miner notified of new block")
    scheduledAttempts.values.foreach(_.cancel)
    scheduledAttempts.clear()
    if (wallet.privateKeyAccount(history.lastBlock.get.signerData.generator).isLeft) {
      microBlockMiner.foreach(_.cancel())
      microBlockMiner = None
    }
    wallet.privateKeyAccounts().foreach(generateBlockTask(_).runAsync)
  }

  private def startMicroBlockMining(account: PrivateKeyAccount, lastBlock: Block): Unit = {
    microBlockMiner = Some(generateMicroBlockSequence(account, lastBlock).runAsync)
    log.trace("requested to generate microblock")
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