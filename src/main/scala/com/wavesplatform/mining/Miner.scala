package com.wavesplatform.mining

import java.time.{Duration, Instant}
import java.util.concurrent.atomic.AtomicBoolean

import com.wavesplatform.network._
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.{Coordinator, UtxPool}
import io.netty.channel.group.ChannelGroup
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution._
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.transaction.PoSCalc._
import scorex.transaction.{BlockchainUpdater, CheckpointService, History}
import scorex.utils.{ScorexLogging, Time}
import scorex.wallet.Wallet

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.math.Ordering.Implicits._


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
               wallet: Wallet,
               startingLastBlock: Block) extends ScorexLogging {

  import Miner._

  private def peerCount = allChannels.size()

  private val minerSettings = settings.minerSettings
  private val blockchainSettings = settings.blockchainSettings

  private val scheduledAttempts = TrieMap.empty[ByteStr, Cancelable]

  private def checkAge(parentHeight: Int, parent: Block): Either[String, Unit] =
    Either
      .cond(parentHeight == 1, (), Duration.between(Instant.ofEpochMilli(parent.timestamp), Instant.ofEpochMilli(timeService.correctedTime())))
      .left.flatMap(blockAge => Either.cond(blockAge <= minerSettings.intervalAfterLastBlockThenGenerationIsAllowed, (),
      s"BlockVhain is too old (last block ${parent.uniqueId} generated $blockAge ago)"
    ))

  private def generateOneBlockTask(account: PrivateKeyAccount, parentHeight: Int, parent: Block,
                                   greatGrandParent: Option[Block], balance: Long)(delay: FiniteDuration): Task[Either[String, Block]] = Task {
    val pc = peerCount
    val lastBlockKernelData = parent.consensusData
    val currentTime = timeService.correctedTime()
    val h = calcHit(lastBlockKernelData, account)
    val t = calcTarget(parent, currentTime, balance)
    for {
      _ <- Either.cond(pc >= minerSettings.quorum, (), s"Hit $h was NOT less than target $t, not forging block with ${account.address}")
      _ <- Either.cond(h < t, (), s"Quorum not available ($pc/${minerSettings.quorum}, not forging block with ${account.address}")
      _ = log.debug(s"Forging with ${account.address}, H $h < T $t, balance $balance, prev block ${parent.uniqueId}")
      _ = log.debug(s"Previous block ID ${parent.uniqueId} at $parentHeight with target ${lastBlockKernelData.baseTarget}")
      avgBlockDelay = blockchainSettings.genesisSettings.averageBlockDelay
      btg = calcBaseTarget(avgBlockDelay, parentHeight, parent, greatGrandParent, currentTime)
      gs = calcGeneratorSignature(lastBlockKernelData, account)
      consensusData = NxtLikeConsensusBlockData(btg, gs)
      unconfirmed = utx.packUnconfirmed()
      _ = log.debug(s"Adding ${unconfirmed.size} unconfirmed transaction(s) to new block")
      block = Block.buildAndSign(Version, currentTime, parent.uniqueId, consensusData, unconfirmed, account)
    } yield block
  }.delayExecution(delay)

  private def generateBlockTask(account: PrivateKeyAccount, parentHeight: Int, parent: Block, greatGrandParent: Option[Block]): Task[Unit] = Task {
    (for {
      _ <- checkAge(parentHeight, parent)
      ts <- nextBlockGenerationTime(parentHeight, stateReader, blockchainSettings.functionalitySettings, parent, account)
      offset = calcOffset(timeService, ts)
      key = ByteStr(account.publicKey)
      _ = scheduledAttempts.get(key) match {
        case Some(_) => log.debug(s"Block generation already scheduled for $key")
        case None =>
          log.debug(s"Next attempt for acc=$account in $offset")
          val balance = generatingBalance(stateReader, blockchainSettings.functionalitySettings, account, parentHeight)
          val blockGenTask = generateOneBlockTask(account, parentHeight, parent, greatGrandParent, balance)(offset).flatMap {
            case Right(block) => Task {
              CoordinatorHandler.loggingResult("local", s"locally mined block (${
                block.uniqueId
              })",
                Coordinator.processBlock(checkpoint, history, blockchainUpdater, timeService, stateReader,
                  utx, blockchainReadiness, Miner.this, settings)(block, local = true)
                  .map {
                    score =>
                      allChannels.broadcast(ScoreChanged(score))
                      allChannels.broadcast(BlockForged(block))
                      score
                  })
            }
            case Left(err) =>
              scheduledAttempts.remove(key)
              log.debug(s"No block generated because $err, retrying")
              generateBlockTask(account, parentHeight, parent, greatGrandParent)
          }
          scheduledAttempts.put(key, blockGenTask.runAsync)
      }
    } yield ()).left.map(err => log.debug(s"NOT scheduling block generation: $err"))
  }

  def lastBlockChanged(newHeight: Int, newBlock: Block): Unit = {
    log.debug(s"Miner notified of new block")
    scheduledAttempts.values.foreach(_.cancel)
    scheduledAttempts.clear()
    val grandParent = history.parent(newBlock, 2)
    wallet.privateKeyAccounts().foreach(generateBlockTask(_, newHeight, newBlock, grandParent).runAsync)
  }

  def shutdown(): Unit = ()
}

object Miner extends ScorexLogging {
  val Version: Byte = 3
  val MinimalGenerationOffsetMillis: Long = 1001

  def calcOffset(timeService: Time, ts: Long): FiniteDuration = {
    val generationInstant = ts + 10
    val calculatedOffset = generationInstant - timeService.correctedTime()
    Math.max(MinimalGenerationOffsetMillis, calculatedOffset).millis
  }
}