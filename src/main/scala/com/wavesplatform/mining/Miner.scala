package com.wavesplatform.mining

import java.time.{Duration, Instant}
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicBoolean

import com.google.common.util.concurrent.{FutureCallback, Futures, ListenableScheduledFuture, MoreExecutors}
import com.wavesplatform.network._
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.{Coordinator, UtxPool}
import io.netty.channel.group.ChannelGroup
import scorex.account.PrivateKeyAccount
import scorex.block.{Block, MicroBlock}
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.PoSCalc._
import scorex.transaction.{BlockchainUpdater, CheckpointService, History}
import scorex.utils.{ScorexLogging, SynchronizedOne, Time}
import scorex.wallet.Wallet

import scala.math.Ordering.Implicits._


class Miner(
               allChannels: ChannelGroup,
               blockchainReadiness: AtomicBoolean,
               blockchainUpdater: BlockchainUpdater,
               checkpoint: CheckpointService,
               history: History,
               stateReader: StateReader,
               settings: WavesSettings,
               time: Time,
               utx: UtxPool,
               wallet: Wallet,
               startingLastBlock: Block) extends ScorexLogging with SynchronizedOne {

  import Miner._

  private val minerPool = MoreExecutors.listeningDecorator(Executors.newScheduledThreadPool(2))
  private val scheduledAttempts = Synchronized(scala.collection.concurrent.TrieMap.empty[ByteStr, ScheduledFuture[_]])
  private val accumulatedBlock = Synchronized(startingLastBlock)
  private val microblockScheduledFuture: Synchronized[Option[ListenableScheduledFuture[_]]] = Synchronized(None)

  private val minerSettings = settings.minerSettings

  private def checkAge(parentHeight: Int, parent: Block): Either[String, Unit] =
    Either
      .cond(parentHeight == 1, (), Duration.between(Instant.ofEpochMilli(parent.timestamp), Instant.ofEpochMilli(time.correctedTime())))
      .left.flatMap(blockAge => Either.cond(
      blockAge <= minerSettings.intervalAfterLastBlockThenGenerationIsAllowed,
      (),
      s"Blockchain is too old (last block ${parent.uniqueId} generated $blockAge ago)"
    ))

  private def generateBlock(account: PrivateKeyAccount, parentHeight: Int, parent: Block,
                            greatGrandParent: Option[Block], balance: Long): Callable[Option[Block]] = () => {
    val pc = allChannels.size()
    if (pc >= minerSettings.quorum) {
      val lastBlockKernelData = parent.consensusData
      val currentTime = time.correctedTime()
      val h = calcHit(lastBlockKernelData, account)
      val t = calcTarget(parent, currentTime, balance)
      if (h < t) {
        log.debug(s"Forging with ${account.address}, H $h < T $t, balance $balance, prev block ${parent.uniqueId}")
        log.debug(s"Previous block ID ${parent.uniqueId} at $parentHeight with target ${lastBlockKernelData.baseTarget}")

        val avgBlockDelay = settings.blockchainSettings.genesisSettings.averageBlockDelay
        val btg = calcBaseTarget(avgBlockDelay, parentHeight, parent, greatGrandParent, currentTime)
        val gs = calcGeneratorSignature(lastBlockKernelData, account)
        val consensusData = NxtLikeConsensusBlockData(btg, gs)

        val unconfirmed = utx.packUnconfirmed()
        log.debug(s"Adding ${unconfirmed.size} unconfirmed transaction(s) to new block")

        Some(Block.buildAndSign(Version, currentTime, parent.uniqueId, consensusData, unconfirmed, account))
      } else {
        log.warn(s"Hit $h was NOT less than target $t, not forging block with ${account.address}")
        None
      }
    } else {
      log.debug(s"Quorum not available ($pc/${minerSettings.quorum}, not forging block with ${account.address}")
      None
    }
  }

  // Next block generation time can be None only when the account's generating balance is less than required. The
  // balance, in turn, changes only when new block is appended to the blockchain, which causes all scheduled forging
  // attempts to be cancelled and re-scheduled over the new parent.
  private def retry(parentHeight: Int, parent: Block, greatGrandParent: Option[Block])(account: PrivateKeyAccount): Unit = {
    val calculatedTimestamp = for {
      _ <- checkAge(parentHeight, parent)
      ts <- nextBlockGenerationTime(parentHeight, stateReader, settings.blockchainSettings.functionalitySettings, parent, account)
    } yield ts

    calculatedTimestamp match {
      case Left(e) => log.debug(s"NOT scheduling block generation: $e")
      case Right(ts) => write { implicit l =>
        val generationInstant = Instant.ofEpochMilli(ts + 10)
        val calculatedOffset = Duration.between(Instant.ofEpochMilli(time.correctedTime()), generationInstant)
        val generationOffset = MinimalGenerationOffset.max(calculatedOffset)
        val balance = generatingBalance(stateReader, settings.blockchainSettings.functionalitySettings, account, parentHeight)

        val key = ByteStr(account.publicKey)
        scheduledAttempts().get(key) match {
          case Some(_) => log.debug(s"Block generation already scheduled for $key")
          case None if !minerPool.isShutdown =>
            log.debug(s"Next attempt in $calculatedOffset ${if (calculatedOffset == generationOffset) "" else s"(adjusted to $generationOffset)"} " +
              s"at $generationInstant with ${account.address}, parent ${parent.uniqueId}")
            val thisAttempt = minerPool.schedule(generateBlock(account, parentHeight, parent, greatGrandParent, balance),
              generationOffset.toMillis, TimeUnit.MILLISECONDS)

            Futures.addCallback(thisAttempt, new FutureCallback[Option[Block]] {
              override def onSuccess(result: Option[Block]): Unit = result match {
                case Some(block) =>
                  CoordinatorHandler.loggingResult("local", s"locally mined block (${block.uniqueId})",
                    Coordinator.processBlock(checkpoint, history, blockchainUpdater, time, stateReader,
                      utx, blockchainReadiness, Miner.this, settings)(block, local = true)
                      .map { score =>
                        allChannels.broadcast(ScoreChanged(score))
                        allChannels.broadcast(BlockForged(block))
                        score
                      })
                case None =>
                  if (scheduledAttempts.mutate(_.remove(key, thisAttempt))) {
                    log.debug(s"No block generated: Retrying")
                    retry(parentHeight, parent, greatGrandParent)(account)
                  }
              }

              override def onFailure(t: Throwable): Unit = {}
            })
          case _ =>
        }
      }
    }
  }

  def lastBlockChanged(): Unit = write { implicit l =>
    val lastBlock = history.lastBlock.get
    accumulatedBlock.set(lastBlock)

    val currentAttempts = scheduledAttempts()
    log.debug(s"New block: ${lastBlock.uniqueId}; cancelling scheduled mining tasks(count=${currentAttempts.size}) and trying to schedule new attempts")
    currentAttempts.values.foreach(_.cancel(false))
    scheduledAttempts.mutate(_.clear())
    val grandParent = history.parent(lastBlock, 2)
    wallet.privateKeyAccounts().foreach(retry(history.height(), lastBlock, grandParent))
    microblockScheduledFuture().foreach(_.cancel(false))
    microblockScheduledFuture.set(None)
    wallet.privateKeyAccounts().find(pk => lastBlock.signerData.generator == pk) foreach { account =>
      microblockScheduledFuture.set(Some(minerPool.scheduleAtFixedRate(() => {
        val unconfirmed = utx.packUnconfirmed()
        if (unconfirmed.isEmpty) {
          log.debug("skipping microblock because no txs in utx pool")
        }
        else {
          log.debug(s"Adding ${unconfirmed.size} unconfirmed transaction(s) to new microBlock")
          val unsigned = accumulatedBlock().copy(version = 3, transactionData = accumulatedBlock().transactionData ++ unconfirmed)
          val signature = ByteStr(EllipticCurveImpl.sign(account, unsigned.bytes))
          val signed = accumulatedBlock().copy(signerData = accumulatedBlock().signerData.copy(signature = signature))

          (for {
            micro <- MicroBlock.buildAndSign(account, unconfirmed, lastBlock.signerData.signature, signature)
            r <- Coordinator.processMicroBlock(checkpoint, history, blockchainUpdater, utx)(micro)
          } yield r) match {
            case Left(err) => log.debug(err.toString)
            case Right(_) =>
              accumulatedBlock.set(signed)
          }
        }
      }, 3, 3, TimeUnit.SECONDS)))
    }
  }

  def shutdown(): Unit = minerPool.shutdownNow()
}

object Miner extends ScorexLogging {
  private val minimalDurationMillis = 1001
  val Version: Byte = 3
  val MinimalGenerationOffset: Duration = Duration.ofMillis(minimalDurationMillis)
}
