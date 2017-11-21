package com.wavesplatform.network

import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.metrics.{BlockStats, _}
import com.wavesplatform.mining.Miner
import com.wavesplatform.network.MicroBlockSynchronizer.MicroblockData
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2.StateReader
import com.wavesplatform.{Coordinator, UtxPool}
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import monix.eval.Task
import monix.execution.Scheduler
import scorex.block.Block
import scorex.transaction.ValidationError.InvalidSignature
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

object CoordinatorHandler extends ScorexLogging {

  private val blockReceivingLag = Kamon.metrics.histogram("block-receiving-lag")

  def apply(checkpointService: CheckpointService,
            history: History,
            blockchainUpdater: BlockchainUpdater,
            time: Time,
            stateReader: StateReader,
            utxStorage: UtxPool,
            miner: Miner,
            settings: WavesSettings,
            peerDatabase: PeerDatabase,
            allChannels: ChannelGroup,
            featureProvider: FeatureProvider,
            microBlockOwners: MicroBlockOwners,
           )(blocks: ChannelObservable[Block],
             checkpoints: ChannelObservable[Checkpoint],
             extensions: ChannelObservable[ExtensionBlocks],
             microblockDatas: ChannelObservable[MicroblockData]): Unit = {
    val scheduler = Scheduler.singleThread("coordinator-handler")

    val processBlock = Coordinator.processSingleBlock(checkpointService, history, blockchainUpdater, time, stateReader, utxStorage, settings.blockchainSettings, featureProvider) _
    val processCheckpoint = Coordinator.processCheckpoint(checkpointService, history, blockchainUpdater) _
    val processFork = Coordinator.processFork(checkpointService, history, blockchainUpdater, stateReader, utxStorage, time, settings, featureProvider) _
    val processMicroBlock = Coordinator.processMicroBlock(checkpointService, history, blockchainUpdater, utxStorage) _

    def scheduleMiningAndBroadcastScore(score: BigInt): Unit = {
      miner.scheduleMining()
      allChannels.broadcast(LocalScoreChanged(score))
    }

    def processAndBlacklistOnFailure[A](ch: Channel, start: => String, success: => String, errorPrefix: String,
                                        f: => Either[_, Option[A]], r: A => Unit): Task[Unit] = {
      log.debug(start)
      Task(f).map {
        case Right(maybeNewScore) =>
          log.debug(success)
          maybeNewScore.foreach(r)
        case Left(ve) =>
          log.warn(s"$errorPrefix: $ve")
          peerDatabase.blacklistAndClose(ch, s"$errorPrefix: $ve")
      }.onErrorHandle[Unit]((t: Throwable) => log.debug(s"${id(ch)} Exception caught", t))
    }


    blocks.executeOn(scheduler).mapTask { case ((ch, b)) => Task {
      BlockStats.received(b, BlockStats.Source.Broadcast, ch)
      blockReceivingLag.safeRecord(System.currentTimeMillis() - b.timestamp)
      b.signaturesValid().flatMap(b => processBlock(b)) match {
        case Right(None) =>
          log.trace(s"${id(ch)} $b already appended")
        case Right(Some(newScore)) =>
          BlockStats.applied(b, BlockStats.Source.Broadcast, history.height())
          log.debug(s"${id(ch)} Appended $b")
          if (b.transactionData.isEmpty)
            allChannels.broadcast(BlockForged(b), Some(ch))
          scheduleMiningAndBroadcastScore(newScore)
        case Left(is: InvalidSignature) =>
          peerDatabase.blacklistAndClose(ch, s"Could not append $b: $is")
        case Left(ve) =>
          BlockStats.declined(b, BlockStats.Source.Broadcast)
          log.debug(s"${id(ch)} Could not append $b: $ve")
      }
    }.onErrorHandle[Unit]((t: Throwable) => log.debug(s"${id(ch)} Exception caught", t))
    }

    checkpoints.executeOn(scheduler).mapTask { case ((ch, c)) => processAndBlacklistOnFailure(ch,
      s"${id(ch)} Attempting to process checkpoint",
      s"${id(ch)} Successfully processed checkpoint",
      s"${id(ch)} Error processing checkpoint",
      processCheckpoint(c).map(Some(_)), scheduleMiningAndBroadcastScore)
    }

    extensions.executeOn(scheduler).mapTask { case ((ch, ExtensionBlocks(extensionBlocks))) =>
      extensionBlocks.foreach(BlockStats.received(_, BlockStats.Source.Ext, ch))
      processAndBlacklistOnFailure(ch,
        s"${id(ch)} Attempting to append extension ${formatBlocks(extensionBlocks)}",
        s"${id(ch)} Successfully appended extension ${formatBlocks(extensionBlocks)}",
        s"${id(ch)} Error appending extension ${formatBlocks(extensionBlocks)}",
        processFork(extensionBlocks),
        scheduleMiningAndBroadcastScore)
    }

    microblockDatas.executeOn(scheduler).mapTask { case ((ch, md)) =>
      import md.microBlock
      val microblockTotalResBlockSig = microBlock.totalResBlockSig
      Task(microBlock.signaturesValid().flatMap(processMicroBlock) match {
        case Right(()) =>
          md.invOpt match {
            case Some(mi) => allChannels.broadcast(mi, except = microBlockOwners.all(microBlock.totalResBlockSig))
            case None => log.warn(s"${id(ch)} Not broadcasting MicroBlockInv")
          }
          BlockStats.applied(microBlock)
        case Left(is: InvalidSignature) =>
          peerDatabase.blacklistAndClose(ch, s"Could not append microblock $microblockTotalResBlockSig: $is")
        case Left(ve) =>
          BlockStats.declined(microBlock)
          log.debug(s"${id(ch)} Could not append microblock $microblockTotalResBlockSig: $ve")
      }).onErrorHandle[Unit]((t: Throwable) => log.debug(s"${id(ch)} Exception caught", t))
    }
  }
}