package com.wavesplatform.state2

import com.wavesplatform.UtxPool
import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.mining.Miner
import com.wavesplatform.network._
import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings}
import com.wavesplatform.state2.reader.SnapshotStateReader
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import monix.eval.Task
import scorex.block.Block
import scorex.consensus.TransactionsOrdering
import scorex.transaction.PoSCalc.{calcBaseTarget, calcGeneratorSignature, calcHit, calcTarget}
import scorex.transaction._
import scorex.transaction.ValidationError.GenericError
import scorex.utils.{ScorexLogging, Time}
import scorex.transaction.PoSCalc._

import scala.util.{Left, Right}

package object appender extends ScorexLogging {

  private val MaxTimeDrift: Long = 100 // millis

  private[appender] val scheduler = monix.execution.Scheduler.singleThread("appender")

  private[appender] def processAndBlacklistOnFailure[A, B](ch: Channel, peerDatabase: PeerDatabase, miner: Miner, allChannels: ChannelGroup,
                                                           start: => String, success: => String, errorPrefix: String)(
                                                              f: => Task[Either[B, Option[BigInt]]]): Task[Either[B, Option[BigInt]]] = {

    log.debug(start)
    f map {
      case Right(maybeNewScore) =>
        log.debug(success)
        maybeNewScore.foreach(scheduleMiningAndBroadcastScore(miner, allChannels))
        Right(maybeNewScore)
      case Left(ve) =>
        log.warn(s"$errorPrefix: $ve")
        peerDatabase.blacklistAndClose(ch, s"$errorPrefix: $ve")
        Left(ve)
    }
  }

  def scheduleMiningAndBroadcastScore(miner: Miner, allChannels: ChannelGroup)(score: BigInt): Unit = {
    miner.scheduleMining()
    allChannels.broadcast(LocalScoreChanged(score))
  }

  private def validateEffectiveBalance(fp: FeatureProvider, fs: FunctionalitySettings, block: Block, baseHeight: Int)(effectiveBalance: Long): Either[String, Long] =
    Either.cond(block.timestamp < fs.minimalGeneratingBalanceAfter ||
      (block.timestamp >= fs.minimalGeneratingBalanceAfter && effectiveBalance >= MinimalEffectiveBalanceForGenerator1) ||
      fp.featureActivationHeight(BlockchainFeatures.SmallerMinimalGeneratingBalance.id).exists(baseHeight >= _)
        && effectiveBalance >= MinimalEffectiveBalanceForGenerator2, effectiveBalance,
      s"generator's effective balance $effectiveBalance is less that required for generation")

  private[appender] def appendBlock(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater,
                                    stateReader: SnapshotStateReader, utxStorage: UtxPool, time: Time, settings: BlockchainSettings,
                                    featureProvider: FeatureProvider)(block: Block): Either[ValidationError, Option[Int]] = for {
    _ <- Either.cond(checkpoint.isBlockValid(block.signerData.signature, history.height() + 1), (),
      GenericError(s"Block $block at height ${history.height() + 1} is not valid w.r.t. checkpoint"))
    _ <- blockConsensusValidation(history, featureProvider, settings, time.correctedTime(), block) { height =>
      PoSCalc.generatingBalance(stateReader, settings.functionalitySettings, block.signerData.generator, height).toEither.left.map(_.toString)
        .flatMap(validateEffectiveBalance(featureProvider, settings.functionalitySettings, block, height))
    }
    baseHeight = history.height()
    maybeDiscardedTxs <- blockchainUpdater.processBlock(block)
  } yield {
    utxStorage.removeAll(block.transactionData)
    maybeDiscardedTxs.toSeq.flatten.foreach(utxStorage.putIfNew)
    maybeDiscardedTxs.map(_ => baseHeight)
  }

  private def blockConsensusValidation(history: History, fp: FeatureProvider, bcs: BlockchainSettings, currentTs: Long, block: Block)
                                      (genBalance: Int => Either[String, Long]): Either[ValidationError, Unit] = history.read { _ =>

    val fs = bcs.functionalitySettings
    val blockTime = block.timestamp
    val generator = block.signerData.generator

    (for {
      height <- history.heightOf(block.reference).toRight(s"history does not contain parent ${block.reference}")
      _ <- Either.cond(height > fs.blockVersion3AfterHeight
        || block.version == Block.GenesisBlockVersion
        || block.version == Block.PlainBlockVersion,
        (), s"Block Version 3 can only appear at height greater than ${fs.blockVersion3AfterHeight}")
      _ <- Either.cond(blockTime - currentTs < MaxTimeDrift, (), s"timestamp $blockTime is from future")
      _ <- Either.cond(blockTime < fs.requireSortedTransactionsAfter
        || height > fs.dontRequireSortedTransactionsAfter
        || block.transactionData.sorted(TransactionsOrdering.InBlock) == block.transactionData,
        (), "transactions are not sorted")
      parent <- history.parent(block).toRight(s"history does not contain parent ${block.reference}")
      prevBlockData = parent.consensusData
      blockData = block.consensusData
      cbt = calcBaseTarget(bcs.genesisSettings.averageBlockDelay, height, parent.consensusData.baseTarget, parent.timestamp, history.parent(parent, 2).map(_.timestamp), blockTime)
      bbt = blockData.baseTarget
      _ <- Either.cond(cbt == bbt, (), s"declared baseTarget $bbt does not match calculated baseTarget $cbt")
      calcGs = calcGeneratorSignature(prevBlockData, generator)
      blockGs = blockData.generationSignature.arr
      _ <- Either.cond(calcGs.sameElements(blockGs), (), s"declared generation signature ${blockGs.mkString} does not match calculated generation signature ${calcGs.mkString}")
      effectiveBalance <- genBalance(height)
      hit = calcHit(prevBlockData, generator)
      target = calcTarget(parent.consensusData.baseTarget, parent.timestamp, blockTime, effectiveBalance)
      _ <- Either.cond(hit < target, (), s"calculated hit $hit >= calculated target $target")
    } yield ()).left.map(e => GenericError(s"Block $block is invalid: $e"))
  }

}
