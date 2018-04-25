package com.wavesplatform.state

import com.wavesplatform.consensus.{GeneratingBalanceProvider, PoSCalculator}
import com.wavesplatform.mining._
import com.wavesplatform.network._
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.utx.UtxPool
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import monix.eval.Task
import scorex.block.Block
import scorex.consensus.TransactionsOrdering
import scorex.transaction.ValidationError.{BlockAppendError, BlockFromFuture, GenericError}
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

import scala.util.{Left, Right}

package object appender extends ScorexLogging {

  private val MaxTimeDrift: Long = 100 // millis

  private val correctBlockId1 = ByteStr.decodeBase58("2GNCYVy7k3kEPXzz12saMtRDeXFKr8cymVsG8Yxx3sZZ75eHj9csfXnGHuuJe7XawbcwjKdifUrV1uMq4ZNCWPf1").get
  private val correctBlockId2 = ByteStr.decodeBase58("5uZoDnRKeWZV9Thu2nvJVZ5dBvPB7k2gvpzFD618FMXCbBVBMN2rRyvKBZBhAGnGdgeh2LXEeSr9bJqruJxngsE7").get
  private val height1         = 812608
  private val height2         = 813207

  private[appender] def processAndBlacklistOnFailure[A, B](
      ch: Channel,
      peerDatabase: PeerDatabase,
      miner: Miner,
      allChannels: ChannelGroup,
      start: => String,
      success: => String,
      errorPrefix: String)(f: => Task[Either[B, Option[BigInt]]]): Task[Either[B, Option[BigInt]]] = {

    log.debug(start)
    f map {
      case Right(maybeNewScore) =>
        log.debug(success)
        maybeNewScore.foreach(_ => miner.scheduleMining())
        Right(maybeNewScore)
      case Left(ve) =>
        log.warn(s"$errorPrefix: $ve")
        peerDatabase.blacklistAndClose(ch, s"$errorPrefix: $ve")
        Left(ve)
    }
  }

  private[appender] def appendBlock(checkpoint: CheckpointService,
                                    blockchainUpdater: BlockchainUpdater with Blockchain,
                                    utxStorage: UtxPool,
                                    pos: PoSCalculator,
                                    time: Time,
                                    settings: WavesSettings)(block: Block): Either[ValidationError, Option[Int]] =
    for {
      _ <- Either.cond(
        checkpoint.isBlockValid(block.signerData.signature, blockchainUpdater.height + 1),
        (),
        BlockAppendError(s"Block $block at height ${blockchainUpdater.height + 1} is not valid w.r.t. checkpoint", block)
      )
      _ <- Either.cond(
        blockchainUpdater.accountScript(block.sender).isEmpty,
        (),
        BlockAppendError(s"Account(${block.sender.toAddress}) is scripted are therefore not allowed to forge blocks", block)
      )
      _ <- blockConsensusValidation(blockchainUpdater, settings, pos, time.correctedTime(), block) { height =>
        val balance = GeneratingBalanceProvider.balance(blockchainUpdater, settings.blockchainSettings.functionalitySettings, height, block.sender)
        Either.cond(
          GeneratingBalanceProvider.isEffectiveBalanceValid(blockchainUpdater,
                                                            settings.blockchainSettings.functionalitySettings,
                                                            height,
                                                            block,
                                                            balance),
          balance,
          s"generator's effective balance $balance is less that required for generation"
        )
      }
      baseHeight = blockchainUpdater.height
      maybeDiscardedTxs <- blockchainUpdater.processBlock(block)
    } yield {
      utxStorage.removeAll(block.transactionData)
      utxStorage.batched { ops =>
        maybeDiscardedTxs.toSeq.flatten.foreach(ops.putIfNew)
      }
      maybeDiscardedTxs.map(_ => baseHeight)
    }

  private def blockConsensusValidation(blockchain: Blockchain, settings: WavesSettings, pos: PoSCalculator, currentTs: Long, block: Block)(
      genBalance: Int => Either[String, Long]): Either[ValidationError, Unit] = {

    val bcs       = settings.blockchainSettings
    val fs        = bcs.functionalitySettings
    val blockTime = block.timestamp
    val generator = block.signerData.generator

    val r: Either[ValidationError, Unit] = for {
      height <- blockchain.heightOf(block.reference).toRight(GenericError(s"height: history does not contain parent ${block.reference}"))
      _ <- Either.cond(
        height > fs.blockVersion3AfterHeight
          || block.version == Block.GenesisBlockVersion
          || block.version == Block.PlainBlockVersion,
        (),
        GenericError(s"Block Version 3 can only appear at height greater than ${fs.blockVersion3AfterHeight}")
      )
      _ <- Either.cond(blockTime - currentTs < MaxTimeDrift, (), BlockFromFuture(blockTime))
      _ <- {
        val constraints = MiningEstimators(settings.minerSettings, blockchain, height)
        Either.cond(!OneDimensionalMiningConstraint.full(constraints.total).put(block).isOverfilled, (), GenericError("Block is full"))
      }
      _ <- Either.cond(
        blockTime < fs.requireSortedTransactionsAfter
          || height > fs.dontRequireSortedTransactionsAfter
          || block.transactionData.sorted(TransactionsOrdering.InBlock) == block.transactionData,
        (),
        GenericError("transactions are not sorted")
      )
      parent <- blockchain.parent(block).toRight(GenericError(s"parent: history does not contain parent ${block.reference}"))
      prevBlockData = parent.consensusData
      blockData     = block.consensusData
      ggp           = blockchain.parent(parent, 2)
      cbt = pos.baseTarget(bcs.genesisSettings.averageBlockDelay.toSeconds,
                           height,
                           parent.consensusData.baseTarget,
                           parent.timestamp,
                           ggp.map(_.timestamp),
                           blockTime)
      bbt = blockData.baseTarget
      _ <- Either.cond(cbt == bbt, (), GenericError(s"declared baseTarget $bbt does not match calculated baseTarget $cbt"))
      calcGs  = pos.generatorSignature(prevBlockData.generationSignature.arr, generator.publicKey)
      blockGs = blockData.generationSignature.arr
      _ <- Either.cond(
        calcGs.sameElements(blockGs),
        (),
        GenericError(
          s"declared generation signature ${blockData.generationSignature.base58} does not match calculated generation signature ${ByteStr(calcGs).base58}")
      )
      effectiveBalance <- genBalance(height).left.map(GenericError(_))
      hit    = pos.hit(blockGs)
      target = pos.target(parent.timestamp, parent.consensusData.baseTarget, blockTime, effectiveBalance)
      _ <- Either.cond(
        hit < target || (height == height1 && block.uniqueId == correctBlockId1) || (height == height2 && block.uniqueId == correctBlockId2),
        (),
        GenericError(s"calculated hit $hit >= calculated target $target")
      )
    } yield ()

    r.left.map {
      case GenericError(x) => GenericError(s"Block $block is invalid: $x")
      case x               => x
    }
  }
}
