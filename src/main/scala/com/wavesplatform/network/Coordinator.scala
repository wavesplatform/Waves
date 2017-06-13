package com.wavesplatform.network

import cats._
import cats.implicits._
import com.wavesplatform.mining.Miner
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.local.LocalChannel
import io.netty.channel.{Channel, ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.consensus.TransactionsOrdering
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError.GenericError
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

import scala.concurrent.duration._
import scala.util.control.NonFatal

@Sharable
class Coordinator(
    checkpoints: CheckpointService,
    history: History,
    blockchainUpdater: BlockchainUpdater,
    time: Time,
    stateReader: StateReader,
    utxStorage: UnconfirmedTransactionsStorage,
    settings: BlockchainSettings,
    checkpointPublicKey: String,
    miner: Miner,
    blacklist: Channel => Unit,
    broadcast: (AnyRef, Option[Channel]) => Unit)
  extends ChannelInboundHandlerAdapter with ScorexLogging {
  import Coordinator._

  private def isValidWithRespectToCheckpoint(candidate: Block, estimatedHeight: Int): Boolean =
    !checkpoints.get.exists {
      case Checkpoint(items, _) =>
        val blockSignature = candidate.signerData.signature
        items.exists { case BlockCheckpoint(h, sig) =>
          h == estimatedHeight && blockSignature != ByteStr(sig)
        }
    }

  private def validateWithRespectToCheckpoint(candidate: Block, estimatedHeight: Int): Either[ValidationError, Unit] = {
    if (isValidWithRespectToCheckpoint(candidate, estimatedHeight))
      Right(())
    else
      Left(GenericError(s"Block ${str(candidate)} [h = $estimatedHeight] is not valid with respect to checkpoint"))
  }

  def isBlockValid(b: Block): Either[ValidationError, Unit] = {
    if (history.contains(b)) Right(())
    else {
      def historyContainsParent = history.contains(b.reference)
      def consensusDataIsValid = blockConsensusValidation(history, stateReader, settings, time)(b)

      if (!historyContainsParent) Left(GenericError(s"Invalid block ${b.encodedId}: no parent block in history"))
      else if (!consensusDataIsValid) Left(GenericError(s"Invalid block ${b.encodedId}: consensus data is not valid"))
      else Right(())
    }
  }

  private def processNewBlock(block: Block): Either[ValidationError, Unit] = for {
    _ <- validateWithRespectToCheckpoint(block, history.height() + 1)
    _ <- isBlockValid(block)
    _ <- blockchainUpdater.processBlock(block)
  } yield {
    block.transactionData.foreach(utxStorage.remove)
    UnconfirmedTransactionsStorage.clearIncorrectTransactions(settings.functionalitySettings,
      stateReader, utxStorage, time)
  }

  private def str(block: Block) = {
    if (log.logger.isTraceEnabled) block.json
    else block.uniqueId + ", parent " + block.reference
  }

  private def processFork(ctx: ChannelHandlerContext, lastCommonBlockId: BlockId, blocks: Iterator[Block]): Unit = {
    val newBlocks = blocks.toSeq

    def isForkValidWithCheckpoint(lastCommonHeight: Int): Boolean = {
      newBlocks.zipWithIndex.forall(p => isValidWithRespectToCheckpoint(p._1, lastCommonHeight + 1 + p._2))
    }

    if (history.heightOf(lastCommonBlockId).exists(isForkValidWithCheckpoint)) {
      blockchainUpdater.removeAfter(lastCommonBlockId)

      foldM[({type l[α] = Either[(ValidationError, BlockId), α]})#l, List, Block, Unit](newBlocks.toList, ()) { case ((), block: Block) => processNewBlock(block).left.map((_, block.uniqueId)) } match {
        case Right(_) =>
        case Left(err) =>
          log.error(s"Can't processFork(lastBlockCommonId: $lastCommonBlockId because: ${err._1}")
          if (history.lastBlock.uniqueId == err._2) {
//            from.foreach(_.blacklist())
          }
      }

      ctx.writeAndFlush(LocalScoreChanged(history.score()))
      miner.lastBlockChanged(history.height(), history.lastBlock)
      checkExpiry(ctx)
    } else {
//      from.foreach(_.blacklist())
      log.warn(s"${id(ctx)} Fork contains block that doesn't match checkpoint, declining fork")
    }
  }

  private def processSingleBlock(ctx: ChannelHandlerContext, newBlock: Block, from: Option[Channel]): Unit = {
    val parentBlockId = newBlock.reference
    val local = from.isEmpty

    val isBlockToBeAdded = try {
      if (history.contains(newBlock)) {
        // we have already got the block - skip
        false
      } else if (history.contains(parentBlockId)) {

        val lastBlock = history.lastBlock

        if (lastBlock.uniqueId != parentBlockId) {
          // someone has happened to be faster and already added a block or blocks after the parent
          log.debug(s"A child for parent of the block already exists, local=$local: ${str(newBlock)}")

          val cmp = PoSCalc.blockOrdering(history, stateReader, settings.functionalitySettings)
          if (lastBlock.reference == parentBlockId && cmp.lt(lastBlock, newBlock)) {
            log.debug(s"New block ${str(newBlock)} is better than last ${str(lastBlock)}")
          }

          false

        } else true

      } else {
        // the block either has come too early or, if local, too late (e.g. removeAfter() has come earlier)
        log.debug(s"${id(ctx)} Parent of the block is not in the history, local=$local: ${str(newBlock)}")
        false
      }
    } catch {
      case e: UnsupportedOperationException =>
        log.debug(s"DB can't find last block because of unexpected modification")
        false
    }

    if (isBlockToBeAdded) {
      log.info(s"${id(ctx)} New ${if (local) "local" else "foreign"} block: ${str(newBlock)}")
      processNewBlock(newBlock) match {
        case Right(_) =>
          miner.lastBlockChanged(history.height(), newBlock)
          ctx.writeAndFlush(LocalScoreChanged(history.score()))
          if (local) {
//            network.broadcast(newBlock)
          } else {
//            self ! BroadcastCurrentScore
          }
        case Left(err) =>
//          from.foreach(_.blacklist())
          log.warn(s"Can't apply single block, local=$local: ${str(newBlock)}")
      }
    }
  }

  private def handleCheckpoint(checkpoint: Checkpoint, from: Option[Channel]): Unit =
    if (checkpoints.get.forall(c => !(c.signature sameElements checkpoint.signature))) {
      val maybePublicKeyBytes = Base58.decode(checkpointPublicKey).toOption

      maybePublicKeyBytes foreach {
        publicKey =>
          if (EllipticCurveImpl.verify(checkpoint.signature, checkpoint.toSign, publicKey)) {
            checkpoints.set(Some(checkpoint))
            broadcast(checkpoint, from)
            makeBlockchainCompliantWith(checkpoint)
          } else {
            from.filterNot(_.isInstanceOf[LocalChannel]).foreach(blacklist(_))
          }
      }
    }

  private def makeBlockchainCompliantWith(checkpoint: Checkpoint): Unit = {
    val existingItems = checkpoint.items.filter {
      checkpoint => history.blockAt(checkpoint.height).isDefined
    }

    val fork = existingItems.takeWhile {
      case BlockCheckpoint(h, sig) =>
        val block = history.blockAt(h).get
        block.signerData.signature != ByteStr(sig)
    }

    if (fork.nonEmpty) {
      val genesisBlockHeight = 1
      val hh = existingItems.map(_.height) :+ genesisBlockHeight
      history.blockAt(hh(fork.size)).foreach {
        lastValidBlock =>
          log.warn(s"Fork detected (length = ${fork.size}), rollback to last valid block id [${lastValidBlock.encodedId}]")
          blockchainUpdater.removeAfter(lastValidBlock.uniqueId)
      }
    }
  }

  private def checkExpiry(ctx: ChannelHandlerContext): Unit = {
    if ((time.correctedTime() - history.lastBlock.timestamp).millis > 1.hour) {
      ctx.writeAndFlush(BlockchainExpired)
    } else {
      ctx.writeAndFlush(BlockchainUpdated)
    }
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case c: Checkpoint => handleCheckpoint(c, Some(ctx.channel()))
    case ExtensionBlocks(blocks) =>
      log.debug(s"${id(ctx)} Processing fork")
      processFork(ctx, blocks.head.reference, blocks.iterator)
      log.debug(s"${id(ctx)} Finished processing fork, local score is ${history.score()}")
    case b: Block =>
      processSingleBlock(ctx, b, Some(ctx.channel()))
    case RollbackTo(blockId) =>
      blockchainUpdater.removeAfter(blockId)
      ctx.writeAndFlush(LocalScoreChanged(history.score()))
    case _: Handshake =>
      ctx.writeAndFlush(history.score())
    case other => log.debug(other.getClass.getCanonicalName)
  }
}

object Coordinator extends ScorexLogging {
  def foldM[G[_], F[_], A, B](fa: F[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G], F: Traverse[F]): G[B] =
    F.foldLeft(fa, G.pure(z))((gb, a) => G.flatMap(gb)(f(_, a)))

  val MaxTimeDrift: FiniteDuration = 15.seconds

  def blockConsensusValidation(history: History, state: StateReader, bcs: BlockchainSettings, time: Time)(block: Block): Boolean = try {

    import PoSCalc._

    val fs = bcs.functionalitySettings

    val blockTime = block.timestamp

    require((blockTime - time.correctedTime()).millis < MaxTimeDrift, s"Block timestamp $blockTime is from future")

    if (blockTime > fs.requireSortedTransactionsAfter) {
      require(block.transactionData.sorted(TransactionsOrdering.InBlock) == block.transactionData, "Transactions must be sorted correctly")
    }

    val parentOpt = history.parent(block)
    require(parentOpt.isDefined || history.height() == 1,
      s"Can't find parent ${block.reference} of ${block.uniqueId}")

    val parent = parentOpt.get
    val parentHeightOpt = history.heightOf(parent.uniqueId)
    require(parentHeightOpt.isDefined, s"Can't get height of ${block.reference}")
    val parentHeight = parentHeightOpt.get

    val prevBlockData = parent.consensusData
    val blockData = block.consensusData

    val cbt = calcBaseTarget(bcs.genesisSettings.averageBlockDelay, parentHeight, parent, history.parent(parent, 2), blockTime)
    val bbt = blockData.baseTarget
    require(cbt == bbt, s"Declared baseTarget $bbt of ${block.uniqueId} does not match calculated baseTarget $cbt")

    val generator = block.signerData.generator

    //check generation signature
    val calcGs = calcGeneratorSignature(prevBlockData, generator)
    val blockGs = blockData.generationSignature
    require(calcGs.sameElements(blockGs),
      s"Declared signature ${blockGs.mkString} of ${block.uniqueId} does not match calculated signature ${calcGs.mkString}")

    val effectiveBalance = generatingBalance(state, fs)(generator, parentHeight)

    if (blockTime >= fs.minimalGeneratingBalanceAfterTimestamp) {
      require(effectiveBalance >= MinimalEffectiveBalanceForGenerator, s"Effective balance $effectiveBalance is less that minimal ($MinimalEffectiveBalanceForGenerator)")
    }

    //check hit < target
    calcHit(prevBlockData, generator) < calcTarget(parent, blockTime, effectiveBalance)
  } catch {
    case e: IllegalArgumentException =>
      log.error("Error while checking a block", e)
      false
    case NonFatal(t) =>
      log.error("Fatal error while checking a block", t)
      throw t
  }
}
