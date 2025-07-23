package com.wavesplatform.state

import cats.syntax.either.*
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.block.{Block, BlockSnapshot}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.*
import com.wavesplatform.mining.Miner
import com.wavesplatform.network.BlockSnapshotResponse
import com.wavesplatform.protobuf.PBSnapshots
import com.wavesplatform.state
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult.Applied
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.TxValidationError.{BlockAppendError, BlockFromFuture, GenericError}
import com.wavesplatform.utils.{LoggerFacade, Time}
import com.wavesplatform.utx.UtxPool
import kamon.Kamon

package object appender {

  val MaxTimeDrift: Long = 100 // millis

  // Invalid blocks, that are already in blockchain
  private val exceptions = List(
    812608 -> ByteStr.decodeBase58("2GNCYVy7k3kEPXzz12saMtRDeXFKr8cymVsG8Yxx3sZZ75eHj9csfXnGHuuJe7XawbcwjKdifUrV1uMq4ZNCWPf1").get,
    813207 -> ByteStr.decodeBase58("5uZoDnRKeWZV9Thu2nvJVZ5dBvPB7k2gvpzFD618FMXCbBVBMN2rRyvKBZBhAGnGdgeh2LXEeSr9bJqruJxngsE7").get
  )

  private def responseToSnapshot(block: Block, height: Int)(s: BlockSnapshotResponse): BlockSnapshot =
    BlockSnapshot(
      block.id(),
      block.transactionData.zip(s.snapshots).map { case (tx, pbs) => PBSnapshots.fromProtobuf(pbs, tx.id(), height) }
    )

  def getCommittedGeneratorsAndParentHeight(
      blockchain: Blockchain,
      block: Block
  ): Either[ValidationError, (parentHeight: Height, committedGenerators: Set[Address])] =
    for {
      parentHeight <- blockchain
        .heightOf(block.header.reference)
        .toRight(GenericError(s"height: history does not contain parent ${block.header.reference}"))
    } yield (Height(parentHeight), blockchain.committedGenerators(Height(parentHeight + 1)).keySet.map(_.toAddress))

  private[appender] def appendKeyBlock(
      blockchain: BlockchainUpdater & Blockchain,
      utx: UtxPool,
      pos: PoSSelector,
      time: Time,
      log: LoggerFacade,
      verify: Boolean,
      txSignParCheck: Boolean
  )(block: Block, snapshot: Option[BlockSnapshotResponse]): Either[ValidationError, BlockApplyResult] =
    for {
      data <- getCommittedGeneratorsAndParentHeight(blockchain, block)
      (hitSource, gb) <-
        if (verify) validateBlockAndReturnBalances(blockchain, pos, time, data.committedGenerators)(block, data.parentHeight)
        else validateGenerationSignature(blockchain, pos, data.committedGenerators)(block, data.parentHeight)
      applyResult <-
        metrics.appendBlock
          .measureSuccessful(
            blockchain
              .processBlock(block, hitSource, snapshot.map(responseToSnapshot(block, blockchain.height + 1)), gb, None, verify, txSignParCheck)
          )
          .map {
            case res @ Applied(discardedDiffs, _) =>
              // TODO: move UTX cleanup from appender
              if (block.transactionData.nonEmpty) {
                utx.removeAll(block.transactionData)
                log.trace(
                  s"Removing txs of ${block.id()} ${block.transactionData.map(_.id()).mkString("(", ", ", ")")} from UTX pool"
                )
              }
              utx.setPrioritySnapshots(discardedDiffs)
              utx.scheduleCleanup()
              res
            case res => res
          }
    } yield applyResult

  private[appender] def appendExtensionBlock(
      blockchain: BlockchainUpdater & Blockchain,
      pos: PoSSelector,
      time: Time,
      verify: Boolean,
      txSignParCheck: Boolean
  )(block: Block, snapshot: Option[BlockSnapshotResponse]): Either[ValidationError, (BlockApplyResult, Int)] = {
    if (block.header.challengedHeader.nonEmpty) {
      processBlockWithChallenge(blockchain, pos, time, verify, txSignParCheck)(block, snapshot)
    } else {
      for {
        data <- getCommittedGeneratorsAndParentHeight(blockchain, block)
        (hitSource, gb) <-
          if (verify) validateBlockAndReturnBalances(blockchain, pos, time, data.committedGenerators)(block, data.parentHeight)
          else validateGenerationSignature(blockchain, pos, data.committedGenerators)(block, data.parentHeight)
        applyResult <- metrics.appendBlock.measureSuccessful(
          blockchain.processBlock(
            block,
            hitSource,
            snapshot.map(responseToSnapshot(block, blockchain.height + 1)),
            gb,
            None,
            verify,
            txSignParCheck
          )
        )
      } yield applyResult -> blockchain.height
    }
  }

  private[appender] def appendChallengeBlock(
      blockchainUpdater: BlockchainUpdater & Blockchain,
      utx: UtxPool,
      pos: PoSSelector,
      time: Time,
      log: LoggerFacade,
      verify: Boolean,
      txSignParCheck: Boolean
  )(block: Block, snapshot: Option[BlockSnapshotResponse]): Either[ValidationError, BlockApplyResult] =
    processBlockWithChallenge(blockchainUpdater, pos, time, verify, txSignParCheck)(block, snapshot).map {
      case (res @ Applied(discardedDiffs, _), _) =>
        if (block.transactionData.nonEmpty) {
          utx.removeAll(block.transactionData)
          log.trace(
            s"Removing txs of ${block.id()} ${block.transactionData.map(_.id()).mkString("(", ", ", ")")} from UTX pool"
          )
        }
        utx.setPrioritySnapshots(discardedDiffs)
        utx.scheduleCleanup()
        res
      case (res, _) => res
    }

  private def processBlockWithChallenge(
      blockchain: BlockchainUpdater & Blockchain,
      pos: PoSSelector,
      time: Time,
      verify: Boolean,
      txSignParCheck: Boolean
  )(block: Block, snapshot: Option[BlockSnapshotResponse]): Either[ValidationError, (BlockApplyResult, Int)] = {
    val challengedBlock = block.toOriginal
    for {
      data <- getCommittedGeneratorsAndParentHeight(blockchain, challengedBlock)
      challengedHitSource <-
        if (verify) validateBlock(blockchain, pos, time, data.committedGenerators)(challengedBlock, data.parentHeight).map(_.hitSource)
        else pos.validateGenerationSignature(challengedBlock)

      (hitSource, gb) <-
        if (verify) validateBlockAndReturnBalances(blockchain, pos, time, data.committedGenerators)(block, data.parentHeight)
        else validateGenerationSignature(blockchain, pos, data.committedGenerators)(block, data.parentHeight)

      applyResult <-
        metrics.appendBlock
          .measureSuccessful(
            blockchain.processBlock(
              block,
              hitSource,
              snapshot.map(responseToSnapshot(block, blockchain.height + 1)),
              gb,
              Some(challengedHitSource),
              verify,
              txSignParCheck
            )
          )
    } yield applyResult -> blockchain.height
  }

  /** @param parentHeight
    *   Of newBlock. Generator balances must be taken before a block application.
    * @return
    */
  def generatorBalances(
      blockchain: Blockchain,
      parentHeight: Height,
      newBlock: Block,
      generators: Iterable[Address]
  ): Either[ValidationError, GeneratorBalances] =
    generators
      .foldLeft(Map.empty[Address, Long].asRight[String]) {
        case (Right(r), generator) =>
          for {
            b <- genBalance(blockchain, generator, parentHeight, newBlock)
          } yield r.updated(generator, b)

        case (r, _) => r
      }
      .left
      .map(GenericError(_))

  private def validateGenerationSignature(blockchain: Blockchain, pos: PoSSelector, committedGenerators: Set[Address])(
      block: Block,
      parentHeight: Height
  ): Either[ValidationError, (ByteStr, GeneratorBalances)] =
    for {
      hitSource <- pos.validateGenerationSignature(block)
      xs        <- generatorBalances(blockchain, parentHeight, block, committedGenerators)
    } yield (hitSource, xs)

  private def validateBlockAndReturnBalances(blockchainUpdater: Blockchain, pos: PoSSelector, time: Time, committedGenerators: Set[Address])(
      block: Block,
      parentHeight: Height
  ): Either[ValidationError, (ByteStr, GeneratorBalances)] = {
    for {
      (hitSource, minerBalance) <- validateBlock(blockchainUpdater, pos, time, committedGenerators)(block, parentHeight)
      generatorBalances         <- generatorBalances(blockchainUpdater, parentHeight, block, committedGenerators - block.sender.toAddress)
    } yield (hitSource, generatorBalances.updated(block.sender.toAddress, minerBalance))
  }

  private def validateBlock(blockchainUpdater: Blockchain, pos: PoSSelector, time: Time, committedGenerators: Set[Address])(
      block: Block,
      parentHeight: Height
  ): Either[ValidationError, (hitSource: ByteStr, minerBalance: Long)] = {
    val blockSenderAddress = block.sender.toAddress
    for {
      _ <- Miner.isAllowedForMining(blockSenderAddress, blockchainUpdater).leftMap(BlockAppendError(_, block))
      r <- blockConsensusValidation(blockchainUpdater, pos, time.correctedTime(), committedGenerators)(block, parentHeight)
      _ <- validateStateHash(block, blockchainUpdater)
      _ <- validateChallengedHeader(block, blockchainUpdater)
    } yield r
  }

  private def blockConsensusValidation(blockchain: Blockchain, pos: PoSSelector, currentTs: Long, committedGenerators: Set[Address])(
      block: Block,
      parentHeight: Height
  ): Either[ValidationError, (hitSource: ByteStr, minerBalance: Long)] =
    metrics.blockConsensusValidation
      .measureSuccessful {
        val blockTime = block.header.timestamp
        val miner     = block.sender.toAddress

        for {
          parent <- blockchain.parentHeader(block.header).toRight(GenericError(s"parent: history does not contain parent ${block.header.reference}"))
          grandParent = blockchain.parentHeader(parent, 2)

          // If no one commited, fallback to a classic
          _ <- Either.raiseUnless(committedGenerators.isEmpty || committedGenerators.contains(miner)) {
            GenericError(s"$miner is not allowed to mine, allowed: ${committedGenerators.mkString(", ")}")
          }

          minerBalance <- genBalance(blockchain, miner, parentHeight, block).leftMap(GenericError(_)) // Fail fast
          _            <- validateBlockVersion(parentHeight, block, blockchain)
          _            <- Either.cond(blockTime - currentTs < MaxTimeDrift, (), BlockFromFuture(blockTime, currentTs))
          _            <- pos.validateBaseTarget(parentHeight, block, parent, grandParent)
          hitSource    <- pos.validateGenerationSignature(block)
          _ <- pos
            .validateBlockDelay(parentHeight, block.header, parent, minerBalance)
            .leftFlatMap(checkExceptions(parentHeight, block, _))
        } yield (hitSource, minerBalance)
      }
      .left
      .map {
        case GenericError(x) => GenericError(s"Block $block is invalid: $x")
        case x               => x
      }

  private def genBalance(blockchain: Blockchain, generatorAddress: Address, parentHeight: Height, block: Block): Either[String, Long] = {
    val parentBlockId = block.header.reference
    val balance       = blockchain.generatingBalance(generatorAddress, Some(parentBlockId))

    if (blockchain.isEffectiveBalanceValid(parentHeight, block, balance))
      Either.right(
        balance + block.header.challengedHeader.map(ch => blockchain.generatingBalance(ch.generator.toAddress, Some(parentBlockId))).getOrElse(0L)
      )
    else if (generatorAddress == block.sender.toAddress) Either.left(s"generator's effective balance $balance is less that required for generation")
    else Either.right(0L) // Ignore for a regular generator, not a miner
  }

  private def checkExceptions(height: Int, block: Block, origError: ValidationError): Either[ValidationError, Unit] =
    Either.raiseUnless(exceptions.contains((height, block.id())))(origError)

  private def validateBlockVersion(parentHeight: Int, block: Block, blockchain: Blockchain): Either[ValidationError, Unit] = {
    Either.cond(
      blockchain.blockVersionAt(parentHeight + 1) == block.header.version,
      (),
      GenericError(s"Block version should be equal to ${blockchain.blockVersionAt(parentHeight + 1)}")
    )
  }

  private def validateChallengedHeader(block: Block, blockchain: Blockchain): Either[ValidationError, Unit] =
    for {
      _ <- Either.cond(
        block.header.challengedHeader.isEmpty || blockchain.supportsLightNodeBlockFields(blockchain.height + 1),
        (),
        BlockAppendError("Challenged header is not supported yet", block)
      )
      _ <- Either.cond(
        !block.header.challengedHeader.map(_.generator).contains(block.header.generator),
        (),
        BlockAppendError("Challenged block generator and challenging block generator should not be equal", block)
      )
    } yield ()

  private def validateStateHash(block: Block, blockchain: Blockchain): Either[ValidationError, Unit] =
    Either.cond(
      block.header.stateHash.isEmpty || blockchain.supportsLightNodeBlockFields(blockchain.height + 1),
      (),
      BlockAppendError("Block state hash is not supported yet", block)
    )

  private object metrics {
    val blockConsensusValidation = Kamon.timer("block-appender.block-consensus-validation").withoutTags()
    val appendBlock              = Kamon.timer("block-appender.blockchain-append-block").withoutTags()
  }

}
