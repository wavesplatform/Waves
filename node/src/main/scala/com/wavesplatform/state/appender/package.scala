package com.wavesplatform.state

import cats.syntax.either.*
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.block.{Block, BlockSnapshot}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.{GeneratingBalanceProvider, PoSSelector}
import com.wavesplatform.crypto.bls.BlsPublicKey
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

  def findBlockAndGetGenerators(
      blockchain: Blockchain,
      block: Block
  ): Either[ValidationError, (parentHeight: Height, generatorBalances: GeneratorBalances, eligibleGenerators: Set[Address])] =
    for {
      parentHeight <- blockchain
        .heightOf(block.header.reference)
        .toRight(GenericError(s"height: history does not contain parent ${block.header.reference}"))
    } yield {
      val blockHeight         = Height(parentHeight + 1)
      val period              = blockchain.generationPeriodOf(blockHeight)
      val committedGenerators = blockchain.committedGenerators(period)
      val generatorBalances   = getGeneratorBalances(blockchain, block, committedGenerators)
      val eligibleGenerators = generatorBalances.view.collect {
        case (addr, _, balance) if blockchain.isEffectiveBalanceValid(parentHeight, block, balance) => addr
      }.toSet
      (Height(parentHeight), generatorBalances, eligibleGenerators)
    }

  private[appender] def appendKeyBlock(
      blockchainUpdater: BlockchainUpdater & Blockchain,
      utx: UtxPool,
      pos: PoSSelector,
      time: Time,
      log: LoggerFacade,
      verify: Boolean,
      txSignParCheck: Boolean
  )(block: Block, snapshot: Option[BlockSnapshotResponse]): Either[ValidationError, BlockApplyResult] =
    for {
      data <- findBlockAndGetGenerators(blockchainUpdater, block)
      hitSource <-
        if (verify) validateBlock(blockchainUpdater, pos, time, data.eligibleGenerators)(block, data.parentHeight)
        else pos.validateGenerationSignature(block)
      applyResult <-
        metrics.appendBlock
          .measureSuccessful(
            blockchainUpdater
              .processBlock(
                block,
                hitSource,
                snapshot.map(responseToSnapshot(block, blockchainUpdater.height + 1)),
                data.generatorBalances,
                challengedHitSource = None,
                verify,
                txSignParCheck
              )
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
      blockchainUpdater: BlockchainUpdater & Blockchain,
      pos: PoSSelector,
      time: Time,
      verify: Boolean,
      txSignParCheck: Boolean
  )(block: Block, snapshot: Option[BlockSnapshotResponse]): Either[ValidationError, (BlockApplyResult, Int)] = {
    if (block.header.challengedHeader.nonEmpty) {
      processBlockWithChallenge(blockchainUpdater, pos, time, verify, txSignParCheck)(block, snapshot)
    } else {
      for {
        data <- findBlockAndGetGenerators(blockchainUpdater, block)
        hitSource <-
          if (verify) validateBlock(blockchainUpdater, pos, time, data.eligibleGenerators)(block, data.parentHeight)
          else pos.validateGenerationSignature(block)
        applyResult <- metrics.appendBlock.measureSuccessful(
          blockchainUpdater.processBlock(
            block,
            hitSource,
            snapshot.map(responseToSnapshot(block, blockchainUpdater.height + 1)),
            data.generatorBalances,
            challengedHitSource = None,
            verify,
            txSignParCheck
          )
        )
      } yield applyResult -> blockchainUpdater.height
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
      blockchainUpdater: BlockchainUpdater & Blockchain,
      pos: PoSSelector,
      time: Time,
      verify: Boolean,
      txSignParCheck: Boolean
  )(block: Block, snapshot: Option[BlockSnapshotResponse]): Either[ValidationError, (BlockApplyResult, Int)] = {
    val challengedBlock = block.toOriginal
    for {
      data <- findBlockAndGetGenerators(blockchainUpdater, challengedBlock)

      challengedHitSource <-
        if (verify) validateBlock(blockchainUpdater, pos, time, data.eligibleGenerators)(challengedBlock, data.parentHeight)
        else pos.validateGenerationSignature(challengedBlock)

      hitSource <-
        if (verify) validateBlock(blockchainUpdater, pos, time, data.eligibleGenerators)(block, data.parentHeight)
        else pos.validateGenerationSignature(block)

      applyResult <-
        metrics.appendBlock
          .measureSuccessful(
            blockchainUpdater.processBlock(
              block,
              hitSource,
              snapshot.map(responseToSnapshot(block, blockchainUpdater.height + 1)),
              data.generatorBalances,
              Some(challengedHitSource),
              verify,
              txSignParCheck
            )
          )
    } yield applyResult -> blockchainUpdater.height
  }

  private def getGeneratorBalances(
      blockchain: Blockchain,
      newBlock: Block,
      generators: Seq[(Address, BlsPublicKey, TransactionId)]
  ): GeneratorBalances = {
    val parentBlockId = newBlock.header.reference
    generators.map { case (addr, blsPk, _) =>
      val balance = GeneratingBalanceProvider.unchallengedBalance(blockchain, addr, Some(parentBlockId))
      (addr, blsPk, balance)
    }
  }

  /** @return
    *   Hit source
    */
  private def validateBlock(blockchainUpdater: Blockchain, pos: PoSSelector, time: Time, eligibleGenerators: Set[Address])(
      block: Block,
      parentHeight: Height
  ): Either[ValidationError, ByteStr] =
    for {
      _ <- Miner.isAllowedForMining(block.sender.toAddress, blockchainUpdater).leftMap(BlockAppendError(_, block))
      r <- blockConsensusValidation(blockchainUpdater, pos, time.correctedTime(), eligibleGenerators)(block, parentHeight)
      _ <- validateStateHash(block, blockchainUpdater)
      _ <- validateChallengedHeader(block, blockchainUpdater)
    } yield r

  private def blockConsensusValidation(blockchain: Blockchain, pos: PoSSelector, currentTs: Long, eligibleGenerators: Set[Address])(
      block: Block,
      parentHeight: Height
  ): Either[ValidationError, ByteStr] =
    metrics.blockConsensusValidation
      .measureSuccessful {
        val blockTime = block.header.timestamp
        val miner     = block.sender.toAddress

        for {
          parent <- blockchain.parentHeader(block.header).toRight(GenericError(s"parent: history does not contain parent ${block.header.reference}"))
          grandParent = blockchain.parentHeader(parent, 2)

          // If no one commited, fallback to a classic
          _ <- Either.raiseUnless(eligibleGenerators.isEmpty || eligibleGenerators.contains(miner)) {
            GenericError(s"$miner is not allowed to mine, allowed: ${eligibleGenerators.mkString(", ")}")
          }

          minerBalance <- minerBalance(blockchain, miner, parentHeight, block).leftMap(GenericError(_))
          _            <- validateBlockVersion(parentHeight, block, blockchain)
          _            <- Either.cond(blockTime - currentTs < MaxTimeDrift, (), BlockFromFuture(blockTime, currentTs))
          _            <- pos.validateBaseTarget(parentHeight, block, parent, grandParent)
          hitSource    <- pos.validateGenerationSignature(block)
          _ <- pos
            .validateBlockDelay(parentHeight, block.header, parent, minerBalance)
            .leftFlatMap(checkExceptions(parentHeight, block, _))
        } yield hitSource
      }
      .left
      .map {
        case GenericError(x) => GenericError(s"Block $block is invalid: $x")
        case x               => x
      }

  private def minerBalance(blockchain: Blockchain, minerAddress: Address, parentHeight: Height, block: Block): Either[String, Long] = {
    val parentBlockId = block.header.reference
    val balance       = blockchain.generatingBalance(minerAddress, Some(parentBlockId))

    if (blockchain.isEffectiveBalanceValid(parentHeight, block, balance))
      Either.right(
        balance + block.header.challengedHeader.map(ch => blockchain.generatingBalance(ch.generator.toAddress, Some(parentBlockId))).getOrElse(0L)
      )
    else if (minerAddress == block.sender.toAddress) Either.left(s"generator's effective balance $balance is less that required for generation")
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
