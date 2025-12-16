package com.wavesplatform.state

import cats.instances.seq.*
import cats.syntax.either.*
import cats.syntax.traverse.*
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.block.{Block, BlockEndorsement, BlockSnapshot}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.{GeneratingBalanceProvider, PoSSelector}
import com.wavesplatform.crypto.bls.{BlsPublicKey, BlsUtils}
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
    Height(812608) -> ByteStr.decodeBase58("2GNCYVy7k3kEPXzz12saMtRDeXFKr8cymVsG8Yxx3sZZ75eHj9csfXnGHuuJe7XawbcwjKdifUrV1uMq4ZNCWPf1").get,
    Height(813207) -> ByteStr.decodeBase58("5uZoDnRKeWZV9Thu2nvJVZ5dBvPB7k2gvpzFD618FMXCbBVBMN2rRyvKBZBhAGnGdgeh2LXEeSr9bJqruJxngsE7").get
  )

  private def responseToSnapshot(block: Block, height: Height)(s: BlockSnapshotResponse): BlockSnapshot =
    BlockSnapshot(
      block.id(),
      block.transactionData.zip(s.snapshots).map { case (tx, pbs) => PBSnapshots.fromProtobuf(pbs, tx.id(), height) }
    )

  /** @return generatorBalances before newBlock
    */
  def findBlockAndGetGenerators(
      blockchain: Blockchain,
      newBlock: Block
  ): Either[ValidationError, (parentHeight: Height, generatorBalances: GeneratorBalances)] = {
    val parentBlockId = newBlock.header.reference
    val r = for {
      parentHeight <- blockchain
        .heightOf(parentBlockId)
        .map(Height(_))
        .toRight(s"height: history does not contain parent $parentBlockId")

      blockHeight   = parentHeight.next
      currentPeriod = blockchain.generationPeriodOf(blockHeight)
      minerAddress  = newBlock.header.generator.toAddress

      conflictGenerators = currentPeriod.fold(ConflictGenerators.empty)(blockchain.conflictGenerators).upTo(blockHeight)
      validGenerators = currentPeriod
        .fold(Nil)(blockchain.committedGenerators)
        .view
        .map { case v @ (address, _) => v -> GeneratingBalanceProvider.balance(blockchain, address) }
        .zipWithIndex
        .collect {
          case (((address, blsPk), balance), idx)
              if !conflictGenerators.contains(GeneratorIndex(idx)) && blockchain.isGeneratingBalanceValid(
                parentHeight.toInt,
                newBlock,
                balance
              ) =>
            GeneratorInfo(GeneratorIndex(idx), address, blsPk, balance)
        }
        .toSeq

      generatorSet = validGenerators.view.map(_.address).toSet

      // If no one commited, fallback to classic
      _ <- Either.raiseWhen(validGenerators.nonEmpty && !generatorSet.contains(minerAddress)) {
        s"$minerAddress is not allowed to generate a block, allowed: ${generatorSet.mkString(", ")}. " +
          s"If this is your node: commit to generation for the next period"
      }
    } yield (parentHeight, validGenerators)

    r.leftMap(GenericError(_))
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
      (hitSource, balances) <-
        if (verify) validateBlock(blockchainUpdater, pos, time, data.generatorBalances)(block, data.parentHeight)
        else pos.validateGenerationSignature(block).map(_ -> Seq.empty)
      applyResult <-
        metrics.appendBlock
          .measureSuccessful(
            blockchainUpdater
              .processBlock(
                block,
                hitSource,
                snapshot.map(responseToSnapshot(block, Height(blockchainUpdater.height + 1))),
                balances,
                challengedHitSource = None,
                verify,
                txSignParCheck
              )
          )
          .map {
            case res @ Applied(discardedDiffs = discardedDiffs) =>
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
        (hitSource, balances) <-
          if (verify) validateBlock(blockchainUpdater, pos, time, data.generatorBalances)(block, data.parentHeight)
          else pos.validateGenerationSignature(block).map(_ -> Seq.empty)
        applyResult <- metrics.appendBlock.measureSuccessful(
          blockchainUpdater.processBlock(
            block,
            hitSource,
            snapshot.map(responseToSnapshot(block, Height(blockchainUpdater.height + 1))),
            balances,
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
      case (res @ Applied(discardedDiffs = discardedDiffs), _) =>
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

      (challengedHitSource, _) <-
        if (verify) validateBlock(blockchainUpdater, pos, time, data.generatorBalances)(challengedBlock, data.parentHeight)
        else pos.validateGenerationSignature(challengedBlock).map(_ -> Seq.empty)

      (hitSource, balances) <-
        if (verify) validateBlock(blockchainUpdater, pos, time, data.generatorBalances)(block, data.parentHeight)
        else pos.validateGenerationSignature(block).map(_ -> Seq.empty)

      applyResult <-
        metrics.appendBlock
          .measureSuccessful(
            blockchainUpdater.processBlock(
              block,
              hitSource,
              snapshot.map(responseToSnapshot(block, Height(blockchainUpdater.height + 1))),
              balances,
              Some(challengedHitSource),
              verify,
              txSignParCheck
            )
          )
    } yield applyResult -> blockchainUpdater.height
  }

  /** @return
    *   Hit source
    */
  private def validateBlock(blockchainUpdater: Blockchain, pos: PoSSelector, time: Time, generatorBalances: GeneratorBalances)(
      block: Block,
      parentHeight: Height
  ): Either[ValidationError, (ByteStr, GeneratorBalances)] =
    for {
      _ <- Miner.isAllowedForMining(block.sender.toAddress, blockchainUpdater).leftMap(BlockAppendError(_, block))
      r <- blockConsensusValidation(blockchainUpdater, pos, time.correctedTime())(block, parentHeight)
      _ <- validateStateHash(block, blockchainUpdater)
      _ <- validateChallengedHeader(block, blockchainUpdater)
      b <- validateFinalizationVoting(block, blockchainUpdater, generatorBalances)
    } yield (r, b)

  private def blockConsensusValidation(blockchain: Blockchain, pos: PoSSelector, currentTs: Long)(
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

          minerBalance <- minerBalance(blockchain, miner, parentHeight, block).leftMap(GenericError(_))
          _            <- validateBlockVersion(parentHeight.toInt, block, blockchain)
          _            <- Either.cond(blockTime - currentTs < MaxTimeDrift, (), BlockFromFuture(blockTime, currentTs))
          _            <- pos.validateBaseTarget(parentHeight.toInt, block, parent, grandParent)
          hitSource    <- pos.validateGenerationSignature(block)
          _ <- pos
            .validateBlockDelay(parentHeight.toInt, block.header, parent, minerBalance)
            .leftFlatMap(checkExceptions(parentHeight, block, _))
        } yield hitSource
      }
      .left
      .map {
        case GenericError(x) => GenericError(s"$block is invalid: $x")
        case x               => x
      }

  private def minerBalance(blockchain: Blockchain, minerAddress: Address, parentHeight: Height, block: Block): Either[String, Long] = {
    val parentBlockId = block.header.reference
    val balance       = blockchain.generatingBalance(minerAddress, Some(parentBlockId))

    if (blockchain.isGeneratingBalanceValid(parentHeight.toInt, block, balance))
      Either.right(
        balance + block.header.challengedHeader.map(ch => blockchain.generatingBalance(ch.generator.toAddress, Some(parentBlockId))).getOrElse(0L)
      )
    else if (minerAddress == block.sender.toAddress) Either.left(s"generator's effective balance $balance is less that required for generation")
    else Either.right(0L) // Ignore for a regular generator, not a miner
  }

  private def checkExceptions(height: Height, block: Block, origError: ValidationError): Either[ValidationError, Unit] =
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

  private def validateConflictingEndorsement(
      blockchain: Blockchain,
      commitedGenerators: IndexedSeq[(Address, BlsPublicKey)],
      validEndorsements: Set[Address],
      minerAddress: Address,
      generatorsWithEnoughBalance: Set[GeneratorIndex],
      validFinalizedHeight: Height
  )(
      conflictingEndorsement: BlockEndorsement
  ): Either[String, Unit] = for {
    (address, blsPublicKey) <- commitedGenerators
      .lift(conflictingEndorsement.endorserIndex.toInt)
      .toRight(s"Invalid conflicting endorser index ${conflictingEndorsement.endorserIndex}")
    _ <- Either.raiseUnless(generatorsWithEnoughBalance.contains(conflictingEndorsement.endorserIndex))(
      s"Conflicting endorsement sender $address has insufficient balance"
    )
    _ <- Either.raiseWhen(address == minerAddress)("Conflicting endorsement from miner is not allowed")
    _ <- Either.raiseWhen(validEndorsements.contains(address))(s"Block contains both conflicting and valid endorsement from $address")
    _ <- Either.raiseWhen(conflictingEndorsement.finalizedHeight > validFinalizedHeight) {
      s"Finalized height ${conflictingEndorsement.finalizedHeight} is higher than expected $validFinalizedHeight"
    }
    finalizedBlock <- blockchain
      .blockHeader(conflictingEndorsement.finalizedHeight.toInt)
      .toRight(s"Can't find block at ${conflictingEndorsement.finalizedHeight}")
    _ <- Either.raiseWhen(conflictingEndorsement.finalizedId == finalizedBlock.id()) {
      s"Contains expected finalized block: ${conflictingEndorsement.finalizedId}"
    }
    _ <- Either.raiseUnless(conflictingEndorsement.signatureValid(blsPublicKey))(s"Invalid conflicting endorsement signature from $address")
  } yield ()

  def validateFinalizationVoting(
      block: Block,
      blockchain: Blockchain,
      validGeneratorBalances: GeneratorBalances
  ): Either[ValidationError, GeneratorBalances] =
    block.header.finalizationVoting
      .fold(Right(validGeneratorBalances)) { fv =>
        for {
          _ <- Either.raiseUnless(blockchain.supportsFinalizationVoting(blockchain.height + 1))(
            "Finalization voting is not allowed before Deterministic Finality feature activation"
          )
          _ <- Either.raiseWhen(block.header.challengedHeader.nonEmpty && block.header.finalizationVoting.nonEmpty)(
            "Finalization voting is not allowed in challenging block"
          )
          _ <- Either.raiseWhen(fv.finalizedHeight.toInt >= blockchain.height)("Voting for finalized block")
          _ <- Either.raiseWhen(fv.valid.isEmpty && fv.conflict.isEmpty)("Finalization voting contains neither valid nor conflicting endorsements")
          _ <- Either.raiseWhen(fv.valid.size > blockchain.settings.functionalitySettings.maxEndorsements)("Too many valid endorsements")
          _ <- Either.raiseWhen(fv.valid.toSet.size != fv.valid.length)("Duplicate valid endorser indexes")
          _ <- Either.raiseWhen(fv.conflict.groupBy(_.endorserIndex).size != fv.conflict.length)("Duplicate conflicting endorser indexes")

          generatorsWithEnoughBalance = validGeneratorBalances.view.map(_._1).toSet
          blockGenerationPeriod <- blockchain
            .generationPeriodOf(Height(blockchain.height + 1))
            .toRight(s"No period for height ${blockchain.height + 1}")
          allCommittedGenerators = blockchain.committedGenerators(blockGenerationPeriod)

          validEndorsers <- fv.valid.traverse(gi => allCommittedGenerators.lift(gi.toInt).toRight(s"Invalid endorser index: $gi"))
          _ <- fv.valid.traverse { idx =>
            Either.raiseUnless(generatorsWithEnoughBalance.contains(idx))(s"Valid endorsement sender $idx has insufficient balance")
          }
          validEndorserAddresses = validEndorsers.view.map(_._1).toSet
          _ <- Either.raiseWhen(validEndorserAddresses.contains(block.header.generator.toAddress))("Miner can't endorse their own block")
          _ <- fv.conflict
            .traverse(
              validateConflictingEndorsement(
                blockchain,
                allCommittedGenerators,
                validEndorserAddresses,
                block.header.generator.toAddress,
                generatorsWithEnoughBalance,
                fv.finalizedHeight
              )
            )
          conflictingEndorsers            = fv.conflict.map(_.endorserIndex).toSet
          nonConflictingGeneratorBalances = validGeneratorBalances.filterNot(x => conflictingEndorsers.contains(x.index))
          _ <-
            if (fv.valid.isEmpty)
              Either.raiseUnless(fv.aggregatedEndorsement.arr.isEmpty)(
                "No endorsements are included, but aggregated endorsement signature is non-empty"
              )
            else
              for {
                finalizedBlockId <- blockchain.blockId(fv.finalizedHeight.toInt).toRight(s"Unable to get block ID at height ${fv.finalizedHeight}")
                _ <-
                  if (validEndorsers.isEmpty) Right(())
                  else
                    BlsUtils.verifyAgg(
                      fv.aggregatedEndorsement.arr,
                      BlockEndorsement.mkMessage(finalizedBlockId, fv.finalizedHeight, block.header.reference),
                      validEndorsers.view.map(_._2.arr)
                    )
              } yield ()
        } yield nonConflictingGeneratorBalances
      }
      .leftMap(s => BlockAppendError(s, block))

  private object metrics {
    val blockConsensusValidation = Kamon.timer("block-appender.block-consensus-validation").withoutTags()
    val appendBlock              = Kamon.timer("block-appender.blockchain-append-block").withoutTags()
  }

}
