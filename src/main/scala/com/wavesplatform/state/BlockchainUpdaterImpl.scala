package com.wavesplatform.state

import cats.implicits._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.metrics.{Instrumented, TxsInBlockchainStats}
import com.wavesplatform.mining.MiningEstimators
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.state.reader.{CompositeBlockchain, LeaseDetails}
import com.wavesplatform.utils.{UnsupportedFeature, forceStopApplication}
import kamon.Kamon
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import scorex.account.{Address, Alias}
import scorex.block.Block.BlockId
import scorex.block.{Block, BlockHeader, MicroBlock}
import scorex.transaction.Transaction.Type
import scorex.transaction._
import scorex.transaction.assets.{IssueTransaction, SmartIssueTransaction}
import scorex.transaction.base.LeaseTxBase
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.smart.script.Script
import scorex.transaction.validation.ValidationError
import scorex.transaction.validation.ValidationError.{BlockAppendError, GenericError, MicroBlockAppendError}
import scorex.utils.{ScorexLogging, Time}

class BlockchainUpdaterImpl(blockchain: Blockchain, settings: WavesSettings, time: Time)
    extends BlockchainUpdater
    with NG
    with ScorexLogging
    with Instrumented {

  import com.wavesplatform.state.BlockchainUpdaterImpl._
  import settings.blockchainSettings.functionalitySettings

  private lazy val maxBlockReadinessAge = settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed.toMillis

  private var ngState: Option[NgState] = Option.empty

  private val service               = monix.execution.Scheduler.singleThread("last-block-info-publisher")
  private val internalLastBlockInfo = ConcurrentSubject.publish[LastBlockInfo](service)

  override val lastBlockInfo: Observable[LastBlockInfo] = internalLastBlockInfo.cache(1)
  lastBlockInfo.subscribe()(monix.execution.Scheduler.global) // Start caching

  def blockchainReady: Boolean = {
    val lastBlock = ngState.map(_.base.timestamp).orElse(blockchain.lastBlockTimestamp).get
    lastBlock + maxBlockReadinessAge > time.correctedTime()
  }

  // Store last block information in a cache
  lastBlockId.foreach { id =>
    internalLastBlockInfo.onNext(LastBlockInfo(id, height, score, blockchainReady))
  }

  private def displayFeatures(s: Set[Short]): String =
    s"FEATURE${if (s.size > 1) "S" else ""} ${s.mkString(", ")} ${if (s.size > 1) "have been" else "has been"}"

  private def featuresApprovedWithBlock(block: Block): Set[Short] = {
    val height = blockchain.height + 1

    val featuresCheckPeriod        = functionalitySettings.activationWindowSize(height)
    val blocksForFeatureActivation = functionalitySettings.blocksForFeatureActivation(height)

    if (height % featuresCheckPeriod == 0) {
      val approvedFeatures = blockchain
        .featureVotes(height)
        .map { case (feature, votes) => feature -> (if (block.featureVotes.contains(feature)) votes + 1 else votes) }
        .filter { case (_, votes) => votes >= blocksForFeatureActivation }
        .keySet

      if (approvedFeatures.nonEmpty) log.info(s"${displayFeatures(approvedFeatures)} APPROVED at height $height")

      val unimplementedApproved = approvedFeatures.diff(BlockchainFeatures.implemented)
      if (unimplementedApproved.nonEmpty) {
        log.warn(s"UNIMPLEMENTED ${displayFeatures(unimplementedApproved)} APPROVED ON BLOCKCHAIN")
        log.warn("PLEASE, UPDATE THE NODE AS SOON AS POSSIBLE")
        log.warn("OTHERWISE THE NODE WILL BE STOPPED OR FORKED UPON FEATURE ACTIVATION")
      }

      val activatedFeatures: Set[Short] = blockchain.activatedFeaturesAt(height)

      val unimplementedActivated = activatedFeatures.diff(BlockchainFeatures.implemented)
      if (unimplementedActivated.nonEmpty) {
        log.error(s"UNIMPLEMENTED ${displayFeatures(unimplementedActivated)} ACTIVATED ON BLOCKCHAIN")
        log.error("PLEASE, UPDATE THE NODE IMMEDIATELY")
        if (settings.featuresSettings.autoShutdownOnUnsupportedFeature) {
          log.error("FOR THIS REASON THE NODE WAS STOPPED AUTOMATICALLY")
          forceStopApplication(UnsupportedFeature)
        } else log.error("OTHERWISE THE NODE WILL END UP ON A FORK")
      }

      approvedFeatures
    } else {

      Set.empty
    }
  }

  override def processBlock(block: Block): Either[ValidationError, Option[DiscardedTransactions]] = {
    val height                             = blockchain.height
    val notImplementedFeatures: Set[Short] = blockchain.activatedFeaturesAt(height).diff(BlockchainFeatures.implemented)

    Either
      .cond(
        !settings.featuresSettings.autoShutdownOnUnsupportedFeature || notImplementedFeatures.isEmpty,
        (),
        GenericError(s"UNIMPLEMENTED ${displayFeatures(notImplementedFeatures)} ACTIVATED ON BLOCKCHAIN, UPDATE THE NODE IMMEDIATELY")
      )
      .flatMap(_ =>
        (ngState match {
          case None =>
            blockchain.lastBlockId match {
              case Some(uniqueId) if uniqueId != block.reference =>
                val logDetails = s"The referenced block(${block.reference})" +
                  s" ${if (blockchain.contains(block.reference)) "exits, it's not last persisted" else "doesn't exist"}"
                Left(BlockAppendError(s"References incorrect or non-existing block: " + logDetails, block))
              case _ =>
                BlockDiffer
                  .fromBlock(functionalitySettings, blockchain, blockchain.lastBlock, block)
                  .map(d => Some((d, Seq.empty[Transaction])))
            }
          case Some(ng) =>
            if (ng.base.reference == block.reference) {
              if (block.blockScore() > ng.base.blockScore()) {
                BlockDiffer.fromBlock(functionalitySettings, blockchain, blockchain.lastBlock, block).map { diff =>
                  log.trace(
                    s"Better liquid block(score=${block.blockScore()}) received and applied instead of existing(score=${ng.base.blockScore()})")
                  Some((diff, ng.transactions))
                }
              } else if (areVersionsOfSameBlock(block, ng.base)) {
                if (block.transactionData.lengthCompare(ng.transactions.size) <= 0) {
                  log.trace(s"Existing liquid block is better than new one, discarding $block")
                  Right(None)
                } else {
                  log.trace(s"New liquid block is better version of exsting, swapping")
                  BlockDiffer
                    .fromBlock(functionalitySettings, blockchain, blockchain.lastBlock, block)
                    .map(d => Some((d, Seq.empty[Transaction])))
                }
              } else
                Left(BlockAppendError(
                  s"Competitors liquid block $block(score=${block.blockScore()}) is not better than existing (ng.base ${ng.base}(score=${ng.base.blockScore()}))",
                  block))
            } else
              measureSuccessful(forgeBlockTimeStats, ng.totalDiffOf(block.reference)) match {
                case None => Left(BlockAppendError(s"References incorrect or non-existing block", block))
                case Some((referencedForgedBlock, referencedLiquidDiff, discarded)) =>
                  if (referencedForgedBlock.signaturesValid().isRight) {
                    if (discarded.nonEmpty) {
                      microBlockForkStats.increment()
                      microBlockForkHeightStats.record(discarded.size)
                    }

                    val diff = BlockDiffer
                      .fromBlock(functionalitySettings,
                                 CompositeBlockchain.composite(blockchain, referencedLiquidDiff),
                                 Some(referencedForgedBlock),
                                 block)

                    diff.map { hardenedDiff =>
                      blockchain.append(referencedLiquidDiff, referencedForgedBlock)
                      TxsInBlockchainStats.record(ng.transactions.size)
                      Some((hardenedDiff, discarded.flatMap(_.transactionData)))
                    }
                  } else {
                    val errorText = s"Forged block has invalid signature: base: ${ng.base}, requested reference: ${block.reference}"
                    log.error(errorText)
                    Left(BlockAppendError(errorText, block))
                  }
              }
        }).map {
          _ map {
            case ((newBlockDiff, discarded)) =>
              val height     = blockchain.height + 1
              val estimators = MiningEstimators(settings.minerSettings, blockchain, blockchain.height)
              ngState = Some(new NgState(block, newBlockDiff, featuresApprovedWithBlock(block), estimators.total))
              lastBlockId.foreach(id => internalLastBlockInfo.onNext(LastBlockInfo(id, height, score, blockchainReady)))
              if ((block.timestamp > time
                    .getTimestamp() - settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed.toMillis) || (height % 100 == 0)) {
                log.info(s"New height: $height")
              }
              discarded
          }
      })
  }

  override def removeAfter(blockId: ByteStr): Either[ValidationError, Seq[Block]] = {
    val ng = ngState
    if (ng.exists(_.contains(blockId))) {
      log.trace("Resetting liquid block, no rollback is necessary")
      Right(Seq.empty)
    } else {
      val discardedNgBlock = ng.map(_.bestLiquidBlock).toSeq
      ngState = None
      Right(blockchain.rollbackTo(blockId) ++ discardedNgBlock)
    }
  }

  override def processMicroBlock(microBlock: MicroBlock): Either[ValidationError, Unit] = {
    ngState match {
      case None =>
        Left(MicroBlockAppendError("No base block exists", microBlock))
      case Some(ng) if ng.base.signerData.generator.toAddress != microBlock.sender.toAddress =>
        Left(MicroBlockAppendError("Base block has been generated by another account", microBlock))
      case Some(ng) =>
        ng.lastMicroBlock match {
          case None if ng.base.uniqueId != microBlock.prevResBlockSig =>
            blockMicroForkStats.increment()
            Left(MicroBlockAppendError("It's first micro and it doesn't reference base block(which exists)", microBlock))
          case Some(prevMicro) if prevMicro.totalResBlockSig != microBlock.prevResBlockSig =>
            microMicroForkStats.increment()
            Left(MicroBlockAppendError("It doesn't reference last known microBlock(which exists)", microBlock))
          case _ =>
            for {
              _    <- microBlock.signaturesValid()
              diff <- BlockDiffer.fromMicroBlock(functionalitySettings, this, blockchain.lastBlockTimestamp, microBlock, ng.base.timestamp)
              _ <- Either.cond(ng.append(microBlock, diff, System.currentTimeMillis),
                               (),
                               MicroBlockAppendError("Limit of txs was reached", microBlock))
            } yield {
              log.info(s"$microBlock appended")
              internalLastBlockInfo.onNext(LastBlockInfo(microBlock.totalResBlockSig, height, score, ready = true))
            }
        }
    }
  }

  def shutdown(): Unit = {
    internalLastBlockInfo.onComplete()
    service.shutdown()
  }

  private def newlyApprovedFeatures = ngState.fold(Map.empty[Short, Int])(_.approvedFeatures.map(_ -> height).toMap)

  override def approvedFeatures: Map[Short, Int] = newlyApprovedFeatures ++ blockchain.approvedFeatures

  override def activatedFeatures: Map[Short, Int] =
    newlyApprovedFeatures.mapValues(_ + functionalitySettings.activationWindowSize(height)) ++ blockchain.activatedFeatures

  override def featureVotes(height: Int): Map[Short, Int] = {
    val innerVotes = blockchain.featureVotes(height)
    ngState match {
      case Some(ng) if this.height <= height =>
        val ngVotes = ng.base.featureVotes.map { featureId =>
          featureId -> (innerVotes.getOrElse(featureId, 0) + 1)
        }.toMap

        innerVotes ++ ngVotes
      case _ => innerVotes
    }
  }

  private def liquidBlockHeaderAndSize() = ngState.map { s =>
    (s.bestLiquidBlock, s.bestLiquidBlock.bytes().length)
  }

  override def blockHeaderAndSize(blockId: BlockId): Option[(BlockHeader, Int)] =
    liquidBlockHeaderAndSize().filter(_._1.uniqueId == blockId) orElse blockchain.blockHeaderAndSize(blockId)

  override def height: Int = blockchain.height + ngState.fold(0)(_ => 1)

  override def blockBytes(height: Int): Option[Array[Byte]] =
    blockchain
      .blockBytes(height)
      .orElse(ngState.collect { case ng if height == blockchain.height + 1 => ng.bestLiquidBlock.bytes() })

  override def scoreOf(blockId: BlockId): Option[BigInt] =
    blockchain
      .scoreOf(blockId)
      .orElse(ngState.collect { case ng if ng.contains(blockId) => blockchain.score + ng.base.blockScore() })

  override def heightOf(blockId: BlockId): Option[Int] =
    blockchain
      .heightOf(blockId)
      .orElse(ngState.collect { case ng if ng.contains(blockId) => this.height })

  override def lastBlockIds(howMany: Int): Seq[BlockId] =
    ngState.fold(blockchain.lastBlockIds(howMany))(_.bestLiquidBlockId +: blockchain.lastBlockIds(howMany - 1))

  override def microBlock(id: BlockId): Option[MicroBlock] =
    for {
      ng <- ngState
      mb <- ng.microBlock(id)
    } yield mb

  def lastBlockTimestamp: Option[Long] = ngState.map(_.base.timestamp).orElse(blockchain.lastBlockTimestamp)

  def lastBlockId: Option[AssetId] = ngState.map(_.bestLiquidBlockId).orElse(blockchain.lastBlockId)

  def blockAt(height: Int): Option[Block] =
    if (height == this.height)
      ngState.map(_.bestLiquidBlock)
    else
      blockchain.blockAt(height)

  override def lastPersistedBlockIds(count: Int): Seq[BlockId] = {
    blockchain.lastBlockIds(count)
  }

  override def microblockIds: Seq[BlockId] = ngState.fold(Seq.empty[BlockId])(_.microBlockIds)

  override def bestLastBlockInfo(maxTimestamp: Long): Option[BlockMinerInfo] = {
    ngState
      .map(_.bestLastBlockInfo(maxTimestamp))
      .orElse(blockchain.lastBlock.map(b => BlockMinerInfo(b.consensusData, b.timestamp, b.uniqueId)))
  }

  override def score: BigInt = blockchain.score + ngState.fold(BigInt(0))(_.bestLiquidBlock.blockScore())

  override def lastBlock: Option[Block] = ngState.map(_.bestLiquidBlock).orElse(blockchain.lastBlock)

  override def blockBytes(blockId: ByteStr): Option[Array[Byte]] =
    (for {
      ng            <- ngState
      (block, _, _) <- ng.totalDiffOf(blockId)
    } yield block.bytes()).orElse(blockchain.blockBytes(blockId))

  override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] = {
    ngState match {
      case Some(ng) if ng.contains(parentSignature) => Some(Seq.empty[ByteStr])
      case maybeNg =>
        blockchain.blockIdsAfter(parentSignature, howMany).map { ib =>
          if (ib.lengthCompare(howMany) < 0) ib ++ maybeNg.map(_.bestLiquidBlockId) else ib
        }
    }
  }

  override def parent(block: Block, back: Int): Option[Block] = {
    ngState match {
      case Some(ng) if ng.contains(block.reference) =>
        if (back == 1) Some(ng.base) else blockchain.parent(ng.base, back - 1)
      case _ =>
        blockchain.parent(block, back)
    }
  }

  override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)] = {
    if (height == blockchain.height + 1)
      ngState.map(x => (x.bestLiquidBlock, x.bestLiquidBlock.bytes().length))
    else
      blockchain.blockHeaderAndSize(height)
  }

  override def portfolio(a: Address): Portfolio = {
    val p = ngState.fold(Portfolio.empty)(_.bestLiquidDiff.portfolios.getOrElse(a, Portfolio.empty))
    blockchain.portfolio(a).combine(p)
  }

  override def transactionInfo(id: AssetId): Option[(Int, Transaction)] =
    ngState
      .fold(Diff.empty)(_.bestLiquidDiff)
      .transactions
      .get(id)
      .map(t => (t._1, t._2))
      .orElse(blockchain.transactionInfo(id))

  override def addressTransactions(address: Address, types: Set[Type], count: Int, from: Int): Seq[(Int, Transaction)] =
    ngState.fold(blockchain.addressTransactions(address, types, count, from)) { ng =>
      val transactionsFromDiff = ng.bestLiquidDiff.transactions.values.view
        .collect {
          case (height, tx, addresses) if addresses(address) && (types.isEmpty || types.contains(tx.builder.typeId)) => (height, tx)
        }
        .slice(from, from + count)
        .toSeq

      val actualTxCount = transactionsFromDiff.length

      if (actualTxCount == count) transactionsFromDiff
      else {
        transactionsFromDiff ++ blockchain.addressTransactions(address, types, count - actualTxCount, 0)
      }
    }

  override def containsTransaction(id: AssetId): Boolean = ngState.fold(blockchain.containsTransaction(id)) { ng =>
    ng.bestLiquidDiff.transactions.contains(id) || blockchain.containsTransaction(id)
  }

  override def assetDescription(id: AssetId): Option[AssetDescription] = ngState.fold(blockchain.assetDescription(id)) { ng =>
    val diff = ng.bestLiquidDiff
    blockchain.assetDescription(id) match {
      case Some(ad) =>
        ng.bestLiquidDiff.issuedAssets
          .get(id)
          .map { newAssetInfo =>
            ad.copy(
              reissuable = newAssetInfo.isReissuable,
              totalVolume = ad.totalVolume + newAssetInfo.volume,
              script = newAssetInfo.script
            )
          }
          .orElse(Some(ad))
          .map { ad =>
            diff.sponsorship.get(id).fold(ad) {
              case SponsorshipValue(sponsorship) =>
                ad.copy(sponsorship = sponsorship)
              case SponsorshipNoInfo =>
                ad
            }
          }
      case None =>
        val sponsorship = diff.sponsorship.get(id).fold(0L) {
          case SponsorshipValue(sponsorship) =>
            sponsorship
          case SponsorshipNoInfo =>
            0L
        }
        ng.bestLiquidDiff.transactions
          .get(id)
          .collectFirst {
            case (_, it: IssueTransaction, _) =>
              AssetDescription(it.sender, it.name, it.description, it.decimals, it.reissuable, it.quantity, None, sponsorship)
            case (_, it: SmartIssueTransaction, _) =>
              AssetDescription(it.sender, it.name, it.description, it.decimals, it.reissuable, it.quantity, it.script, sponsorship)
          }
          .map(z =>
            ng.bestLiquidDiff.issuedAssets.get(id).fold(z)(r => z.copy(reissuable = r.isReissuable, totalVolume = r.volume, script = r.script)))
    }
  }

  override def resolveAlias(a: Alias): Option[Address] =
    ngState.fold(blockchain.resolveAlias(a))(_.bestLiquidDiff.aliases.get(a).orElse(blockchain.resolveAlias(a)))

  override def leaseDetails(leaseId: AssetId): Option[LeaseDetails] = ngState match {
    case Some(ng) =>
      blockchain.leaseDetails(leaseId).map(ld => ld.copy(isActive = ng.bestLiquidDiff.leaseState.getOrElse(leaseId, ld.isActive))) orElse
        ng.bestLiquidDiff.transactions.get(leaseId).collect {
          case (h, lt: LeaseTransaction, _) =>
            LeaseDetails(lt.sender, lt.recipient, h, lt.amount, ng.bestLiquidDiff.leaseState(lt.id()))
        }
    case None =>
      blockchain.leaseDetails(leaseId)
  }

  override def filledVolumeAndFee(orderId: AssetId): VolumeAndFee =
    ngState.fold(blockchain.filledVolumeAndFee(orderId))(
      _.bestLiquidDiff.orderFills.get(orderId).orEmpty.combine(blockchain.filledVolumeAndFee(orderId)))

  /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
  override def balanceSnapshots(address: Address, from: Int, to: Int): Seq[BalanceSnapshot] =
    if (to <= blockchain.height || ngState.isEmpty) {
      blockchain.balanceSnapshots(address, from, to)
    } else {
      val bs = BalanceSnapshot(height, portfolio(address))
      if (blockchain.height > 0 && from < this.height) bs +: blockchain.balanceSnapshots(address, from, to) else Seq(bs)
    }

  override def accountScript(address: Address): Option[Script] = ngState.fold(blockchain.accountScript(address)) { ng =>
    ng.bestLiquidDiff.scripts.get(address) match {
      case None            => blockchain.accountScript(address)
      case Some(None)      => None
      case Some(Some(scr)) => Some(scr)
    }
  }

  override def accountData(acc: Address): AccountDataInfo = ngState.fold(blockchain.accountData(acc)) { ng =>
    val fromInner = blockchain.accountData(acc)
    val fromDiff  = ng.bestLiquidDiff.accountData.get(acc).orEmpty
    fromInner.combine(fromDiff)
  }

  override def accountData(acc: Address, key: String): Option[DataEntry[_]] = ngState.fold(blockchain.accountData(acc, key)) { ng =>
    val diffData = ng.bestLiquidDiff.accountData.get(acc).orEmpty
    diffData.data.get(key).orElse(blockchain.accountData(acc, key))
  }

  private def changedBalances(pred: Portfolio => Boolean, f: Address => Long): Map[Address, Long] =
    ngState
      .fold(Map.empty[Address, Long]) { ng =>
        for {
          (address, p) <- ng.bestLiquidDiff.portfolios
          if pred(p)
        } yield address -> f(address)
      }

  override def assetDistribution(height: Int, assetId: AssetId): Map[Address, Long] = ngState.fold(blockchain.assetDistribution(height, assetId)) {
    ng =>
      val innerDistribution = blockchain.assetDistribution(height, assetId)
      if (height < this.height) innerDistribution
      else {
        innerDistribution ++ changedBalances(_.assets.getOrElse(assetId, 0L) != 0, portfolio(_).assets.getOrElse(assetId, 0L))
      }
  }

  override def wavesDistribution(height: Int): Map[Address, Long] = ngState.fold(blockchain.wavesDistribution(height)) { ng =>
    val innerDistribution = blockchain.wavesDistribution(height)
    if (height < this.height) innerDistribution
    else {
      innerDistribution ++ changedBalances(_.balance != 0, portfolio(_).balance)
    }
  }

  override def allActiveLeases: Set[LeaseTxBase] = ngState.fold(blockchain.allActiveLeases) { ng =>
    val (active, canceled) = ng.bestLiquidDiff.leaseState.partition(_._2)
    val fromDiff = active.keys
      .map { id =>
        ng.bestLiquidDiff.transactions(id)._2
      }
      .collect { case lt: LeaseTxBase => lt }
      .toSet
    val fromInner = blockchain.allActiveLeases.filterNot(ltx => canceled.keySet.contains(ltx.id()))
    fromDiff ++ fromInner
  }

  /** Builds a new portfolio map by applying a partial function to all portfolios on which the function is defined.
    *
    * @note Portfolios passed to `pf` only contain Waves and Leasing balances to improve performance */
  override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] =
    ngState.fold(blockchain.collectLposPortfolios(pf)) { ng =>
      val b = Map.newBuilder[Address, A]
      for ((a, p) <- ng.bestLiquidDiff.portfolios if p.lease != LeaseBalance.empty || p.balance != 0) {
        pf.runWith(b += a -> _)(a -> portfolio(a).copy(assets = Map.empty))
      }

      blockchain.collectLposPortfolios(pf) ++ b.result()
    }

  override def append(diff: Diff, block: Block): Unit = blockchain.append(diff, block)

  override def rollbackTo(targetBlockId: AssetId): Seq[Block] = blockchain.rollbackTo(targetBlockId)

  override def transactionHeight(id: AssetId): Option[Int] = ngState match {
    case Some(ng) => ng.bestLiquidDiff.transactions.get(id).map(_._1)
    case None     => blockchain.transactionHeight(id)
  }

  override def balance(address: Address, mayBeAssetId: Option[AssetId]): Long = ngState match {
    case Some(ng) =>
      blockchain.balance(address, mayBeAssetId) + ng.bestLiquidDiff.portfolios.getOrElse(address, Portfolio.empty).balanceOf(mayBeAssetId)
    case None =>
      blockchain.balance(address, mayBeAssetId)
  }
}

object BlockchainUpdaterImpl extends ScorexLogging {

  import kamon.metric.instrument.{Time => KTime}

  private val blockMicroForkStats       = Kamon.metrics.counter("block-micro-fork")
  private val microMicroForkStats       = Kamon.metrics.counter("micro-micro-fork")
  private val microBlockForkStats       = Kamon.metrics.counter("micro-block-fork")
  private val microBlockForkHeightStats = Kamon.metrics.histogram("micro-block-fork-height")
  private val forgeBlockTimeStats       = Kamon.metrics.histogram("forge-block-time", KTime.Milliseconds)

  def areVersionsOfSameBlock(b1: Block, b2: Block): Boolean =
    b1.signerData.generator == b2.signerData.generator &&
      b1.consensusData.baseTarget == b2.consensusData.baseTarget &&
      b1.reference == b2.reference &&
      b1.timestamp == b2.timestamp
}
