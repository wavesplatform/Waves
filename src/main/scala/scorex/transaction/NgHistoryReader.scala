package scorex.transaction

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.state.reader.LeaseDetails
import scorex.account.{Address, Alias}
import scorex.block.Block.BlockId
import scorex.block.{Block, BlockHeader, MicroBlock}
import scorex.transaction.Transaction.Type
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.smart.Script

class NgHistoryReader(ngState: () => Option[NgState], inner: Blockchain, fs: FunctionalitySettings) extends NG {

  private def newlyApprovedFeatures = ngState().fold(Map.empty[Short, Int])(_.approvedFeatures.map(_ -> height).toMap)

  override def approvedFeatures(): Map[Short, Int] = newlyApprovedFeatures ++ inner.approvedFeatures()

  override def activatedFeatures(): Map[Short, Int] =
    newlyApprovedFeatures.mapValues(_ + fs.activationWindowSize(height)) ++ inner.activatedFeatures()

  override def featureVotes(height: Int): Map[Short, Int] = {
    val innerVotes = inner.featureVotes(height)
    ngState() match {
      case Some(ng) if this.height <= height =>
        val ngVotes = ng.base.featureVotes.map { featureId =>
          featureId -> (innerVotes.getOrElse(featureId, 0) + 1)
        }.toMap

        innerVotes ++ ngVotes
      case _ => innerVotes
    }
  }

  private def liquidBlockHeaderAndSize() = ngState().map { s =>
    (s.bestLiquidBlock, s.bestLiquidBlock.bytes().length)
  }

  override def blockHeaderAndSize(blockId: BlockId): Option[(BlockHeader, Int)] =
    liquidBlockHeaderAndSize().filter(_._1.uniqueId == blockId) orElse inner.blockHeaderAndSize(blockId)

  override def height: Int = inner.height + ngState().fold(0)(_ => 1)

  override def blockBytes(height: Int): Option[Array[Byte]] =
    inner
      .blockBytes(height)
      .orElse(ngState().collect { case ng if height == inner.height + 1 => ng.bestLiquidBlock.bytes() })

  override def scoreOf(blockId: BlockId): Option[BigInt] =
    inner
      .scoreOf(blockId)
      .orElse(ngState().collect { case ng if ng.contains(blockId) => inner.score + ng.base.blockScore() })

  override def heightOf(blockId: BlockId): Option[Int] =
    inner
      .heightOf(blockId)
      .orElse(ngState().collect { case ng if ng.contains(blockId) => this.height })

  override def lastBlockIds(howMany: Int): Seq[BlockId] =
    ngState().fold(inner.lastBlockIds(howMany))(_.bestLiquidBlockId +: inner.lastBlockIds(howMany - 1))

  override def microBlock(id: BlockId): Option[MicroBlock] =
    for {
      ng <- ngState()
      mb <- ng.microBlock(id)
    } yield mb

  def lastBlockTimestamp: Option[Long] = ngState().map(_.base.timestamp).orElse(inner.lastBlockTimestamp)

  def lastBlockId: Option[AssetId] = ngState().map(_.bestLiquidBlockId).orElse(inner.lastBlockId)

  def blockAt(height: Int): Option[Block] =
    if (height == this.height)
      ngState().map(_.bestLiquidBlock)
    else
      inner.blockAt(height)

  override def lastPersistedBlockIds(count: Int): Seq[BlockId] = {
    inner.lastBlockIds(count)
  }

  override def microblockIds(): Seq[BlockId] = ngState().fold(Seq.empty[BlockId])(_.microBlockIds)

  override def bestLastBlockInfo(maxTimestamp: Long): Option[BlockMinerInfo] = {
    ngState()
      .map(_.bestLastBlockInfo(maxTimestamp))
      .orElse(inner.lastBlock.map(b => BlockMinerInfo(b.consensusData, b.timestamp, b.uniqueId)))
  }

  override def score: BigInt = inner.score + ngState().fold(BigInt(0))(_.bestLiquidBlock.blockScore())

  override def lastBlock: Option[Block] = ngState().map(_.bestLiquidBlock).orElse(inner.lastBlock)

  override def blockBytes(blockId: ByteStr): Option[Array[Byte]] =
    (for {
      ng            <- ngState()
      (block, _, _) <- ng.totalDiffOf(blockId)
    } yield block.bytes()).orElse(inner.blockBytes(blockId))

  override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] = {
    ngState() match {
      case Some(ng) if ng.contains(parentSignature) => Some(Seq.empty[ByteStr])
      case maybeNg =>
        inner.blockIdsAfter(parentSignature, howMany).map { ib =>
          if (ib.lengthCompare(howMany) < 0) ib ++ maybeNg.map(_.bestLiquidBlockId) else ib
        }
    }
  }

  override def parent(block: Block, back: Int): Option[Block] = {
    ngState() match {
      case Some(ng) if ng.contains(block.reference) =>
        if (back == 1) Some(ng.base) else inner.parent(ng.base, back - 1)
      case _ =>
        inner.parent(block, back)
    }
  }

  override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)] = {
    if (height == inner.height + 1)
      ngState().map(x => (x.bestLiquidBlock, x.bestLiquidBlock.bytes().length))
    else
      inner.blockHeaderAndSize(height)
  }

  override def portfolio(a: Address): Portfolio = ???

  override def transactionInfo(id: AssetId): Option[(Int, Transaction)] = ???

  override def addressTransactions(address: Address, types: Set[Type], count: Int, from: Int): Seq[(Int, Transaction)] = ???

  override def containsTransaction(id: AssetId): Boolean = ???

  override def assetDescription(id: AssetId): Option[AssetDescription] = ???

  override def resolveAlias(a: Alias): Option[Address] = ???

  override def leaseDetails(leaseId: AssetId): Option[LeaseDetails] = ???

  override def filledVolumeAndFee(orderId: AssetId): VolumeAndFee = ???

  /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
  override def balanceSnapshots(address: Address, from: Int, to: Int): Seq[BalanceSnapshot] = ???

  override def accountScript(address: Address): Option[Script] = ???

  override def accountData(acc: Address): AccountDataInfo = ???

  override def accountData(acc: Address, key: String): Option[DataEntry[_]] = ???

  override def assetDistribution(height: Int, assetId: AssetId): Map[Address, Long] = ???

  override def wavesDistribution(height: Int): Map[Address, Long] = ???

  override def allActiveLeases: Set[LeaseTransaction] = ???

  /** Builds a new portfolio map by applying a partial function to all portfolios on which the function is defined.
    *
    * @note Portfolios passed to `pf` only contain Waves and Leasing balances to improve performance */
  override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] = ???
  override def append(diff: Diff, block: Block): Unit                                                  = ???
  override def rollbackTo(targetBlockId: AssetId): Seq[Block]                                          = ???
}
