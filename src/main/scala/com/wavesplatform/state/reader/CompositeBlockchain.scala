package com.wavesplatform.state.reader

import cats.implicits._
import com.wavesplatform.state._
import scorex.account.{Address, Alias}
import scorex.block.{Block, BlockHeader}
import scorex.transaction.{AssetId, Transaction}
import scorex.transaction.Transaction.Type
import scorex.transaction.assets.{IssueTransaction, SmartIssueTransaction}
import scorex.transaction.base.LeaseTxBase
import scorex.transaction.smart.script.Script

class CompositeBlockchain(inner: Blockchain, maybeDiff: => Option[Diff]) extends Blockchain {

  private def diff = maybeDiff.getOrElse(Diff.empty)

  override def portfolio(a: Address): Portfolio = inner.portfolio(a).combine(diff.portfolios.getOrElse(a, Portfolio.empty))

  override def balance(address: Address, assetId: Option[AssetId]): Long =
    inner.balance(address, assetId) + diff.portfolios.getOrElse(address, Portfolio.empty).balanceOf(assetId)

  override def assetDescription(id: ByteStr): Option[AssetDescription] =
    inner.assetDescription(id) match {
      case Some(ad) =>
        diff.issuedAssets
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
        diff.transactions
          .get(id)
          .collectFirst {
            case (_, it: IssueTransaction, _) =>
              AssetDescription(it.sender, it.name, it.description, it.decimals, it.reissuable, it.quantity, None, sponsorship)
            case (_, it: SmartIssueTransaction, _) =>
              AssetDescription(it.sender, it.name, it.description, it.decimals, it.reissuable, it.quantity, it.script, sponsorship)
          }
          .map(z => diff.issuedAssets.get(id).fold(z)(r => z.copy(reissuable = r.isReissuable, totalVolume = r.volume, script = r.script)))
    }

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = {
    inner.leaseDetails(leaseId).map(ld => ld.copy(isActive = diff.leaseState.getOrElse(leaseId, ld.isActive))) orElse
      diff.transactions.get(leaseId).collect {
        case (h, lt: LeaseTxBase, _) =>
          LeaseDetails(lt.sender, lt.recipient, h, lt.amount, diff.leaseState(lt.id()))
      }
  }

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] =
    diff.transactions
      .get(id)
      .map(t => (t._1, t._2))
      .orElse(inner.transactionInfo(id))

  override def transactionHeight(id: ByteStr): Option[Int] =
    diff.transactions
      .get(id)
      .map(_._1)
      .orElse(inner.transactionHeight(id))

  override def height: Int = inner.height + (if (maybeDiff.isDefined) 1 else 0)

  override def addressTransactions(address: Address, types: Set[Type], count: Int, from: Int): Seq[(Int, Transaction)] = {
    val transactionsFromDiff = diff.transactions.values.view
      .collect {
        case (height, tx, addresses) if addresses(address) && (types.isEmpty || types.contains(tx.builder.typeId)) => (height, tx)
      }
      .slice(from, from + count)
      .toSeq

    val actualTxCount = transactionsFromDiff.length

    if (actualTxCount == count) transactionsFromDiff
    else {
      transactionsFromDiff ++ inner.addressTransactions(address, types, count - actualTxCount, 0)
    }
  }

  override def resolveAlias(a: Alias): Option[Address] = diff.aliases.get(a).orElse(inner.resolveAlias(a))

  override def allActiveLeases: Set[LeaseTxBase] = {
    val (active, canceled) = diff.leaseState.partition(_._2)
    val fromDiff = active.keys
      .map { id =>
        diff.transactions(id)._2
      }
      .collect { case lt: LeaseTxBase => lt }
      .toSet
    val fromInner = inner.allActiveLeases.filterNot(ltx => canceled.keySet.contains(ltx.id()))
    fromDiff ++ fromInner
  }

  override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] = {
    val b = Map.newBuilder[Address, A]
    for ((a, p) <- diff.portfolios if p.lease != LeaseBalance.empty || p.balance != 0) {
      pf.runWith(b += a -> _)(a -> portfolio(a).copy(assets = Map.empty))
    }

    inner.collectLposPortfolios(pf) ++ b.result()
  }

  override def containsTransaction(id: ByteStr): Boolean = diff.transactions.contains(id) || inner.containsTransaction(id)

  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee =
    diff.orderFills.get(orderId).orEmpty.combine(inner.filledVolumeAndFee(orderId))

  override def balanceSnapshots(address: Address, from: Int, to: Int): Seq[BalanceSnapshot] = {
    if (to <= inner.height || maybeDiff.isEmpty) {
      inner.balanceSnapshots(address, from, to)
    } else {
      val bs = BalanceSnapshot(height, portfolio(address))
      if (inner.height > 0 && from < this.height) bs +: inner.balanceSnapshots(address, from, to) else Seq(bs)
    }
  }

  override def accountScript(address: Address): Option[Script] = {
    diff.scripts.get(address) match {
      case None            => inner.accountScript(address)
      case Some(None)      => None
      case Some(Some(scr)) => Some(scr)
    }
  }

  override def accountData(acc: Address): AccountDataInfo = {
    val fromInner = inner.accountData(acc)
    val fromDiff  = diff.accountData.get(acc).orEmpty
    fromInner.combine(fromDiff)
  }

  override def accountData(acc: Address, key: String): Option[DataEntry[_]] = {
    val diffData = diff.accountData.get(acc).orEmpty
    diffData.data.get(key).orElse(inner.accountData(acc, key))
  }

  private def changedBalances(pred: Portfolio => Boolean, f: Address => Long): Map[Address, Long] =
    for {
      (address, p) <- diff.portfolios
      if pred(p)
    } yield address -> f(address)

  override def assetDistribution(height: Int, assetId: ByteStr): Map[Address, Long] = {
    val innerDistribution = inner.assetDistribution(height, assetId)
    if (height < this.height) innerDistribution
    else {
      innerDistribution ++ changedBalances(_.assets.getOrElse(assetId, 0L) != 0, portfolio(_).assets.getOrElse(assetId, 0L))
    }
  }

  override def wavesDistribution(height: Int): Map[Address, Long] = {
    val innerDistribution = inner.wavesDistribution(height)
    if (height < this.height) innerDistribution
    else {
      innerDistribution ++ changedBalances(_.balance != 0, portfolio(_).balance)
    }
  }

  override def score: BigInt = inner.score

  override def scoreOf(blockId: ByteStr): Option[BigInt] = inner.scoreOf(blockId)

  override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)] = inner.blockHeaderAndSize(height)

  override def blockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)] = inner.blockHeaderAndSize(blockId)

  override def lastBlock: Option[Block] = inner.lastBlock

  override def blockBytes(height: Int): Option[Array[Type]] = inner.blockBytes(height)

  override def blockBytes(blockId: ByteStr): Option[Array[Type]] = inner.blockBytes(blockId)

  override def heightOf(blockId: ByteStr): Option[Int] = inner.heightOf(blockId)

  /** Returns the most recent block IDs, starting from the most recent  one */
  override def lastBlockIds(howMany: Int): Seq[ByteStr] = inner.lastBlockIds(howMany)

  /** Returns a chain of blocks starting with the block with the given ID (from oldest to newest) */
  override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] = inner.blockIdsAfter(parentSignature, howMany)

  override def parent(block: Block, back: Int): Option[Block] = inner.parent(block, back)

  /** Features related */
  override def approvedFeatures: Map[Short, Int] = inner.approvedFeatures

  override def activatedFeatures: Map[Short, Int] = inner.activatedFeatures

  override def featureVotes(height: Int): Map[Short, Int] = inner.featureVotes(height)

  override def append(diff: Diff, block: Block): Unit = inner.append(diff, block)

  override def rollbackTo(targetBlockId: ByteStr): Seq[Block] = inner.rollbackTo(targetBlockId)
}

object CompositeBlockchain {
  def composite(inner: Blockchain, diff: => Option[Diff]): Blockchain = new CompositeBlockchain(inner, diff)
  def composite(inner: Blockchain, diff: Diff): Blockchain            = new CompositeBlockchain(inner, Some(diff))
}
