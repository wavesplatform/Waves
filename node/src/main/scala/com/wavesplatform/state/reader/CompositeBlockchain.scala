package com.wavesplatform.state.reader

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Transaction.Type
import com.wavesplatform.transaction.ValidationError.{AliasDoesNotExist, AliasIsDisabled}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.{Asset, Transaction, ValidationError}

class CompositeBlockchain(inner: Blockchain, maybeDiff: => Option[Diff], carry: Long = 0) extends Blockchain {

  private def diff = maybeDiff.getOrElse(Diff.empty)

  override def portfolio(a: Address): Portfolio = inner.portfolio(a).combine(diff.portfolios.getOrElse(a, Portfolio.empty))

  override def balance(address: Address, assetId: Asset): Long =
    inner.balance(address, assetId) + diff.portfolios.getOrElse(address, Portfolio.empty).balanceOf(assetId)

  override def leaseBalance(address: Address): LeaseBalance = {
    cats.Monoid.combine(inner.leaseBalance(address), diff.portfolios.getOrElse(address, Portfolio.empty).lease)
  }

  override def assetScript(asset: IssuedAsset): Option[Script] = maybeDiff.flatMap(_.assetScripts.get(asset)).getOrElse(inner.assetScript(asset))

  override def hasAssetScript(asset: IssuedAsset): Boolean = maybeDiff.flatMap(_.assetScripts.get(asset)) match {
    case Some(s) => s.nonEmpty
    case None    => inner.hasAssetScript(asset)
  }

  override def assetDescription(asset: IssuedAsset): Option[AssetDescription] = {
    val script: Option[Script] = assetScript(asset)
    inner.assetDescription(asset) match {
      case Some(ad) =>
        diff.issuedAssets
          .get(asset)
          .map { newAssetInfo =>
            val oldAssetInfo = AssetInfo(ad.reissuable, ad.totalVolume)
            val combination  = Monoid.combine(oldAssetInfo, newAssetInfo)
            ad.copy(reissuable = combination.isReissuable, totalVolume = combination.volume, script = script)
          }
          .orElse(Some(ad.copy(script = script)))
          .map { ad =>
            diff.sponsorship.get(asset).fold(ad) {
              case SponsorshipValue(sponsorship) =>
                ad.copy(sponsorship = sponsorship)
              case SponsorshipNoInfo =>
                ad
            }
          }
      case None =>
        val sponsorship = diff.sponsorship.get(asset).fold(0L) {
          case SponsorshipValue(sp) => sp
          case SponsorshipNoInfo    => 0L
        }
        diff.transactions
          .get(asset.id)
          .collectFirst {
            case (_, it: IssueTransaction, _) =>
              AssetDescription(it.sender, it.name, it.description, it.decimals, it.reissuable, it.quantity, script, sponsorship)
          }
          .map(z => diff.issuedAssets.get(asset).fold(z)(r => z.copy(reissuable = r.isReissuable, totalVolume = r.volume, script = script)))
    }
  }

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = {
    inner.leaseDetails(leaseId).map(ld => ld.copy(isActive = diff.leaseState.getOrElse(leaseId, ld.isActive))) orElse
      diff.transactions.get(leaseId).collect {
        case (h, lt: LeaseTransaction, _) =>
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

  override def addressTransactions(address: Address, types: Set[Type], count: Int, fromId: Option[ByteStr]): Either[String, Seq[(Int, Transaction)]] =
    addressTransactionsFromDiff(inner, maybeDiff)(address, types, count, fromId)

  override def resolveAlias(alias: Alias): Either[ValidationError, Address] = inner.resolveAlias(alias) match {
    case l @ Left(AliasIsDisabled(_)) => l
    case Right(addr)                  => Right(diff.aliases.getOrElse(alias, addr))
    case Left(_)                      => diff.aliases.get(alias).toRight(AliasDoesNotExist(alias))
  }

  override def allActiveLeases: Set[LeaseTransaction] = {
    val (active, canceled) = diff.leaseState.partition(_._2)
    val fromDiff = active.keys
      .map { id =>
        diff.transactions(id)._2
      }
      .collect { case lt: LeaseTransaction => lt }
      .toSet
    val fromInner = inner.allActiveLeases.filterNot(ltx => canceled.keySet.contains(ltx.id()))
    fromDiff ++ fromInner
  }

  override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] = {
    val b = Map.newBuilder[Address, A]
    for ((a, p) <- diff.portfolios if p.lease != LeaseBalance.empty || p.balance != 0) {
      pf.runWith(b += a -> _)(a -> this.wavesPortfolio(a))
    }

    inner.collectLposPortfolios(pf) ++ b.result()
  }

  override def containsTransaction(tx: Transaction): Boolean = diff.transactions.contains(tx.id()) || inner.containsTransaction(tx)

  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee =
    diff.orderFills.get(orderId).orEmpty.combine(inner.filledVolumeAndFee(orderId))

  override def balanceSnapshots(address: Address, from: Int, to: BlockId): Seq[BalanceSnapshot] = {
    if (inner.heightOf(to).isDefined || maybeDiff.isEmpty) {
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

  override def hasScript(address: Address): Boolean = {
    diff.scripts.get(address) match {
      case None          => inner.hasScript(address)
      case Some(None)    => false
      case Some(Some(_)) => true
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

  override def assetDistribution(assetId: IssuedAsset): AssetDistribution = {
    val fromInner = inner.assetDistribution(assetId)
    val fromDiff  = AssetDistribution(changedBalances(_.assets.getOrElse(assetId, 0L) != 0, balance(_, assetId)))

    fromInner |+| fromDiff
  }

  override def assetDistributionAtHeight(asset: IssuedAsset,
                                         height: Int,
                                         count: Int,
                                         fromAddress: Option[Address]): Either[ValidationError, AssetDistributionPage] = {
    inner.assetDistributionAtHeight(asset, height, count, fromAddress)
  }

  override def wavesDistribution(height: Int): Either[ValidationError, Map[Address, Long]] = {
    val innerDistribution = inner.wavesDistribution(height)
    if (height < this.height) innerDistribution
    else {
      innerDistribution.map(_ ++ changedBalances(_.balance != 0, balance(_)))
    }
  }

  override def score: BigInt = inner.score

  override def scoreOf(blockId: ByteStr): Option[BigInt] = inner.scoreOf(blockId)

  override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)] = inner.blockHeaderAndSize(height)

  override def blockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)] = inner.blockHeaderAndSize(blockId)

  override def lastBlock: Option[Block] = inner.lastBlock

  override def carryFee: Long = carry

  override def blockBytes(height: Int): Option[Array[Type]] = inner.blockBytes(height)

  override def blockBytes(blockId: ByteStr): Option[Array[Type]] = inner.blockBytes(blockId)

  override def heightOf(blockId: ByteStr): Option[Int] = inner.heightOf(blockId)

  /** Returns the most recent block IDs, starting from the most recent  one */
  override def lastBlockIds(howMany: Int): Seq[ByteStr] = inner.lastBlockIds(howMany)

  /** Returns a chain of blocks starting with the block with the given ID (from oldest to newest) */
  override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] = inner.blockIdsAfter(parentSignature, howMany)

  override def parent(block: Block, back: Int): Option[Block] = inner.parent(block, back)

  override def totalFee(height: Int): Option[Long] = inner.totalFee(height)

  /** Features related */
  override def approvedFeatures: Map[Short, Int] = inner.approvedFeatures

  override def activatedFeatures: Map[Short, Int] = inner.activatedFeatures

  override def featureVotes(height: Int): Map[Short, Int] = inner.featureVotes(height)
}

object CompositeBlockchain {
  def composite(inner: Blockchain, diff: => Option[Diff]): Blockchain          = new CompositeBlockchain(inner, diff)
  def composite(inner: Blockchain, diff: Diff, carryFee: Long = 0): Blockchain = new CompositeBlockchain(inner, Some(diff), carryFee)
}
