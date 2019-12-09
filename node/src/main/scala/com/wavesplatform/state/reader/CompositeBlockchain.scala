package com.wavesplatform.state.reader

import cats.data.Ior
import cats.implicits._
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.{AliasDoesNotExist, AliasIsDisabled}
import com.wavesplatform.transaction.assets.UpdateAssetInfoTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, Transaction}

final case class CompositeBlockchain(
    inner: Blockchain,
    maybeDiff: Option[Diff] = None,
    newBlock: Option[Block] = None,
    carry: Long = 0,
    reward: Option[Long] = None
) extends Blockchain {
  override val settings: BlockchainSettings = inner.settings

  def diff: Diff = maybeDiff.getOrElse(Diff.empty)

  override def balance(address: Address, assetId: Asset): Long =
    inner.balance(address, assetId) + diff.portfolios.getOrElse(address, Portfolio.empty).balanceOf(assetId)

  override def leaseBalance(address: Address): LeaseBalance = {
    cats.Monoid.combine(inner.leaseBalance(address), diff.portfolios.getOrElse(address, Portfolio.empty).lease)
  }

  override def assetScript(asset: IssuedAsset): Option[(Script, Long)] =
    maybeDiff
      .flatMap(_.assetScripts.get(asset))
      .getOrElse(inner.assetScript(asset))

  override def assetDescription(asset: IssuedAsset): Option[AssetDescription] = {
    val script = assetScript(asset)

    val fromDiff = diff.issuedAssets
      .get(asset)
      .map {
        case (static, info, volume) =>
          val sponsorship = diff.sponsorship.get(asset).fold(0L) {
            case SponsorshipValue(sp) => sp
            case SponsorshipNoInfo    => 0L
          }

          AssetDescription(
            static.source,
            static.issuer,
            info.name,
            info.description,
            static.decimals,
            volume.isReissuable,
            volume.volume,
            info.lastUpdatedAt,
            script,
            sponsorship,
            static.nft
          )
      }

    val assetDescription =
      inner
        .assetDescription(asset)
        .orElse(fromDiff)
        .map { description =>
          diff.updatedAssets
            .get(asset)
            .fold(description) {
              case Ior.Left(info) =>
                description.copy(name = info.name, description = info.description, lastUpdatedAt = info.lastUpdatedAt)
              case Ior.Right(vol) =>
                description.copy(reissuable = description.reissuable && vol.isReissuable, totalVolume = description.totalVolume + vol.volume)
              case Ior.Both(info, vol) =>
                description
                  .copy(
                    reissuable = description.reissuable && vol.isReissuable,
                    totalVolume = description.totalVolume + vol.volume,
                    name = info.name,
                    description = info.description,
                    lastUpdatedAt = info.lastUpdatedAt
                  )
            }
        }
        .map { description =>
          diff.sponsorship
            .get(asset)
            .fold(description) {
              case SponsorshipNoInfo   => description.copy(sponsorship = 0L)
              case SponsorshipValue(v) => description.copy(sponsorship = v)
            }
        }

    assetDescription map { z =>
      diff.transactions
        .foldLeft(z.copy(script = script)) {
          case (acc, (ut: UpdateAssetInfoTransaction, _)) => acc.copy(name = Right(ut.name), description = Right(ut.description), lastUpdatedAt = Height(height))
          case (acc, _)                                        => acc
        }
    }
  }

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = {
    inner.leaseDetails(leaseId).map(ld => ld.copy(isActive = diff.leaseState.getOrElse(leaseId, ld.isActive))) orElse
      diff.transactionMap().get(leaseId).collect {
        case (lt: LeaseTransaction, _) =>
          LeaseDetails(lt.sender, lt.recipient, this.height, lt.amount, diff.leaseState(lt.id()))
      }
  }

  override def transferById(id: BlockId): Option[(Int, TransferTransaction)] = {
    diff
      .transactionMap()
      .get(id)
      .collect {
        case (tx: TransferTransaction, _) => (height, tx)
      }
      .orElse(inner.transferById(id))
  }

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] =
    diff
      .transactionMap()
      .get(id)
      .map(t => (this.height, t._1))
      .orElse(inner.transactionInfo(id))

  override def transactionHeight(id: ByteStr): Option[Int] =
    diff
      .transactionMap()
      .get(id)
      .map(_ => this.height)
      .orElse(inner.transactionHeight(id))

  override def height: Int = inner.height + newBlock.fold(0)(_ => 1)

  override def resolveAlias(alias: Alias): Either[ValidationError, Address] = inner.resolveAlias(alias) match {
    case l @ Left(AliasIsDisabled(_)) => l
    case Right(addr)                  => Right(diff.aliases.getOrElse(alias, addr))
    case Left(_)                      => diff.aliases.get(alias).toRight(AliasDoesNotExist(alias))
  }

  override def collectActiveLeases(filter: LeaseTransaction => Boolean): Seq[LeaseTransaction] =
    CompositeBlockchain.collectActiveLeases(inner, maybeDiff)(filter)

  override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] = {
    val b = Map.newBuilder[Address, A]
    for ((a, p) <- diff.portfolios if p.lease != LeaseBalance.empty || p.balance != 0) {
      pf.runWith(b += a -> _)(a -> this.wavesPortfolio(a))
    }

    inner.collectLposPortfolios(pf) ++ b.result()
  }

  override def containsTransaction(tx: Transaction): Boolean = diff.transactionMap().contains(tx.id()) || inner.containsTransaction(tx)

  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee =
    diff.orderFills.get(orderId).orEmpty.combine(inner.filledVolumeAndFee(orderId))

  override def balanceSnapshots(address: Address, from: Int, to: BlockId): Seq[BalanceSnapshot] = {
    if (inner.heightOf(to).isDefined || maybeDiff.isEmpty) {
      inner.balanceSnapshots(address, from, to)
    } else {
      val balance = this.balance(address)
      val lease   = this.leaseBalance(address)
      val bs      = BalanceSnapshot(height, Portfolio(balance, lease, Map.empty))
      if (inner.height > 0 && from < this.height) bs +: inner.balanceSnapshots(address, from, to) else Seq(bs)
    }
  }

  override def accountScript(address: Address): Option[AccountScriptInfo] = {
    diff.scripts.get(address) match {
      case None            => inner.accountScript(address)
      case Some(None)      => None
      case Some(Some(scr)) => Some(scr)
    }
  }

  override def hasAccountScript(address: Address): Boolean = {
    diff.scripts.get(address) match {
      case None          => inner.hasAccountScript(address)
      case Some(None)    => false
      case Some(Some(_)) => true
    }
  }

  override def accountData(acc: Address, key: String): Option[DataEntry[_]] = {
    val diffData = diff.accountData.get(acc).orEmpty
    diffData.data.get(key).orElse(inner.accountData(acc, key)).filterNot(_.isEmpty)
  }

  override def carryFee: Long = carry

  override def score: BigInt = newBlock.fold(BigInt(0))(_.blockScore()) + inner.score

  override def blockHeader(height: Int): Option[SignedBlockHeader] =
    newBlock match {
      case Some(b) if this.height == height => Some(SignedBlockHeader(b.header, b.signature))
      case _                                => inner.blockHeader(height)
    }

  override def heightOf(blockId: ByteStr): Option[Int] = newBlock.filter(_.uniqueId == blockId).map(_ => height) orElse inner.heightOf(blockId)

  /** Features related */
  override def approvedFeatures: Map[Short, Int] = inner.approvedFeatures

  override def activatedFeatures: Map[Short, Int] = inner.activatedFeatures

  override def featureVotes(height: Int): Map[Short, Int] = inner.featureVotes(height)

  /** Block reward related */
  override def blockReward(height: Int): Option[Long] = reward.filter(_ => this.height == height) orElse inner.blockReward(height)

  override def blockRewardVotes(height: Int): Seq[Long] = inner.blockRewardVotes(height)

  override def wavesAmount(height: Int): BigInt = inner.wavesAmount(height)

  override def hitSource(height: Int): Option[ByteStr] = inner.hitSource(height)
}

object CompositeBlockchain {
  def collectActiveLeases(inner: Blockchain, maybeDiff: Option[Diff])(
      filter: LeaseTransaction => Boolean
  ): Seq[LeaseTransaction] = {
    val innerActiveLeases = inner.collectActiveLeases(filter)
    maybeDiff match {
      case Some(ng) =>
        val cancelledInLiquidBlock = ng.leaseState.collect {
          case (id, false) => id
        }.toSet
        val addedInLiquidBlock = ng.transactions.collect {
          case (lt: LeaseTransaction, _) if !cancelledInLiquidBlock(lt.id()) => lt
        }
        innerActiveLeases.filterNot(lt => cancelledInLiquidBlock(lt.id())) ++ addedInLiquidBlock
      case _ => innerActiveLeases
    }
  }
}
