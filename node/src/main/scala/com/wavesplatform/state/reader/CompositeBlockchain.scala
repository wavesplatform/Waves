package com.wavesplatform.state.reader

import cats.data.Ior
import cats.implicits._
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.ApplicationStatus.Succeeded
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{AliasDoesNotExist, AliasIsDisabled}
import com.wavesplatform.transaction.assets.UpdateAssetInfoTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{ApplicationStatus, Asset, Transaction}

final case class CompositeBlockchain(
    inner: Blockchain,
    maybeDiff: Option[Diff] = None,
    newBlock: Option[Block] = None,
    carry: Long = 0,
    reward: Option[Long] = None,
    hitSource: Option[ByteStr] = None
) extends Blockchain {
  override val settings: BlockchainSettings = inner.settings

  def diff: Diff = maybeDiff.getOrElse(Diff.empty)

  override def balance(address: Address, assetId: Asset): Long =
    inner.balance(address, assetId) + diff.portfolios.getOrElse(address, Portfolio.empty).balanceOf(assetId)

  override def leaseBalance(address: Address): LeaseBalance = {
    cats.Monoid.combine(inner.leaseBalance(address), diff.portfolios.getOrElse(address, Portfolio.empty).lease)
  }

  override def assetScript(asset: IssuedAsset): Option[AssetScriptInfo] =
    maybeDiff
      .flatMap(_.assetScripts.get(asset))
      .getOrElse(inner.assetScript(asset))

  override def assetDescription(asset: IssuedAsset): Option[AssetDescription] =
    CompositeBlockchain.assetDescription(asset, maybeDiff.orEmpty, inner.assetDescription(asset), inner.assetScript(asset), height)

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = {
    inner.leaseDetails(leaseId).map(ld => ld.copy(isActive = diff.leaseState.getOrElse(leaseId, ld.isActive))) orElse
      diff.transactions.get(leaseId).collect {
        case NewTransactionInfo(lt: LeaseTransaction, _, Succeeded) =>
          LeaseDetails(lt.sender, lt.recipient, this.height, lt.amount, diff.leaseState(lt.id()))
      }
  }

  override def transferById(id: ByteStr): Option[(Int, TransferTransaction)] = {
    diff.transactions
      .get(id)
      .collect {
        case NewTransactionInfo(tx: TransferTransaction, _, Succeeded) => (height, tx)
      }
      .orElse(inner.transferById(id))
  }

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction, ApplicationStatus)] =
    diff.transactions
      .get(id)
      .map(t => (this.height, t.transaction, t.status))
      .orElse(inner.transactionInfo(id))

  override def transactionMeta(id: ByteStr): Option[(Int, ApplicationStatus)] =
    diff.transactions
      .get(id)
      .map(info => (this.height, info.status))
      .orElse(inner.transactionMeta(id))

  override def height: Int = inner.height + newBlock.fold(0)(_ => 1)

  override def resolveAlias(alias: Alias): Either[ValidationError, Address] = inner.resolveAlias(alias) match {
    case l @ Left(AliasIsDisabled(_)) => l
    case Right(addr)                  => Right(diff.aliases.getOrElse(alias, addr))
    case Left(_)                      => diff.aliases.get(alias).toRight(AliasDoesNotExist(alias))
  }

  override def containsTransaction(tx: Transaction): Boolean = diff.transactions.contains(tx.id()) || inner.containsTransaction(tx)

  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee =
    diff.orderFills.get(orderId).orEmpty.combine(inner.filledVolumeAndFee(orderId))

  override def balanceAtHeight(address: Address, h: Int, assetId: Asset = Waves): Option[(Int, Long)] = {
    if (maybeDiff.isEmpty || h < this.height) {
      inner.balanceAtHeight(address, h, assetId)
    } else {
      val balance = this.balance(address, assetId)
      val bs      = height -> balance
      Some(bs)
    }
  }

  override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] = {
    if (maybeDiff.isEmpty || to.exists(id => inner.heightOf(id).isDefined)) {
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

  override def heightOf(blockId: ByteStr): Option[Int] = newBlock.filter(_.id() == blockId).map(_ => height) orElse inner.heightOf(blockId)

  /** Features related */
  override def approvedFeatures: Map[Short, Int] = inner.approvedFeatures

  override def activatedFeatures: Map[Short, Int] = inner.activatedFeatures

  override def featureVotes(height: Int): Map[Short, Int] = inner.featureVotes(height)

  /** Block reward related */
  override def blockReward(height: Int): Option[Long] = reward.filter(_ => this.height == height) orElse inner.blockReward(height)

  override def blockRewardVotes(height: Int): Seq[Long] = inner.blockRewardVotes(height)

  override def wavesAmount(height: Int): BigInt = inner.wavesAmount(height) + BigInt(reward.getOrElse(0L))

  override def hitSource(height: Int): Option[ByteStr] = hitSource.filter(_ => this.height == height) orElse inner.hitSource(height)

  override def continuationStates: Map[Address, ContinuationState] =
    inner.continuationStates ++ diff.continuationStates
}

object CompositeBlockchain {
  def apply(blockchain: Blockchain, ngState: NgState): CompositeBlockchain =
    CompositeBlockchain(blockchain, Some(ngState.bestLiquidDiff), Some(ngState.bestLiquidBlock), ngState.carryFee, ngState.reward)

  private def assetDescription(
      asset: IssuedAsset,
      diff: Diff,
      innerAssetDescription: => Option[AssetDescription],
      innerScript: => Option[AssetScriptInfo],
      height: Int
  ): Option[AssetDescription] = {
    val script = diff.assetScripts.getOrElse(asset, innerScript)

    val fromDiff = diff.issuedAssets
      .get(asset)
      .map {
        case NewAssetInfo(static, info, volume) =>
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
      innerAssetDescription
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
      diff.transactions.values
        .foldLeft(z.copy(script = script)) {
          case (acc, NewTransactionInfo(ut: UpdateAssetInfoTransaction, _, Succeeded)) if ut.assetId == asset =>
            acc.copy(name = ByteString.copyFromUtf8(ut.name), description = ByteString.copyFromUtf8(ut.description), lastUpdatedAt = Height(height))
          case (acc, _) => acc
        }
    }
  }
}
