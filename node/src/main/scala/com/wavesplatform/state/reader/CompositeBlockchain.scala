package com.wavesplatform.state.reader

import cats.Id
import cats.data.Ior
import cats.syntax.option.*
import cats.syntax.semigroup.*
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures.RideV6
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{AliasDoesNotExist, AliasIsDisabled}
import com.wavesplatform.transaction.transfer.{TransferTransaction, TransferTransactionLike}
import com.wavesplatform.transaction.{Asset, ERC20Address, Transaction}

final class CompositeBlockchain private (
    inner: Blockchain,
    maybeDiff: Option[Diff] = None,
    blockMeta: Option[(SignedBlockHeader, ByteStr)] = None,
    carry: Long = 0,
    reward: Option[Long] = None
) extends Blockchain {
  override val settings: BlockchainSettings = inner.settings

  private[CompositeBlockchain] def appendDiff(newDiff: Diff) =
    new CompositeBlockchain(inner, Some(this.maybeDiff.fold(newDiff)(_.combineF(newDiff).explicitGet())), blockMeta, carry, reward)

  def diff: Diff = maybeDiff.getOrElse(Diff.empty)

  override def balance(address: Address, assetId: Asset): Long =
    inner.balance(address, assetId) + diff.portfolios.get(address).fold(0L)(_.balanceOf(assetId))

  override def leaseBalance(address: Address): LeaseBalance =
    inner.leaseBalance(address).combineF[Id](diff.portfolios.getOrElse(address, Portfolio.empty).lease)

  override def assetScript(asset: IssuedAsset): Option[AssetScriptInfo] =
    maybeDiff
      .flatMap(_.assetScripts.get(asset))
      .getOrElse(inner.assetScript(asset))

  override def assetDescription(asset: IssuedAsset): Option[AssetDescription] =
    CompositeBlockchain.assetDescription(asset, maybeDiff.getOrElse(Diff.empty), inner.assetDescription(asset))

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] =
    inner
      .leaseDetails(leaseId)
      .map(ld => ld.copy(status = diff.leaseState.get(leaseId).map(_.status).getOrElse(ld.status)))
      .orElse(diff.leaseState.get(leaseId))

  override def transferById(id: ByteStr): Option[(Int, TransferTransactionLike)] =
    diff.transactions
      .get(id)
      .collect { case NewTransactionInfo(tx: TransferTransaction, _, true, _) =>
        (height, tx)
      }
      .orElse(inner.transferById(id))

  override def transactionInfo(id: ByteStr): Option[(TxMeta, Transaction)] =
    diff.transactions
      .get(id)
      .map(t => (TxMeta(Height(this.height), t.applied, t.spentComplexity), t.transaction))
      .orElse(inner.transactionInfo(id))

  override def transactionMeta(id: ByteStr): Option[TxMeta] =
    diff.transactions
      .get(id)
      .map(t => TxMeta(Height(this.height), t.applied, t.spentComplexity))
      .orElse(inner.transactionMeta(id))

  override def height: Int = inner.height + blockMeta.fold(0)(_ => 1)

  override def resolveAlias(alias: Alias): Either[ValidationError, Address] = inner.resolveAlias(alias) match {
    case l @ Left(AliasIsDisabled(_)) => l
    case Right(addr)                  => Right(diff.aliases.getOrElse(alias, addr))
    case Left(_)                      => diff.aliases.get(alias).toRight(AliasDoesNotExist(alias))
  }

  override def containsTransaction(tx: Transaction): Boolean = diff.transactions.contains(tx.id()) || inner.containsTransaction(tx)

  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee =
    diff.orderFills.get(orderId).orEmpty.combine(inner.filledVolumeAndFee(orderId))

  override def balanceAtHeight(address: Address, h: Int, assetId: Asset = Waves): Option[(Int, Long)] =
    if (maybeDiff.isEmpty || h < this.height) {
      inner.balanceAtHeight(address, h, assetId)
    } else {
      val balance = this.balance(address, assetId)
      val bs      = height -> balance
      Some(bs)
    }

  override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] =
    if (maybeDiff.isEmpty || to.exists(id => inner.heightOf(id).isDefined)) {
      inner.balanceSnapshots(address, from, to)
    } else {
      val balance    = this.balance(address)
      val lease      = this.leaseBalance(address)
      val bs         = BalanceSnapshot(height, Portfolio(balance, lease))
      val height2Fix = this.height == 1 && inner.isFeatureActivated(RideV6) && from < this.height + 1
      if (inner.height > 0 && (from < this.height || height2Fix))
        bs +: inner.balanceSnapshots(address, from, to)
      else
        Seq(bs)
    }

  override def accountScript(address: Address): Option[AccountScriptInfo] =
    diff.scripts.get(address) match {
      case None            => inner.accountScript(address)
      case Some(None)      => None
      case Some(Some(scr)) => Some(scr)
    }

  override def hasAccountScript(address: Address): Boolean =
    diff.scripts.get(address) match {
      case None          => inner.hasAccountScript(address)
      case Some(None)    => false
      case Some(Some(_)) => true
    }

  override def accountData(acc: Address, key: String): Option[DataEntry[?]] =
    diff.accountData.get(acc).orEmpty.data.get(key).orElse(inner.accountData(acc, key)).filterNot(_.isEmpty)

  override def hasData(acc: Address): Boolean = {
    diff.accountData.contains(acc) || inner.hasData(acc)
  }

  override def carryFee: Long = carry

  override def score: BigInt = blockMeta.fold(BigInt(0))(_._1.header.score()) + inner.score

  override def blockHeader(height: Int): Option[SignedBlockHeader] =
    blockMeta match {
      case Some((header, _)) if this.height == height => Some(header)
      case _                                          => inner.blockHeader(height)
    }

  override def heightOf(blockId: ByteStr): Option[Int] = blockMeta.filter(_._1.id() == blockId).map(_ => height) orElse inner.heightOf(blockId)

  /** Features related */
  override def approvedFeatures: Map[Short, Int] = inner.approvedFeatures

  override def activatedFeatures: Map[Short, Int] = inner.activatedFeatures

  override def featureVotes(height: Int): Map[Short, Int] = inner.featureVotes(height)

  /** Block reward related */
  override def blockReward(height: Int): Option[Long] = reward.filter(_ => this.height == height) orElse inner.blockReward(height)

  override def blockRewardVotes(height: Int): Seq[Long] = inner.blockRewardVotes(height)

  override def wavesAmount(height: Int): BigInt = inner.wavesAmount(height) + BigInt(reward.getOrElse(0L))

  override def hitSource(height: Int): Option[ByteStr] =
    blockMeta
      .collect { case (_, hitSource) if this.height == height => hitSource }
      .orElse(inner.hitSource(height))

  override def resolveERC20Address(address: ERC20Address): Option[IssuedAsset] =
    inner
      .resolveERC20Address(address)
      .orElse(diff.issuedAssets.keys.find(id => ERC20Address(id) == address))
}

object CompositeBlockchain {
  def apply(inner: Blockchain, ngState: NgState): CompositeBlockchain =
    new CompositeBlockchain(
      inner,
      Some(ngState.bestLiquidDiff),
      Some(SignedBlockHeader(ngState.bestLiquidBlock.header, ngState.bestLiquidBlock.signature) -> ngState.hitSource),
      ngState.carryFee,
      ngState.reward
    )

  def apply(inner: Blockchain, reward: Option[Long]): CompositeBlockchain =
    new CompositeBlockchain(inner, carry = inner.carryFee, reward = reward)

  def apply(inner: Blockchain, diff: Diff): CompositeBlockchain =
    inner match {
      case cb: CompositeBlockchain => cb.appendDiff(diff)
      case _                       => new CompositeBlockchain(inner, Some(diff))
    }

  def apply(
      inner: Blockchain,
      diff: Diff,
      newBlock: Block,
      hitSource: ByteStr,
      carry: Long,
      reward: Option[Long]
  ): CompositeBlockchain =
    new CompositeBlockchain(inner, Some(diff), Some(SignedBlockHeader(newBlock.header, newBlock.signature) -> hitSource), carry, reward)

  private def assetDescription(
      asset: IssuedAsset,
      diff: Diff,
      innerAssetDescription: => Option[AssetDescription]
  ): Option[AssetDescription] = {
    diff.issuedAssets
      .get(asset)
      .map { case NewAssetInfo(static, info, volume) =>
        AssetDescription(
          static.source,
          static.issuer,
          info.name,
          info.description,
          static.decimals,
          volume.isReissuable,
          volume.volume,
          info.lastUpdatedAt,
          None,
          0L,
          static.nft
        )
      }
      .orElse(innerAssetDescription)
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
      .map { description =>
        diff.assetScripts
          .get(asset)
          .fold(description)(asi => description.copy(script = asi))
      }
  }
}
