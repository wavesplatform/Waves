package com.wavesplatform.state

import cats.syntax.option.*
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures.RideV6
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{AliasDoesNotExist, AliasIsDisabled}
import com.wavesplatform.transaction.transfer.{TransferTransaction, TransferTransactionLike}
import com.wavesplatform.transaction.{Asset, ERC20Address, Transaction}

case class SnapshotBlockchain(
    inner: Blockchain,
    maybeSnapshot: Option[StateSnapshot] = None,
    blockMeta: Option[(SignedBlockHeader, ByteStr)] = None,
    carry: Long = 0,
    reward: Option[Long] = None,
    stateHash: Option[ByteStr] = None
) extends Blockchain {
  override val settings: BlockchainSettings = inner.settings
  lazy val snapshot: StateSnapshot          = maybeSnapshot.orEmpty

  override def balance(address: Address, assetId: Asset): Long =
    snapshot.balances.getOrElse((address, assetId), inner.balance(address, assetId))

  override def balances(req: Seq[(Address, Asset)]): Map[(Address, Asset), Long] = {
    val (innerBalances, snapshotBalances) = req
      .foldLeft((Seq[(Address, Asset)](), Map[(Address, Asset), Long]())) { case ((innerBalances, snapshotBalances), key) =>
        snapshot.balances
          .get(key)
          .fold(
            (innerBalances :+ key, snapshotBalances)
          )(balance => (innerBalances, snapshotBalances + (key -> balance)))
      }
    inner.balances(innerBalances) ++ snapshotBalances
  }

  override def wavesBalances(addresses: Seq[Address]): Map[Address, Long] = {
    val (innerBalances, snapshotBalances) = addresses
      .foldLeft((Seq[Address](), Map[Address, Long]())) { case ((innerBalances, snapshotBalances), address) =>
        snapshot.balances
          .get((address, Waves))
          .fold(
            (innerBalances :+ address, snapshotBalances)
          )(balance => (innerBalances, snapshotBalances + (address -> balance)))
      }
    inner.wavesBalances(innerBalances) ++ snapshotBalances
  }

  override def effectiveBalanceBanHeights(address: Address): Seq[Int] = {
    val maybeLastBlockBan = blockMeta.flatMap(_._1.header.challengedHeader).map(_.generator.toAddress) match {
      case Some(generator) if address == generator => Seq(height)
      case _                                       => Seq.empty
    }
    maybeLastBlockBan ++ inner.effectiveBalanceBanHeights(address)
  }

  override def leaseBalance(address: Address): LeaseBalance =
    snapshot.leaseBalances.getOrElse(address, inner.leaseBalance(address))

  override def leaseBalances(addresses: Seq[Address]): Map[Address, LeaseBalance] = {
    val (innerBalances, snapshotBalances) = addresses
      .foldLeft((Seq[Address](), Map[Address, LeaseBalance]())) { case ((innerBalances, snapshotBalances), address) =>
        snapshot.leaseBalances
          .get(address)
          .fold(
            (innerBalances :+ address, snapshotBalances)
          )(balance => (innerBalances, snapshotBalances + (address -> balance)))
      }
    inner.leaseBalances(innerBalances) ++ snapshotBalances
  }

  override def assetScript(asset: IssuedAsset): Option[AssetScriptInfo] =
    maybeSnapshot
      .flatMap(_.assetScripts.get(asset))
      .orElse(inner.assetScript(asset))

  override def assetDescription(asset: IssuedAsset): Option[AssetDescription] =
    SnapshotBlockchain.assetDescription(asset, snapshot, height, inner)

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = {
    val newer = snapshot.newLeases.get(leaseId).map(n => LeaseDetails(n, LeaseDetails.Status.Active)).orElse(inner.leaseDetails(leaseId))
    snapshot.cancelledLeases.get(leaseId) match {
      case Some(newStatus) => newer.map(_.copy(status = newStatus))
      case None            => newer
    }
  }

  override def transferById(id: ByteStr): Option[(Int, TransferTransactionLike)] =
    snapshot.transactions
      .get(id)
      .collect { case NewTransactionInfo(tx: TransferTransaction, _, _, TxMeta.Status.Succeeded, _) =>
        (height, tx)
      }
      .orElse(inner.transferById(id))

  override def transactionInfo(id: ByteStr): Option[(TxMeta, Transaction)] =
    snapshot.transactions
      .get(id)
      .map(t => (TxMeta(Height(this.height), t.status, t.spentComplexity), t.transaction))
      .orElse(inner.transactionInfo(id))

  override def transactionInfos(ids: Seq[ByteStr]): Seq[Option[(TxMeta, Transaction)]] = {
    inner.transactionInfos(ids).zip(ids).map { case (info, id) =>
      snapshot.transactions
        .get(id)
        .map(t => (TxMeta(Height(this.height), t.status, t.spentComplexity), t.transaction))
        .orElse(info)
    }
  }

  override def transactionMeta(id: ByteStr): Option[TxMeta] =
    snapshot.transactions
      .get(id)
      .map(t => TxMeta(Height(this.height), t.status, t.spentComplexity))
      .orElse(inner.transactionMeta(id))

  override def transactionSnapshot(id: ByteStr): Option[(StateSnapshot, Status)] =
    snapshot.transactions
      .get(id)
      .map(tx => (tx.snapshot, tx.status))
      .orElse(inner.transactionSnapshot(id))

  override def height: Int = inner.height + blockMeta.fold(0)(_ => 1)

  override def resolveAlias(alias: Alias): Either[ValidationError, Address] = inner.resolveAlias(alias) match {
    case l @ Left(AliasIsDisabled(_)) => l
    case Right(addr)                  => Right(snapshot.aliases.getOrElse(alias, addr))
    case Left(_)                      => snapshot.aliases.get(alias).toRight(AliasDoesNotExist(alias))
  }

  override def containsTransaction(tx: Transaction): Boolean =
    snapshot.transactions.contains(tx.id()) || inner.containsTransaction(tx)

  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee =
    snapshot.orderFills.getOrElse(orderId, inner.filledVolumeAndFee(orderId))

  override def balanceAtHeight(address: Address, h: Int, assetId: Asset = Waves): Option[(Int, Long)] =
    if (maybeSnapshot.isEmpty || h < this.height) {
      inner.balanceAtHeight(address, h, assetId)
    } else {
      val balance = this.balance(address, assetId)
      val bs      = height -> balance
      Some(bs)
    }

  override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] = {
    val from1 = math.max(from, 1)

    if (maybeSnapshot.isEmpty || to.exists(id => inner.heightOf(id).isDefined)) {
      inner.balanceSnapshots(address, from1, to)
    } else {
      val balance    = this.balance(address)
      val lease      = this.leaseBalance(address)
      val bs         = BalanceSnapshot(height, Portfolio(balance, lease))
      val height2Fix = this.height == 2 && from1 < 2 && inner.isFeatureActivated(RideV6)
      if (inner.height > 0 && (from1 < this.height - 1 || height2Fix))
        bs +: inner.balanceSnapshots(address, from1, to)
      else
        Seq(bs)
    }
  }

  override def accountScript(address: Address): Option[AccountScriptInfo] =
    snapshot.accountScriptsByAddress.get(address) match {
      case None            => inner.accountScript(address)
      case Some(None)      => None
      case Some(Some(scr)) => Some(scr)
    }

  override def hasAccountScript(address: Address): Boolean =
    snapshot.accountScriptsByAddress.get(address) match {
      case None          => inner.hasAccountScript(address)
      case Some(None)    => false
      case Some(Some(_)) => true
    }

  override def accountData(acc: Address, key: String): Option[DataEntry[?]] =
    (for {
      d <- snapshot.accountData.get(acc)
      e <- d.get(key)
    } yield e).orElse(inner.accountData(acc, key)).filterNot(_.isEmpty)

  override def hasData(acc: Address): Boolean = {
    snapshot.accountData.contains(acc) || inner.hasData(acc)
  }

  override def carryFee(refId: Option[ByteStr]): Long = carry

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
      .orElse(snapshot.erc20Addresses.get(address))

  override def lastStateHash(refId: Option[ByteStr]): BlockId =
    stateHash.orElse(blockMeta.flatMap(_._1.header.stateHash)).getOrElse(inner.lastStateHash(refId))
}

object SnapshotBlockchain {
  def apply(inner: Blockchain, ngState: NgState): SnapshotBlockchain =
    new SnapshotBlockchain(
      inner,
      Some(ngState.bestLiquidSnapshot),
      Some(SignedBlockHeader(ngState.bestLiquidBlock.header, ngState.bestLiquidBlock.signature) -> ngState.hitSource),
      ngState.carryFee,
      ngState.reward,
      Some(ngState.bestLiquidComputedStateHash)
    )

  def apply(inner: Blockchain, reward: Option[Long]): SnapshotBlockchain =
    new SnapshotBlockchain(inner, carry = inner.carryFee(None), reward = reward)

  def apply(inner: Blockchain, snapshot: StateSnapshot): SnapshotBlockchain =
    new SnapshotBlockchain(inner, Some(snapshot))

  def apply(
      inner: Blockchain,
      snapshot: StateSnapshot,
      newBlock: Block,
      hitSource: ByteStr,
      carry: Long,
      reward: Option[Long],
      stateHash: Option[ByteStr]
  ): SnapshotBlockchain =
    new SnapshotBlockchain(inner, Some(snapshot), Some(SignedBlockHeader(newBlock.header, newBlock.signature) -> hitSource), carry, reward, stateHash)

  private def assetDescription(
      asset: IssuedAsset,
      snapshot: StateSnapshot,
      height: Int,
      inner: Blockchain
  ): Option[AssetDescription] = {
    lazy val volume      = snapshot.assetVolumes.get(asset)
    lazy val info        = snapshot.assetNamesAndDescriptions.get(asset)
    lazy val sponsorship = snapshot.sponsorships.get(asset).map(_.minFee)
    lazy val script      = snapshot.assetScripts.get(asset)
    snapshot.assetStatics
      .get(asset)
      .map { case (static, assetNum) =>
        AssetDescription(
          static.source,
          static.issuer,
          info.get.name,
          info.get.description,
          static.decimals,
          volume.get.isReissuable,
          volume.get.volume,
          info.get.lastUpdatedAt,
          script,
          sponsorship.getOrElse(0),
          static.nft,
          assetNum,
          Height @@ height
        )
      }
      .orElse(
        inner
          .assetDescription(asset)
          .map(d =>
            d.copy(
              totalVolume = volume.map(_.volume).getOrElse(d.totalVolume),
              reissuable = volume.map(_.isReissuable).getOrElse(d.reissuable),
              name = info.map(_.name).getOrElse(d.name),
              description = info.map(_.description).getOrElse(d.description),
              lastUpdatedAt = info.map(_.lastUpdatedAt).getOrElse(d.lastUpdatedAt),
              sponsorship = sponsorship.getOrElse(d.sponsorship),
              script = script.orElse(d.script)
            )
          )
      )
  }
}
