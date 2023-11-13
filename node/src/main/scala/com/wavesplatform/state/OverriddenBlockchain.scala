package com.wavesplatform.state

import cats.syntax.option.*
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.transaction.{Asset, ERC20Address, Transaction}

class OverriddenBlockchain(underlying: Blockchain, overrides: BlockchainOverrides) extends Blockchain {
  override def settings: BlockchainSettings = underlying.settings

  override def hasData(address: Address): Boolean = underlying.hasData(address)

  override def accountData(acc: Address, key: String): Option[DataEntry[?]] = underlying.accountData(acc, key)

  override def accountScript(address: Address): Option[AccountScriptInfo] = underlying.accountScript(address)

  override def hasAccountScript(address: Address): Boolean = accountScript(address).nonEmpty

  override def blockHeader(height: Int): Option[SignedBlockHeader] = underlying.blockHeader(height)

  override def hitSource(height: Int): Option[ByteStr] = underlying.hitSource(height)

  override def height: Int = underlying.height

  override def activatedFeatures: Map[Short, Int] = underlying.activatedFeatures

  override def assetDescription(id: Asset.IssuedAsset): Option[AssetDescription] = underlying.assetDescription(id)

  override def assetScript(id: Asset.IssuedAsset): Option[AssetScriptInfo] = underlying.assetScript(id)

  override def resolveAlias(a: Alias): Either[ValidationError, Address] = underlying.resolveAlias(a)

  override def leaseBalance(address: Address): LeaseBalance = underlying.leaseBalance(address)

  override def balance(address: Address, mayBeAssetId: Asset): Long =
    overrides.balance(address, mayBeAssetId).getOrElse(underlying.balance(address, mayBeAssetId))

  override def transactionInfos(ids: Seq[BlockId]): Seq[Option[(TxMeta, Transaction)]] = underlying.transactionInfos(ids)

  override def leaseBalances(addresses: Seq[Address]): Map[Address, LeaseBalance] = underlying.leaseBalances(addresses)

  override def balances(req: Seq[(Address, Asset)]): Map[(Address, Asset), Long] =
    req.map { case k@(addr, asset) => k -> overrides.balance(addr, asset).getOrElse(underlying.balance(addr, asset)) }.toMap

  override def wavesBalances(addresses: Seq[Address]): Map[Address, Long] =
    addresses.map(addr => addr -> overrides.balance(addr, Asset.Waves).getOrElse(underlying.balance(addr))).toMap

  override def effectiveBalanceBanHeights(address: Address): Seq[Int] = underlying.effectiveBalanceBanHeights(address)

  override def lastStateHash(refId: Option[BlockId]): BlockId = underlying.lastStateHash(refId)

  // Ride: wavesBalance (specifies to=None)
  /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
  override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] = {
    val orig     = underlying.balanceSnapshots(address, from, to)
    val toHeight = to.flatMap(this.heightOf).getOrElse(this.height)
    if (toHeight < this.height) orig
    else
      overrides
        .balance(address, Asset.Waves)
        .fold(orig) { regularBalance =>
          orig.headOption match {
            case None         => Seq(BalanceSnapshot(toHeight, regularBalance, 0, 0))
            case Some(latest) => latest.copy(toHeight, regularBalance) +: orig
          }
        }
  }

  override def balanceAtHeight(address: Address, height: Int, assetId: Asset): Option[(Int, Long)] = {
    lazy val orig = underlying.balanceAtHeight(address, height, assetId)
    if (height < this.height) orig
    else overrides.balance(address, assetId).fold(orig)(b => (height, b).some)
  }

  override def transactionMeta(id: ByteStr): Option[TxMeta] = underlying.transactionMeta(id)

  override def transferById(id: ByteStr): Option[(Int, TransferTransactionLike)] = underlying.transferById(id)

  override def score: BigInt = underlying.score

  override def carryFee(refId: Option[BlockId]): Long = underlying.carryFee(refId)

  override def heightOf(blockId: ByteStr): Option[Int] = underlying.heightOf(blockId)

  override def approvedFeatures: Map[Short, Int] = underlying.approvedFeatures

  override def featureVotes(height: Int): Map[Short, Int] = underlying.featureVotes(height)

  override def containsTransaction(tx: Transaction): Boolean = underlying.containsTransaction(tx)

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = underlying.leaseDetails(leaseId)

  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee = underlying.filledVolumeAndFee(orderId)

  override def transactionInfo(id: BlockId): Option[(TxMeta, Transaction)] = underlying.transactionInfo(id)

  override def blockReward(height: Int): Option[Long] = underlying.blockReward(height)

  override def blockRewardVotes(height: Int): Seq[Long] = underlying.blockRewardVotes(height)

  override def wavesAmount(height: Int): BigInt = underlying.wavesAmount(height)

  override def resolveERC20Address(address: ERC20Address): Option[Asset.IssuedAsset] = underlying.resolveERC20Address(address)
}
