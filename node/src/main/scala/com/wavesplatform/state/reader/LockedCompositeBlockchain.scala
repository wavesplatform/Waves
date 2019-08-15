package com.wavesplatform.state.reader

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.{AccountDataInfo, AssetDescription, BalanceSnapshot, DataEntry, InvokeScriptResult, LeaseBalance, Portfolio, TransactionId, VolumeAndFee}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, Transaction}

trait LockedCompositeBlockchain extends CompositeBlockchain {
  protected def readLock[B](f: => B): B

  override def balance(address: Address, assetId: Asset): Long = readLock(super.balance(address, assetId))
  override def leaseBalance(address: Address): LeaseBalance = readLock(super.leaseBalance(address))
  override def assetScript(asset: IssuedAsset): Option[Script] = readLock(super.assetScript(asset))
  override def hasAssetScript(asset: IssuedAsset): Boolean = readLock(super.hasAssetScript(asset))
  override def assetDescription(asset: IssuedAsset): Option[AssetDescription] = readLock(super.assetDescription(asset))
  override def leaseDetails(leaseId: BlockId): Option[LeaseDetails] = readLock(super.leaseDetails(leaseId))
  override def transferById(id: BlockId): Option[(Int, TransferTransaction)] = readLock(super.transferById(id))
  override def transactionInfo(id: BlockId): Option[(Int, Transaction)] = readLock(super.transactionInfo(id))
  override def transactionHeight(id: BlockId): Option[Int] = readLock(super.transactionHeight(id))
  override def height: Int = readLock(super.height)
  override def resolveAlias(alias: Alias): Either[ValidationError, Address] = readLock(super.resolveAlias(alias))
  override def collectActiveLeases[T](pf: PartialFunction[LeaseTransaction, T]): Seq[T] = readLock(super.collectActiveLeases(pf))
  override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] = readLock(super.collectLposPortfolios(pf))
  override def invokeScriptResult(txId: TransactionId): Either[ValidationError, InvokeScriptResult] = readLock(super.invokeScriptResult(txId))
  override def containsTransaction(tx: Transaction): Boolean = readLock(super.containsTransaction(tx))
  override def filledVolumeAndFee(orderId: BlockId): VolumeAndFee = readLock(super.filledVolumeAndFee(orderId))
  override def balanceSnapshots(address: Address, from: Int, to: BlockId): Seq[BalanceSnapshot] = readLock(super.balanceSnapshots(address, from, to))
  override def accountScript(address: Address): Option[Script] = readLock(super.accountScript(address))
  override def hasScript(address: Address): Boolean = readLock(super.hasScript(address))
  override def accountDataKeys(acc: Address): Seq[String] = readLock(super.accountDataKeys(acc))
  override def accountData(acc: Address): AccountDataInfo = readLock(super.accountData(acc))
  override def accountData(acc: Address, key: String): Option[DataEntry[_]] = readLock(super.accountData(acc, key))
  override def lastBlock: Option[Block] = readLock(super.lastBlock)
  override def score: BigInt = readLock(super.score)
  override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)] = readLock(super.blockHeaderAndSize(height))
  override def blockHeaderAndSize(blockId: BlockId): Option[(BlockHeader, Int)] = readLock(super.blockHeaderAndSize(blockId))
  override def blockBytes(height: Int): Option[Array[Byte]] = readLock(super.blockBytes(height))
  override def blockBytes(blockId: BlockId): Option[Array[Byte]] = readLock(super.blockBytes(blockId))
  override def heightOf(blockId: BlockId): Option[Int] = readLock(super.heightOf(blockId))
  override def lastBlockIds(howMany: Int): Seq[BlockId] = readLock(super.lastBlockIds(howMany))
  override def blockIdsAfter(parentSignature: BlockId, howMany: Int): Option[Seq[BlockId]] = readLock(super.blockIdsAfter(parentSignature, howMany))
  override def parentHeader(block: BlockHeader, back: Int): Option[BlockHeader] = readLock(super.parentHeader(block, back))
  override def totalFee(height: Int): Option[Long] = readLock(super.totalFee(height))
  override def approvedFeatures: Map[Short, Int] = readLock(super.approvedFeatures)
  override def activatedFeatures: Map[Short, Int] = readLock(super.activatedFeatures)
  override def featureVotes(height: Int): Map[Short, Int] = readLock(super.featureVotes(height))
}
