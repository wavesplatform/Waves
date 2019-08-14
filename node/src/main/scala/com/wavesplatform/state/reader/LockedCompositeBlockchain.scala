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

trait LockedCompositeBlockchain { self: CompositeBlockchain =>
  protected def readLock[B](f: => B): B

  override def balance(address: Address, assetId: Asset): Long = readLock(self.balance(address, assetId))
  override def leaseBalance(address: Address): LeaseBalance = readLock(self.leaseBalance(address))
  override def assetScript(asset: IssuedAsset): Option[Script] = readLock(self.assetScript(asset))
  override def hasAssetScript(asset: IssuedAsset): Boolean = readLock(self.hasAssetScript(asset))
  override def assetDescription(asset: IssuedAsset): Option[AssetDescription] = readLock(self.assetDescription(asset))
  override def leaseDetails(leaseId: BlockId): Option[LeaseDetails] = readLock(self.leaseDetails(leaseId))
  override def transferById(id: BlockId): Option[(Int, TransferTransaction)] = readLock(self.transferById(id))
  override def transactionInfo(id: BlockId): Option[(Int, Transaction)] = readLock(self.transactionInfo(id))
  override def transactionHeight(id: BlockId): Option[Int] = readLock(self.transactionHeight(id))
  override def height: Int = readLock(self.height)
  override def resolveAlias(alias: Alias): Either[ValidationError, Address] = readLock(self.resolveAlias(alias))
  override def collectActiveLeases[T](pf: PartialFunction[LeaseTransaction, T]): Seq[T] = readLock(self.collectActiveLeases(pf))
  override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] = readLock(self.collectLposPortfolios(pf))
  override def invokeScriptResult(txId: TransactionId): Either[ValidationError, InvokeScriptResult] = readLock(self.invokeScriptResult(txId))
  override def containsTransaction(tx: Transaction): Boolean = readLock(self.containsTransaction(tx))
  override def filledVolumeAndFee(orderId: BlockId): VolumeAndFee = readLock(self.filledVolumeAndFee(orderId))
  override def balanceSnapshots(address: Address, from: Int, to: BlockId): Seq[BalanceSnapshot] = readLock(self.balanceSnapshots(address, from, to))
  override def accountScript(address: Address): Option[Script] = readLock(self.accountScript(address))
  override def hasScript(address: Address): Boolean = readLock(self.hasScript(address))
  override def accountDataKeys(acc: Address): Seq[String] = readLock(self.accountDataKeys(acc))
  override def accountData(acc: Address): AccountDataInfo = readLock(self.accountData(acc))
  override def accountData(acc: Address, key: String): Option[DataEntry[_]] = readLock(self.accountData(acc, key))
  override def lastBlock: Option[Block] = readLock(self.lastBlock)
  override def score: BigInt = readLock(self.score)
  override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)] = readLock(self.blockHeaderAndSize(height))
  override def blockHeaderAndSize(blockId: BlockId): Option[(BlockHeader, Int)] = readLock(self.blockHeaderAndSize(blockId))
  override def blockBytes(height: Int): Option[Array[Byte]] = readLock(self.blockBytes(height))
  override def blockBytes(blockId: BlockId): Option[Array[Byte]] = readLock(self.blockBytes(blockId))
  override def heightOf(blockId: BlockId): Option[Int] = readLock(self.heightOf(blockId))
  override def lastBlockIds(howMany: Int): Seq[BlockId] = readLock(self.lastBlockIds(howMany))
  override def blockIdsAfter(parentSignature: BlockId, howMany: Int): Option[Seq[BlockId]] = readLock(self.blockIdsAfter(parentSignature, howMany))
  override def parentHeader(block: BlockHeader, back: Int): Option[BlockHeader] = readLock(self.parentHeader(block, back))
  override def totalFee(height: Int): Option[Long] = readLock(self.totalFee(height))
  override def approvedFeatures: Map[Short, Int] = readLock(self.approvedFeatures)
  override def activatedFeatures: Map[Short, Int] = readLock(self.activatedFeatures)
  override def featureVotes(height: Int): Map[Short, Int] = readLock(self.featureVotes(height))
}
