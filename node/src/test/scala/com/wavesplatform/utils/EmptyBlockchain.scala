package com.wavesplatform.utils

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.*
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.transaction.{Asset, ERC20Address, Transaction}

trait EmptyBlockchain extends Blockchain {
  override lazy val settings: BlockchainSettings = BlockchainSettings.fromRootConfig(ConfigFactory.load())

  override def height: Int = 0

  override def score: BigInt = 0

  override def blockHeader(height: Int): Option[SignedBlockHeader] = None

  override def hitSource(height: Int): Option[ByteStr] = None

  override def carryFee(refId: Option[ByteStr]): Long = 0

  override def heightOf(blockId: ByteStr): Option[Int] = None

  /** Features related */
  override def approvedFeatures: Map[Short, Int] = Map.empty

  override def activatedFeatures: Map[Short, Int] = Map.empty

  override def featureVotes(height: Int): Map[Short, Int] = Map.empty

  /** Block reward related */
  override def blockReward(height: Int): Option[Long] = None

  override def blockRewardVotes(height: Int): Seq[Long] = Seq.empty

  override def wavesAmount(height: Int): BigInt = 0

  override def transferById(id: ByteStr): Option[(Int, TransferTransactionLike)] = None

  override def transactionInfo(id: ByteStr): Option[(TxMeta, Transaction)] = None

  override def transactionInfos(ids: Seq[ByteStr]): Seq[Option[(TxMeta, Transaction)]] = Seq.empty

  override def transactionMeta(id: ByteStr): Option[TxMeta] = None

  override def containsTransaction(tx: Transaction): Boolean = false

  override def assetDescription(id: IssuedAsset): Option[AssetDescription] = None

  override def resolveAlias(a: Alias): Either[ValidationError, Address] = Left(GenericError("Empty blockchain"))

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = None

  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee = VolumeAndFee(0, 0)

  /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
  override def balanceAtHeight(address: Address, height: Int, assetId: Asset = Waves): Option[(Int, Long)] = Option.empty
  override def balanceSnapshots(address: Address, from: Int, to: Option[ByteStr]): Seq[BalanceSnapshot]    = Seq.empty

  override def accountScript(address: Address): Option[AccountScriptInfo] = None

  override def hasAccountScript(address: Address): Boolean = false

  override def assetScript(asset: IssuedAsset): Option[AssetScriptInfo] = None

  override def accountData(acc: Address, key: String): Option[DataEntry[?]] = None

  override def hasData(acc: Address): Boolean = false

  override def balance(address: Address, mayBeAssetId: Asset): Long = 0

  override def balances(req: Seq[(Address, Asset)]): Map[(Address, Asset), Long] = Map.empty

  override def wavesBalances(addresses: Seq[Address]): Map[Address, Long] = Map.empty

  override def effectiveBalanceBanHeights(address: Address): Seq[Int] = Seq.empty

  override def leaseBalance(address: Address): LeaseBalance = LeaseBalance.empty

  override def leaseBalances(addresses: Seq[Address]): Map[Address, LeaseBalance] = Map.empty

  override def resolveERC20Address(address: ERC20Address): Option[IssuedAsset] = None

  override def prevStateHash(refId: Option[ByteStr]): ByteStr = TxStateSnapshotHashBuilder.InitStateHash
}

object EmptyBlockchain extends EmptyBlockchain
