package com.wavesplatform.state

import com.wavesplatform.account.{Address, AddressOrAlias, Alias}
import com.wavesplatform.block.Block.{BlockId, GenesisBlockVersion, NgBlockVersion, PlainBlockVersion, ProtoBlockVersion, RewardBlockVersion}
import com.wavesplatform.block.{Block, BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.GeneratingBalanceProvider
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatureStatus, BlockchainFeatures}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.traits.domain.Issue
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.AliasDoesNotExist
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, Transaction}

trait Blockchain {
  def settings: BlockchainSettings

  def height: Int
  def score: BigInt

  def blockHeader(height: Int): Option[SignedBlockHeader]
  def hitSource(height: Int): Option[ByteStr]

  def carryFee: Long

  def heightOf(blockId: ByteStr): Option[Int]

  /** Features related */
  def approvedFeatures: Map[Short, Int]
  def activatedFeatures: Map[Short, Int]
  def featureVotes(height: Int): Map[Short, Int]

  /** Block reward related */
  def blockReward(height: Int): Option[Long]
  def blockRewardVotes(height: Int): Seq[Long]

  def wavesAmount(height: Int): BigInt

  def transferById(id: ByteStr): Option[(Int, TransferTransaction)]
  def transactionInfo(id: ByteStr): Option[(Int, Transaction, Boolean)]
  def transactionMeta(id: ByteStr): Option[(Int, Boolean)]

  def containsTransaction(tx: Transaction): Boolean

  def assetDescription(id: IssuedAsset): Option[AssetDescription]

  def resolveAlias(a: Alias): Either[ValidationError, Address]

  def leaseDetails(leaseId: ByteStr): Option[LeaseDetails]

  def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee

  def balanceAtHeight(address: Address, height: Int, assetId: Asset = Waves): Option[(Int, Long)]

  /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
  def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot]

  def accountScript(address: Address): Option[AccountScriptInfo]
  def hasAccountScript(address: Address): Boolean

  def assetScript(id: IssuedAsset): Option[AssetScriptInfo]

  def accountData(acc: Address, key: String): Option[DataEntry[_]]

  def leaseBalance(address: Address): LeaseBalance

  def balance(address: Address, mayBeAssetId: Asset = Waves): Long
}

object Blockchain {
  implicit class BlockchainExt(private val blockchain: Blockchain) extends AnyVal {
    def isEmpty: Boolean = blockchain.height == 0

    def parentHeader(block: BlockHeader, back: Int = 1): Option[BlockHeader] =
      blockchain
        .heightOf(block.reference)
        .map(_ - (back - 1).max(0))
        .flatMap(h => blockchain.blockHeader(h).map(_.header))

    def contains(block: Block): Boolean     = blockchain.contains(block.id())
    def contains(blockId: ByteStr): Boolean = blockchain.heightOf(blockId).isDefined

    def blockId(atHeight: Int): Option[ByteStr] = blockchain.blockHeader(atHeight).map(_.id())

    def lastBlockHeader: Option[SignedBlockHeader] = blockchain.blockHeader(blockchain.height)
    def lastBlockId: Option[ByteStr]               = lastBlockHeader.map(_.id())
    def lastBlockTimestamp: Option[Long]           = lastBlockHeader.map(_.header.timestamp)
    def lastBlockIds(howMany: Int): Seq[ByteStr]   = (blockchain.height to blockchain.height - howMany by -1).flatMap(blockId)

    def resolveAlias(aoa: AddressOrAlias): Either[ValidationError, Address] =
      aoa match {
        case a: Address => Right(a)
        case a: Alias   => blockchain.resolveAlias(a)
      }

    def canCreateAlias(alias: Alias): Boolean = blockchain.resolveAlias(alias) match {
      case Left(AliasDoesNotExist(_)) => true
      case _                          => false
    }

    def effectiveBalance(address: Address, confirmations: Int, block: Option[BlockId] = blockchain.lastBlockId): Long = {
      val blockHeight = block.flatMap(b => blockchain.heightOf(b)).getOrElse(blockchain.height)
      val bottomLimit = (blockHeight - confirmations + 1).max(1).min(blockHeight)
      val balances    = blockchain.balanceSnapshots(address, bottomLimit, block)
      if (balances.isEmpty) 0L else balances.view.map(_.effectiveBalance).min
    }

    def balance(address: Address, atHeight: Int, confirmations: Int): Long = {
      val bottomLimit = (atHeight - confirmations + 1).max(1).min(atHeight)
      val blockId     = blockchain.blockHeader(atHeight).getOrElse(throw new IllegalArgumentException(s"Invalid block height: $atHeight")).id()
      val balances    = blockchain.balanceSnapshots(address, bottomLimit, Some(blockId))
      if (balances.isEmpty) 0L else balances.view.map(_.regularBalance).min
    }

    def unsafeHeightOf(id: ByteStr): Int =
      blockchain
        .heightOf(id)
        .getOrElse(throw new IllegalStateException(s"Can't find a block: $id"))

    def wavesPortfolio(address: Address): Portfolio = Portfolio(
      blockchain.balance(address),
      blockchain.leaseBalance(address),
      Map.empty
    )

    def isMiningAllowed(height: Int, effectiveBalance: Long): Boolean =
      GeneratingBalanceProvider.isMiningAllowed(blockchain, height, effectiveBalance)

    def isEffectiveBalanceValid(height: Int, block: Block, effectiveBalance: Long): Boolean =
      GeneratingBalanceProvider.isEffectiveBalanceValid(blockchain, height, block, effectiveBalance)

    def generatingBalance(account: Address, blockId: Option[BlockId] = None): Long =
      GeneratingBalanceProvider.balance(blockchain, account, blockId)

    def lastBlockReward: Option[Long] = blockchain.blockReward(blockchain.height)

    def hasAssetScript(asset: IssuedAsset): Boolean = blockchain.assetScript(asset).isDefined

    def vrf(atHeight: Int): Option[ByteStr] =
      blockchain
        .blockHeader(atHeight)
        .flatMap(header => if (header.header.version >= Block.ProtoBlockVersion) blockchain.hitSource(atHeight) else None)

    def isNFT(issueTransaction: IssueTransaction): Boolean = isNFT(issueTransaction.quantity, issueTransaction.decimals, issueTransaction.reissuable)
    def isNFT(issueAction: Issue): Boolean                 = isNFT(issueAction.quantity, issueAction.decimals, issueAction.isReissuable)
    def isNFT(quantity: Long, decimals: Int, reissuable: Boolean): Boolean =
      isFeatureActivated(BlockchainFeatures.ReduceNFTFee) && quantity == 1 && decimals == 0 && !reissuable

    def isFeatureActivated(feature: BlockchainFeature, height: Int = blockchain.height): Boolean =
      blockchain.activatedFeatures.get(feature.id).exists(_ <= height)

    def activatedFeaturesAt(height: Int): Set[Short] =
      blockchain.activatedFeatures.collect {
        case (featureId, activationHeight) if height >= activationHeight => featureId
      }.toSet

    def featureStatus(feature: Short, height: Int): BlockchainFeatureStatus =
      if (blockchain.activatedFeatures.get(feature).exists(_ <= height)) BlockchainFeatureStatus.Activated
      else if (blockchain.approvedFeatures.get(feature).exists(_ <= height)) BlockchainFeatureStatus.Approved
      else BlockchainFeatureStatus.Undefined

    def currentBlockVersion: Byte = blockVersionAt(blockchain.height)
    def nextBlockVersion: Byte    = blockVersionAt(blockchain.height + 1)

    def featureActivationHeight(feature: Short): Option[Int] = blockchain.activatedFeatures.get(feature)
    def featureApprovalHeight(feature: Short): Option[Int]   = blockchain.approvedFeatures.get(feature)

    def blockVersionAt(height: Int): Byte =
      if (isFeatureActivated(BlockchainFeatures.BlockV5, height)) ProtoBlockVersion
      else if (isFeatureActivated(BlockchainFeatures.BlockReward, height)) {
        if (blockchain.activatedFeatures(BlockchainFeatures.BlockReward.id) == height) NgBlockVersion else RewardBlockVersion
      }
      else if (blockchain.settings.functionalitySettings.blockVersion3AfterHeight + 1 < height) NgBlockVersion
      else if (height > 1) PlainBlockVersion
      else GenesisBlockVersion
  }
}
