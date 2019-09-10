package com.wavesplatform.state.reader

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.{AliasDoesNotExist, AliasIsDisabled, GenericError}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, Transaction}

trait CompositeBlockchain extends Blockchain {
  def stableBlockchain: Blockchain
  def newBlock: Option[Block]
  def maybeDiff: Option[Diff]
  def maybeCarryFee: Option[Long]
  def reward: Option[Long]

  final def diff: Diff = maybeDiff.getOrElse(Diff.empty)

  override val settings: BlockchainSettings = stableBlockchain.settings

  override def carryFee: Long = maybeCarryFee.getOrElse(stableBlockchain.carryFee)

  override def balance(address: Address, assetId: Asset): Long =
    stableBlockchain.balance(address, assetId) + diff.portfolios.getOrElse(address, Portfolio.empty).balanceOf(assetId)

  override def leaseBalance(address: Address): LeaseBalance = {
    cats.Monoid.combine(stableBlockchain.leaseBalance(address), diff.portfolios.getOrElse(address, Portfolio.empty).lease)
  }

  override def assetScriptWithComplexity(asset: IssuedAsset): Option[(Script, Long)] =
    maybeDiff.flatMap(_.assetScripts.get(asset)).getOrElse(stableBlockchain.assetScriptWithComplexity(asset))

  override def hasAssetScript(asset: IssuedAsset): Boolean = maybeDiff.flatMap(_.assetScripts.get(asset)) match {
    case Some(s) => s.nonEmpty
    case None    => stableBlockchain.hasAssetScript(asset)
  }

  override def assetDescription(asset: IssuedAsset): Option[AssetDescription] = {
    val script: Option[Script] = assetScript(asset)
    stableBlockchain.assetDescription(asset) match {
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
    stableBlockchain.leaseDetails(leaseId).map(ld => ld.copy(isActive = diff.leaseState.getOrElse(leaseId, ld.isActive))) orElse
      diff.transactions.get(leaseId).collect {
        case (txHeight, lt: LeaseTransaction, _) =>
          LeaseDetails(lt.sender, lt.recipient, txHeight, lt.amount, diff.leaseState(lt.id()))
      }
  }

  override def transferById(id: BlockId): Option[(Int, TransferTransaction)] = {
    diff.transactions
      .get(id)
      .collect {
        case (h, tx: TransferTransaction, _) => (h, tx)
      }
      .orElse(stableBlockchain.transferById(id))
  }

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] =
    diff.transactions
      .get(id)
      .map(t => (this.height, t._2))
      .orElse(stableBlockchain.transactionInfo(id))

  override def transactionHeight(id: ByteStr): Option[Int] =
    diff.transactions
      .get(id)
      .map(_ => this.height)
      .orElse(stableBlockchain.transactionHeight(id))

  def heightOffset: Int =
    (newBlock orElse maybeDiff).fold(0)(_ => 1)

  override def height: Int =
    stableBlockchain.height + heightOffset

  override def resolveAlias(alias: Alias): Either[ValidationError, Address] = stableBlockchain.resolveAlias(alias) match {
    case l @ Left(AliasIsDisabled(_)) => l
    case Right(addr)                  => Right(diff.aliases.getOrElse(alias, addr))
    case Left(_)                      => diff.aliases.get(alias).toRight(AliasDoesNotExist(alias))
  }

  override def collectActiveLeases(from: Int, to: Int)(filter: LeaseTransaction => Boolean): Seq[LeaseTransaction] = {
    val innerActiveLeases = stableBlockchain.collectActiveLeases(from, to)(filter)
    maybeDiff match {
      case Some(ng) if to == height =>
        val cancelledInLiquidBlock = ng.leaseState.collect {
          case (id, false) => id
        }.toSet
        val addedInLiquidBlock = ng.transactions.collect {
          case (id, (_, lt: LeaseTransaction, _)) if !cancelledInLiquidBlock(id) => lt
        }
        innerActiveLeases.filterNot(lt => cancelledInLiquidBlock(lt.id())) ++ addedInLiquidBlock
      case _ => innerActiveLeases
    }
  }

  override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] = {
    val b = Map.newBuilder[Address, A]
    for ((a, p) <- diff.portfolios if p.lease != LeaseBalance.empty || p.balance != 0) {
      pf.runWith(b += a -> _)(a -> this.wavesPortfolio(a))
    }

    stableBlockchain.collectLposPortfolios(pf) ++ b.result()
  }

  override def invokeScriptResult(txId: TransactionId): Either[ValidationError, InvokeScriptResult] = {
    diff.scriptResults
      .get(txId)
      .toRight(GenericError("InvokeScript result not found"))
      .orElse(stableBlockchain.invokeScriptResult(txId))
  }

  override def containsTransaction(tx: Transaction): Boolean = diff.transactions.contains(tx.id()) || stableBlockchain.containsTransaction(tx)

  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee =
    diff.orderFills.get(orderId).orEmpty.combine(stableBlockchain.filledVolumeAndFee(orderId))

  override def balanceSnapshots(address: Address, from: Int, to: BlockId): Seq[BalanceSnapshot] = {
    if (stableBlockchain.heightOf(to).isDefined || maybeDiff.isEmpty) {
      stableBlockchain.balanceSnapshots(address, from, to)
    } else {
      val balance = this.balance(address)
      val lease   = this.leaseBalance(address)
      val bs      = BalanceSnapshot(height, Portfolio(balance, lease, Map.empty))
      if (stableBlockchain.height > 0 && from < this.height) bs +: stableBlockchain.balanceSnapshots(address, from, to) else Seq(bs)
    }
  }

  override def accountScriptWithComplexity(address: Address): Option[(Script, Long)] = {
    diff.scripts.get(address) match {
      case None            => stableBlockchain.accountScriptWithComplexity(address)
      case Some(None)      => None
      case Some(Some(scr)) => Some(scr)
    }
  }

  override def hasScript(address: Address): Boolean = {
    diff.scripts.get(address) match {
      case None          => stableBlockchain.hasScript(address)
      case Some(None)    => false
      case Some(Some(_)) => true
    }
  }

  override def accountData(acc: Address, key: String): Option[DataEntry[_]] = {
    val diffData = diff.accountData.get(acc).orEmpty
    diffData.data.get(key).orElse(stableBlockchain.accountData(acc, key))
  }

  override def lastBlock: Option[Block] = newBlock.orElse(stableBlockchain.lastBlock)

  override def score: BigInt = newBlock.fold(BigInt(0))(_.blockScore()) + stableBlockchain.score

  private def filterById(blockId: BlockId): Option[Block] = newBlock.filter(_.uniqueId == blockId)

  private def filterByHeight(height: Int): Option[Block] = newBlock.filter(_ => this.height == height)

  private def headerAndSize(block: Block): (BlockHeader, Int) = block -> block.bytes().length

  override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)] =
    filterByHeight(height).map(headerAndSize) orElse stableBlockchain.blockHeaderAndSize(height)

  override def blockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)] =
    filterById(blockId).map(headerAndSize) orElse stableBlockchain.blockHeaderAndSize(blockId)

  override def blockBytes(height: Int): Option[Array[Byte]] = filterByHeight(height).map(_.bytes()) orElse stableBlockchain.blockBytes(height)

  override def blockBytes(blockId: ByteStr): Option[Array[Byte]] = filterById(blockId).map(_.bytes()) orElse stableBlockchain.blockBytes(blockId)

  override def heightOf(blockId: ByteStr): Option[Int] = filterById(blockId).map(_ => height) orElse stableBlockchain.heightOf(blockId)

  /** Returns the most recent block IDs, starting from the most recent  one */
  override def lastBlockIds(howMany: Int): Seq[ByteStr] =
    if (howMany <= 0) Seq.empty else newBlock.map(_.uniqueId).toSeq ++ stableBlockchain.lastBlockIds(howMany - 1)

  /** Returns a chain of blocks starting with the block with the given ID (from oldest to newest) */
  override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] =
    for {
      ids <- stableBlockchain.blockIdsAfter(parentSignature, howMany)
      newId = newBlock.filter(_.reference == parentSignature).map(_.uniqueId).fold(Seq.empty[ByteStr])(Seq(_))
    } yield newId ++ ids

  override def parentHeader(block: BlockHeader, back: Int): Option[BlockHeader] = stableBlockchain.parentHeader(block, back)

  override def totalFee(height: Int): Option[Long] = stableBlockchain.totalFee(height)

  /** Features related */
  override def approvedFeatures: Map[Short, Int] = stableBlockchain.approvedFeatures

  override def activatedFeatures: Map[Short, Int] = stableBlockchain.activatedFeatures

  override def featureVotes(height: Int): Map[Short, Int] = stableBlockchain.featureVotes(height)

  /** Block reward related */
  override def blockReward(height: Int): Option[Long] = reward.filter(_ => this.height == height) orElse stableBlockchain.blockReward(height)

  override def lastBlockReward: Option[Long] = reward.orElse(stableBlockchain.lastBlockReward)

  override def blockRewardVotes(height: Int): Seq[Long] = stableBlockchain.blockRewardVotes(height)

  override def wavesAmount(height: Int): BigInt = stableBlockchain.wavesAmount(height)
}

object CompositeBlockchain {
  private[this] class CompositeBlockchainImpl(
      val stableBlockchain: Blockchain,
      val maybeDiff: Option[Diff] = None,
      val newBlock: Option[Block] = None,
      val maybeCarryFee: Option[Long] = None,
      val reward: Option[Long] = None
  ) extends CompositeBlockchain

  def apply(
      stableBlockchain: Blockchain,
      maybeDiff: Option[Diff] = None,
      newBlock: Option[Block] = None,
      carryFee: Option[Long] = None,
      reward: Option[Long] = None
  ): CompositeBlockchain = new CompositeBlockchainImpl(stableBlockchain, maybeDiff, newBlock, carryFee, reward)
}
