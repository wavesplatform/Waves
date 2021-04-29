package com.wavesplatform.events

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import cats.Monoid
import cats.syntax.monoid._
import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf._
import com.wavesplatform.state.{AccountDataInfo, AssetDescription, AssetScriptInfo, Blockchain, DataEntry, Diff, DiffToStateApplier, LeaseBalance}
import com.wavesplatform.state.DiffToStateApplier.PortfolioUpdates
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.{Asset, GenesisTransaction, Transaction}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}

final case class AssetStateUpdate(
    asset: IssuedAsset,
    decimals: Int,
    name: ByteStr,
    description: ByteStr,
    reissuable: Boolean,
    volume: BigInt,
    scriptInfo: Option[AssetScriptInfo],
    sponsorship: Option[Long],
    nft: Boolean,
    assetExistedBefore: Boolean
)

final case class StateUpdate(
    balances: Seq[(Address, Asset, Long)],
    leases: Seq[(Address, LeaseBalance)],
    dataEntries: Seq[(Address, DataEntry[_])],
    assets: Seq[AssetStateUpdate]
) {
  def isEmpty: Boolean = balances.isEmpty && leases.isEmpty && dataEntries.isEmpty && assets.isEmpty
}

object StateUpdate {
  implicit val monoid: Monoid[StateUpdate] = new Monoid[StateUpdate] {
    override def empty: StateUpdate = StateUpdate(Seq.empty, Seq.empty, Seq.empty, Seq.empty)

    override def combine(x: StateUpdate, y: StateUpdate): StateUpdate = {
      // merge balance updates, preserving order
      val balancesMap = mutable.LinkedHashMap.empty[(Address, Asset), Long]
      (x.balances ++ y.balances).foreach {
        case (addr, asset, balance) =>
          balancesMap.remove((addr, asset))
          balancesMap.addOne(((addr, asset), balance))
      }
      val balances = balancesMap.toList.map { case ((addr, asset), balance) => (addr, asset, balance) }

      // merge leases, preserving order
      val leasesMap = mutable.LinkedHashMap.empty[Address, LeaseBalance]
      (x.leases ++ y.leases).foreach {
        case (addr, balance) =>
          leasesMap.remove(addr)
          leasesMap.addOne((addr, balance))
      }
      val leases = leasesMap.toList

      // merge data entries, preserving order
      val dataEntriesMap = mutable.LinkedHashMap.empty[(Address, String), DataEntry[_]]
      (x.dataEntries ++ y.dataEntries).foreach {
        case (addr, entry) =>
          dataEntriesMap.remove((addr, entry.key))
          dataEntriesMap.addOne(((addr, entry.key), entry))
      }
      val dataEntries = dataEntriesMap.toList.map { case ((addr, _), entry) => (addr, entry) }

      // merge asset state updates, preserving order
      val assetsMap = mutable.LinkedHashMap.empty[IssuedAsset, AssetStateUpdate]
      (x.assets ++ y.assets).foreach(a => {
        assetsMap.remove(a.asset)
        assetsMap.addOne((a.asset, a))
      })
      val assets = assetsMap.toList.map { case (_, upd) => upd }

      StateUpdate(
        balances = balances,
        leases = leases,
        dataEntries = dataEntries,
        assets = assets
      )
    }
  }

  def atomic(blockchainBeforeWithMinerReward: Blockchain, diff: Diff, byTransaction: Option[Transaction]): StateUpdate = {
    val blockchainAfter = CompositeBlockchain(blockchainBeforeWithMinerReward, Some(diff))

    val PortfolioUpdates(updatedBalances, updatedLeases) = DiffToStateApplier.portfolios(blockchainBeforeWithMinerReward, diff)

    val balances = ArrayBuffer.empty[(Address, Asset, Long)]
    for ((address, assetMap) <- updatedBalances; (asset, balance) <- assetMap) balances += ((address, asset, balance))

    val dataEntries = diff.accountData.toSeq.flatMap {
      case (address, AccountDataInfo(data)) =>
        data.toSeq.map { case (_, entry) => (address, entry) }
    }

    val assets: Seq[AssetStateUpdate] = for {
      a <- (diff.issuedAssets.keySet ++ diff.updatedAssets.keySet ++ diff.assetScripts.keySet ++ diff.sponsorship.keySet).toSeq
      AssetDescription(
        _,
        _,
        name,
        description,
        decimals,
        reissuable,
        totalVolume,
        _,
        script,
        sponsorship,
        nft
      ) <- blockchainAfter.assetDescription(a).toSeq
      existedBefore = !diff.issuedAssets.isDefinedAt(a)
    } yield AssetStateUpdate(
      a,
      decimals,
      name.toByteStr,
      description.toByteStr,
      reissuable,
      totalVolume,
      script,
      if (sponsorship == 0) None else Some(sponsorship),
      nft,
      existedBefore
    )

    StateUpdate(balances.toSeq, updatedLeases.toSeq, dataEntries, assets)
  }

  def container(
      blockchainBeforeWithMinerReward: Blockchain,
      diff: DetailedDiff,
      transactions: Seq[Transaction],
      minerAddress: Address
  ): (StateUpdate, Seq[StateUpdate]) = {
    val DetailedDiff(parentDiff, txsDiffs) = diff
    val parentStateUpdate                  = atomic(blockchainBeforeWithMinerReward, parentDiff, None)

    // miner reward is already in the blockchainBeforeWithMinerReward
    // if miner balance has been changed in parentDiff, it is already included in balance updates
    // if it has not, it needs to be manually requested from the blockchain and added to balance updates
    val parentStateUpdateWithMinerReward = parentStateUpdate.balances.find(_._1 == minerAddress) match {
      case Some(_) => parentStateUpdate
      case None =>
        val minerBalance = blockchainBeforeWithMinerReward.balance(minerAddress, Waves)
        parentStateUpdate.copy(balances = parentStateUpdate.balances :+ ((minerAddress, Waves, minerBalance)))
    }

    val (txsStateUpdates, _) = txsDiffs.reverse
      .zip(transactions)
      .foldLeft((ArrayBuffer.empty[StateUpdate], parentDiff)) {
        case ((updates, accDiff), (txDiff, tx)) =>
          (
            updates += atomic(CompositeBlockchain(blockchainBeforeWithMinerReward, Some(accDiff)), txDiff, Some(tx)),
            accDiff.combine(txDiff)
          )
      }

    (parentStateUpdateWithMinerReward, txsStateUpdates.toSeq)
  }
}

sealed trait BlockchainUpdated extends Product with Serializable {
  def toId: ByteStr
  def toHeight: Int
}

object BlockchainUpdated {
  implicit class BlockchainUpdatedExt(private val bu: BlockchainUpdated) extends AnyVal {
    def references(other: BlockchainUpdated): Boolean = bu match {
      case b: BlockAppended       => b.block.header.reference == other.toId
      case mb: MicroBlockAppended => mb.microBlock.reference == other.toId
      case _                      => false
    }
  }
}

final case class BlockAppended(
    toId: ByteStr,
    toHeight: Int,
    block: Block,
    updatedWavesAmount: Long,
    blockStateUpdate: StateUpdate,
    transactionStateUpdates: Seq[StateUpdate]
) extends BlockchainUpdated

object BlockAppended {
  def from(block: Block, diff: DetailedDiff, minerReward: Option[Long], blockchainBeforeWithMinerReward: Blockchain): BlockAppended = {
    val (blockStateUpdate, txsStateUpdates) =
      StateUpdate.container(blockchainBeforeWithMinerReward, diff, block.transactionData, block.sender.toAddress)

    // updatedWavesAmount can change as a result of either genesis transactions or miner rewards
    val updatedWavesAmount = blockchainBeforeWithMinerReward.height match {
      // genesis case
      case 0 => block.transactionData.collect { case GenesisTransaction(_, amount, _, _, _) => amount }.sum
      // miner reward case
      case height => blockchainBeforeWithMinerReward.wavesAmount(height).toLong
    }

    BlockAppended(block.id.value(), blockchainBeforeWithMinerReward.height + 1, block, updatedWavesAmount, blockStateUpdate, txsStateUpdates)
  }
}

final case class MicroBlockAppended(
    toId: ByteStr,
    toHeight: Int,
    microBlock: MicroBlock,
    microBlockStateUpdate: StateUpdate,
    transactionStateUpdates: Seq[StateUpdate],
    totalTransactionsRoot: ByteStr
) extends BlockchainUpdated

object MicroBlockAppended {
  def from(
      microBlock: MicroBlock,
      diff: DetailedDiff,
      blockchainBeforeWithMinerReward: Blockchain,
      totalBlockId: ByteStr,
      totalTransactionsRoot: ByteStr
  ): MicroBlockAppended = {
    val (microBlockStateUpdate, txsStateUpdates) =
      StateUpdate.container(blockchainBeforeWithMinerReward, diff, microBlock.transactionData, microBlock.sender.toAddress)

    MicroBlockAppended(
      totalBlockId,
      blockchainBeforeWithMinerReward.height,
      microBlock,
      microBlockStateUpdate,
      txsStateUpdates,
      totalTransactionsRoot
    )
  }
}

final case class RollbackResult(removedBlocks: Seq[Block], removedTransactionIds: Seq[ByteStr])

object RollbackResult {
  def micro(removedTransactionIds: Seq[ByteStr]): RollbackResult =
    RollbackResult(Nil, removedTransactionIds)

  implicit val monoid: Monoid[RollbackResult] = new Monoid[RollbackResult] {
    override def empty: RollbackResult = RollbackResult(Nil, Nil)

    override def combine(x: RollbackResult, y: RollbackResult): RollbackResult = {
      RollbackResult(
        x.removedBlocks ++ y.removedBlocks,
        x.removedTransactionIds ++ y.removedTransactionIds
      )
    }
  }
}

final case class RollbackCompleted(toId: ByteStr, toHeight: Int) extends BlockchainUpdated

object RollbackCompleted {
  def from(toBlockId: ByteStr, toHeight: Int): RollbackCompleted = RollbackCompleted(toBlockId, toHeight)
}

final case class MicroBlockRollbackCompleted(toId: ByteStr, toHeight: Int) extends BlockchainUpdated

object MicroBlockRollbackCompleted {
  def from(toBlockId: ByteStr, height: Int): MicroBlockRollbackCompleted = MicroBlockRollbackCompleted(toBlockId, height)
}
