package com.wavesplatform.events

import cats.Monoid
import cats.syntax.monoid._
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.StateUpdate.{BalanceUpdate, DataEntryUpdate, LeaseUpdate, LeasingBalanceUpdate}
import com.wavesplatform.protobuf._
import com.wavesplatform.state.DiffToStateApplier.PortfolioUpdates
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.state.{AccountDataInfo, AssetDescription, AssetScriptInfo, Blockchain, DataEntry, Diff, DiffToStateApplier, EmptyDataEntry, LeaseBalance}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.{Asset, GenesisTransaction, Transaction, TxAmount}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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
    balances: Seq[BalanceUpdate],
    leasingForAddress: Seq[LeasingBalanceUpdate],
    dataEntries: Seq[DataEntryUpdate],
    assets: Seq[AssetStateUpdate],
    leases: Seq[LeaseUpdate]
) {
  def isEmpty: Boolean = balances.isEmpty && leases.isEmpty && dataEntries.isEmpty && assets.isEmpty
}

object StateUpdate {
  case class BalanceUpdate(address: Address, asset: Asset, before: Long, after: Long)

  case class DataEntryUpdate(address: Address, before: DataEntry[_], after: DataEntry[_]) {
    require(before.key == after.key)

    def key: String = before.key
  }

  case class LeasingBalanceUpdate(address: Address, before: LeaseBalance, after: LeaseBalance)

  case class LeaseUpdate(
      leaseId: ByteStr,
      statusAfter: LeaseUpdate.LeaseStatus,
      amount: TxAmount,
      sender: PublicKey,
      recipient: Address,
      originTransactionId: ByteStr
  )

  object LeaseUpdate {
    sealed trait LeaseStatus
    object LeaseStatus {
      case object Active   extends LeaseStatus
      case object Inactive extends LeaseStatus
    }

    import com.wavesplatform.events.protobuf.StateUpdate.LeaseUpdate.{LeaseStatus => PBLeaseStatus}
    import com.wavesplatform.events.protobuf.StateUpdate.{LeaseUpdate => PBLeaseUpdate}

    def fromPB(v: PBLeaseUpdate): LeaseUpdate = {
      LeaseUpdate(
        v.leaseId.toByteStr,
        v.statusAfter match {
          case PBLeaseStatus.ACTIVE   => LeaseStatus.Active
          case PBLeaseStatus.INACTIVE => LeaseStatus.Inactive
          case _                      => ???
        },
        v.amount,
        v.sender.toPublicKey,
        v.recipient.toAddress,
        v.originTransactionId.toByteStr
      )
    }

    def toPB(v: LeaseUpdate): PBLeaseUpdate = {
      PBLeaseUpdate(
        v.leaseId.toByteString,
        v.statusAfter match {
          case LeaseStatus.Active   => PBLeaseStatus.ACTIVE
          case LeaseStatus.Inactive => PBLeaseStatus.INACTIVE
        },
        v.amount,
        v.sender.toByteString,
        v.recipient.toByteString,
        v.leaseId.toByteString
      )
    }
  }

  implicit val monoid: Monoid[StateUpdate] = new Monoid[StateUpdate] {
    override def empty: StateUpdate = StateUpdate(Seq.empty, Seq.empty, Seq.empty, Seq.empty, Seq.empty)

    override def combine(x: StateUpdate, y: StateUpdate): StateUpdate = {
      // merge balance updates, preserving order
      val balancesMap = mutable.LinkedHashMap.empty[(Address, Asset), BalanceUpdate]
      (x.balances ++ y.balances).foreach {
        case balance @ BalanceUpdate(addr, asset, _, _) =>
          balancesMap(addr -> asset) = balancesMap.get(addr -> asset) match {
            case Some(value) => balance.copy(before = value.before)
            case None        => balance
          }
      }
      val balances = balancesMap.values.toList

      // merge leases, preserving order
      val addrLeasesMap = mutable.LinkedHashMap.empty[Address, LeasingBalanceUpdate]
      (x.leasingForAddress ++ y.leasingForAddress).foreach {
        case balance @ LeasingBalanceUpdate(addr, _, _) =>
          addrLeasesMap(addr) = addrLeasesMap.get(addr) match {
            case Some(prevLease) =>
              balance.copy(before = prevLease.before)
            case None =>
              balance
          }
      }
      val addrLeases = addrLeasesMap.values.toList

      // merge data entries, preserving order
      val dataEntriesMap = mutable.LinkedHashMap.empty[(Address, String), DataEntryUpdate]
      (x.dataEntries ++ y.dataEntries).foreach {
        case entry @ DataEntryUpdate(addr, _, _) =>
          dataEntriesMap(addr -> entry.key) = dataEntriesMap.get(addr -> entry.key) match {
            case Some(value) => entry.copy(before = value.before)
            case None        => entry
          }
      }
      val dataEntries = dataEntriesMap.values.toList

      // merge asset state updates, preserving order
      val assetsMap = mutable.LinkedHashMap.empty[IssuedAsset, AssetStateUpdate]
      (x.assets ++ y.assets).foreach { a =>
        assetsMap(a.asset) = a
      }
      val assets = assetsMap.values.toList

      val leasesMap = mutable.LinkedHashMap.empty[ByteStr, LeaseUpdate]
      (x.leases ++ y.leases).foreach { lease =>
        leasesMap(lease.originTransactionId) = lease
      }
      val leases = leasesMap.values.toList

      StateUpdate(
        balances = balances,
        leasingForAddress = addrLeases,
        dataEntries = dataEntries,
        assets = assets,
        leases = leases
      )
    }
  }

  def atomic(blockchainBeforeWithMinerReward: Blockchain, diff: Diff, byTransaction: Option[Transaction]): StateUpdate = {
    val blockchain      = blockchainBeforeWithMinerReward
    val blockchainAfter = CompositeBlockchain(blockchain, Some(diff))

    val PortfolioUpdates(updatedBalances, updatedLeaseBalances) = DiffToStateApplier.portfolios(blockchain, diff)

    val balances = ArrayBuffer.empty[BalanceUpdate]
    for ((address, assetMap) <- updatedBalances; (asset, balance) <- assetMap; before = blockchain.balance(address, asset))
      balances += BalanceUpdate(address, asset, before, balance)

    val leaseBalanceUpdates = updatedLeaseBalances.map {
      case (address, leaseBalance) =>
        val before = blockchain.leaseBalance(address)
        LeasingBalanceUpdate(address, before, leaseBalance)
    }.toVector

    val dataEntries = diff.accountData.toSeq.flatMap {
      case (address, AccountDataInfo(data)) =>
        data.toSeq.map {
          case (_, entry) =>
            val prev = blockchain.accountData(address, entry.key).getOrElse(EmptyDataEntry(entry.key))
            DataEntryUpdate(address, prev, entry)
        }
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

    val updatedLeases = diff.leaseState.map {
      case (leaseId, newState) =>
        val originTransactionId = diff.transactions.values.map(_.transaction) collectFirst {
          case ltx: LeaseTransaction if ltx.id() == leaseId        => ltx.id()
          case lc: LeaseCancelTransaction if lc.leaseId == leaseId => lc.id()
          // TODO: case is: InvokeScriptTransaction if diff.scriptResults(is.id()).leases.contains(leaseId) => ???
        }

        val Some((_, tx: LeaseTransaction, _)) = blockchainAfter.transactionInfo(leaseId)
        LeaseUpdate(
          leaseId,
          if (newState) LeaseStatus.Active else LeaseStatus.Inactive,
          tx.amount,
          tx.sender,
          blockchainAfter.resolveAlias(tx.recipient).explicitGet(),
          originTransactionId.getOrElse(ByteStr.empty)
        )
    }.toVector

    StateUpdate(balances.toVector, leaseBalanceUpdates, dataEntries, assets, updatedLeases)
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
    val parentStateUpdateWithMinerReward = parentStateUpdate.balances.find(_.address == minerAddress) match {
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
      case b: BlockAppended                 => b.block.header.reference == other.toId
      case mb: MicroBlockAppended           => mb.microBlock.reference == other.toId
      case rb: RollbackCompleted            => rb.toHeight < other.toHeight
      case mrb: MicroBlockRollbackCompleted => mrb.toHeight == other.toHeight
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

final case class RollbackCompleted(toId: ByteStr, toHeight: Int) extends BlockchainUpdated

object RollbackCompleted {
  def from(toBlockId: ByteStr, toHeight: Int): RollbackCompleted = RollbackCompleted(toBlockId, toHeight)
}

final case class MicroBlockRollbackCompleted(toId: ByteStr, toHeight: Int) extends BlockchainUpdated

object MicroBlockRollbackCompleted {
  def from(toBlockId: ByteStr, height: Int): MicroBlockRollbackCompleted = MicroBlockRollbackCompleted(toBlockId, height)
}
