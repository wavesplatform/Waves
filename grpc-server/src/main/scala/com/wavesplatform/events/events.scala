package com.wavesplatform.events

import cats.Monoid
import cats.syntax.monoid._
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressOrAlias, PublicKey}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.StateUpdate.{AssetStateUpdate, BalanceUpdate, DataEntryUpdate, LeaseUpdate, LeasingBalanceUpdate}
import com.wavesplatform.events.protobuf.TransactionMetadata
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.protobuf._
import com.wavesplatform.protobuf.transaction.{PBAmounts, PBTransactions}
import com.wavesplatform.state.DiffToStateApplier.PortfolioUpdates
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.state.{
  AccountDataInfo,
  AssetDescription,
  AssetScriptInfo,
  Blockchain,
  DataEntry,
  Diff,
  DiffToStateApplier,
  EmptyDataEntry,
  Height,
  InvokeScriptResult,
  LeaseBalance
}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.transaction.{Asset, GenesisTransaction, TxAmount}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

final case class StateUpdate(
    balances: Seq[BalanceUpdate],
    leasingForAddress: Seq[LeasingBalanceUpdate],
    dataEntries: Seq[DataEntryUpdate],
    assets: Seq[AssetStateUpdate],
    leases: Seq[LeaseUpdate]
) {
  def isEmpty: Boolean = balances.isEmpty && leases.isEmpty && dataEntries.isEmpty && assets.isEmpty

  def reverse: StateUpdate = copy(
    balances.map(_.reverse).reverse,
    leasingForAddress.map(_.reverse).reverse,
    dataEntries.map(_.reverse).reverse,
    assets.map(_.reverse).reverse,
    leases.map(_.reverse).reverse
  )
}

object StateUpdate {
  case class BalanceUpdate(address: Address, asset: Asset, before: Long, after: Long) {
    def reverse: BalanceUpdate = copy(before = after, after = before)
  }

  object BalanceUpdate {
    import com.wavesplatform.events.protobuf.StateUpdate.{BalanceUpdate => PBBalanceUpdate}

    def fromPB(v: PBBalanceUpdate): BalanceUpdate = {
      val (asset, after) = PBAmounts.toAssetAndAmount(v.getAmountAfter)
      val before         = v.amountBefore
      BalanceUpdate(v.address.toAddress, asset, before, after)
    }

    def toPB(v: BalanceUpdate): PBBalanceUpdate = {
      val afterAmount = PBAmounts.fromAssetAndAmount(v.asset, v.after)
      PBBalanceUpdate(v.address.toByteString, Some(afterAmount), v.before)
    }
  }

  case class DataEntryUpdate(address: Address, before: DataEntry[_], after: DataEntry[_]) {
    require(before.key == after.key)

    def key: String              = before.key
    def reverse: DataEntryUpdate = copy(before = after, after = before)
  }

  object DataEntryUpdate {
    import com.wavesplatform.events.protobuf.StateUpdate.{DataEntryUpdate => PBDataEntryUpdate}

    def fromPB(v: PBDataEntryUpdate): DataEntryUpdate = {
      DataEntryUpdate(
        v.address.toAddress,
        PBTransactions.toVanillaDataEntry(v.getDataEntryBefore),
        PBTransactions.toVanillaDataEntry(v.getDataEntry)
      )
    }

    def toPB(v: DataEntryUpdate): PBDataEntryUpdate = {
      PBDataEntryUpdate(
        v.address.toByteString,
        Some(PBTransactions.toPBDataEntry(v.after)),
        Some(PBTransactions.toPBDataEntry(v.before))
      )
    }
  }

  case class LeasingBalanceUpdate(address: Address, before: LeaseBalance, after: LeaseBalance) {
    def reverse: LeasingBalanceUpdate = copy(before = after, after = before)
  }

  object LeasingBalanceUpdate {
    import com.wavesplatform.events.protobuf.StateUpdate.{LeasingUpdate => PBLeasingUpdate}

    def fromPB(v: PBLeasingUpdate): LeasingBalanceUpdate = {
      LeasingBalanceUpdate(
        v.address.toAddress,
        LeaseBalance(v.inBefore, v.outBefore),
        LeaseBalance(v.inAfter, v.outAfter)
      )
    }

    def toPB(v: LeasingBalanceUpdate): PBLeasingUpdate = {
      PBLeasingUpdate(
        v.address.toByteString,
        v.after.in,
        v.after.out,
        v.before.in,
        v.before.out
      )
    }
  }

  case class LeaseUpdate(
      leaseId: ByteStr,
      statusAfter: LeaseUpdate.LeaseStatus,
      amount: TxAmount,
      sender: PublicKey,
      recipient: Address,
      originTransactionId: ByteStr
  ) {
    def reverse: LeaseUpdate =
      copy(statusAfter = statusAfter match {
        case LeaseStatus.Active   => LeaseStatus.Inactive
        case LeaseStatus.Inactive => LeaseStatus.Active
      })
  }

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
        v.originTransactionId.toByteString
      )
    }
  }

  final case class AssetStateUpdate(
      assetId: ByteStr,
      before: Option[AssetDescription],
      after: Option[AssetDescription]
  ) {
    require(before.isDefined || after.isDefined)
    def reverse: AssetStateUpdate = copy(before = after, after = before)
  }

  object AssetStateUpdate {
    final case class AssetDetails(assetId: ByteStr, desc: AssetDescription)

    import com.wavesplatform.events.protobuf.StateUpdate.AssetDetails.{AssetScriptInfo => PBAssetScriptInfo}
    import com.wavesplatform.events.protobuf.StateUpdate.{AssetDetails => PBAssetDetails, AssetStateUpdate => PBAssetStateUpdate}

    def fromPB(self: PBAssetStateUpdate): AssetStateUpdate = {

      def detailsFromPB(v: PBAssetDetails): AssetDescription = {
        AssetDescription(
          v.assetId.toByteStr,
          v.issuer.toPublicKey,
          ByteString.copyFromUtf8(v.name),
          ByteString.copyFromUtf8(v.description),
          v.decimals,
          v.reissuable,
          BigInt(v.safeVolume.toByteArray),
          Height @@ v.lastUpdated,
          v.scriptInfo.map(fromPBScriptInfo),
          v.sponsorship,
          v.nft
        )
      }

      AssetStateUpdate(
        self.before.orElse(self.after).fold(ByteStr.empty)(_.assetId.toByteStr),
        self.before.map(detailsFromPB),
        self.after.map(detailsFromPB)
      )
    }

    def toPB(self: AssetStateUpdate): PBAssetStateUpdate = {
      def detailsToPB(v: AssetDescription): PBAssetDetails = {
        PBAssetDetails(
          assetId = self.assetId.toByteString,
          issuer = v.issuer.toByteString,
          decimals = v.decimals,
          name = v.name.toStringUtf8,
          description = v.description.toStringUtf8,
          reissuable = v.reissuable,
          volume = v.totalVolume.longValue,
          scriptInfo = v.script.map(toPBScriptInfo),
          sponsorship = v.sponsorship,
          nft = v.nft,
          safeVolume = ByteString.copyFrom(v.totalVolume.toByteArray),
          lastUpdated = v.lastUpdatedAt
        )
      }

      PBAssetStateUpdate(
        self.before.map(detailsToPB),
        self.after.map(detailsToPB)
      )
    }

    def fromPBScriptInfo(self: PBAssetScriptInfo): AssetScriptInfo = {
      AssetScriptInfo(
        script = PBTransactions.toVanillaScript(self.script).get,
        complexity = self.complexity
      )
    }

    def toPBScriptInfo(self: AssetScriptInfo): PBAssetScriptInfo = {
      PBAssetScriptInfo(
        script = PBTransactions.toPBScript(Some(self.script)),
        complexity = self.complexity
      )
    }
  }

  final case class AssetInfo(id: ByteStr, decimals: Int, name: String)
  object AssetInfo {
    import com.wavesplatform.events.protobuf.StateUpdate.{AssetInfo => PBAssetInfo}

    def toPB(ai: AssetInfo): PBAssetInfo = PBAssetInfo(
      ai.id.toByteString,
      ai.decimals,
      ai.name
    )

    def fromPB(ai: PBAssetInfo): AssetInfo =
      AssetInfo(
        ai.id.toByteStr,
        ai.decimals,
        ai.name
      )
  }

  import com.wavesplatform.events.protobuf.{StateUpdate => PBStateUpdate}

  def fromPB(v: PBStateUpdate): StateUpdate = {
    StateUpdate(
      v.balances.map(BalanceUpdate.fromPB),
      v.leasingForAddress.map(LeasingBalanceUpdate.fromPB),
      v.dataEntries.map(DataEntryUpdate.fromPB),
      v.assets.map(AssetStateUpdate.fromPB),
      v.individualLeases.map(LeaseUpdate.fromPB)
    )
  }

  def toPB(v: StateUpdate): PBStateUpdate = {
    PBStateUpdate(
      v.balances.map(BalanceUpdate.toPB),
      v.leasingForAddress.map(LeasingBalanceUpdate.toPB),
      v.dataEntries.map(DataEntryUpdate.toPB),
      v.assets.map(AssetStateUpdate.toPB),
      v.leases.map(LeaseUpdate.toPB)
    )
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
      // merge data entries, preserving order
      val dataEntriesMap = mutable.LinkedHashMap.empty[(Address, String), DataEntryUpdate]
      (x.dataEntries ++ y.dataEntries).foreach {
        case entry @ DataEntryUpdate(addr, _, _) =>
          dataEntriesMap(addr -> entry.key) = dataEntriesMap.get(addr -> entry.key) match {
            case Some(value) => entry.copy(before = value.before)
            case None        => entry
          }
      }
      // merge asset state updates, preserving order
      val assetsMap = mutable.LinkedHashMap.empty[ByteStr, AssetStateUpdate]
      (x.assets ++ y.assets).foreach { assetUpdate =>
        assetsMap(assetUpdate.assetId) = assetsMap.get(assetUpdate.assetId) match {
          case Some(prevUpdate) => assetUpdate.copy(before = prevUpdate.before)
          case None             => assetUpdate
        }
      }

      val leasesMap = mutable.LinkedHashMap.empty[ByteStr, LeaseUpdate]
      (x.leases ++ y.leases).foreach { lease =>
        leasesMap(lease.originTransactionId) = lease
      }

      StateUpdate(
        balances = balancesMap.values.toList,
        leasingForAddress = addrLeasesMap.values.toList,
        dataEntries = dataEntriesMap.values.toList,
        assets = assetsMap.values.toList,
        leases = leasesMap.values.toList
      )
    }
  }

  def atomic(blockchainBeforeWithMinerReward: Blockchain, diff: Diff): StateUpdate = {
    val blockchain      = blockchainBeforeWithMinerReward
    val blockchainAfter = CompositeBlockchain(blockchain, diff)

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
      asset <- (diff.issuedAssets.keySet ++ diff.updatedAssets.keySet ++ diff.assetScripts.keySet ++ diff.sponsorship.keySet).toSeq
      assetBefore = blockchainBeforeWithMinerReward.assetDescription(asset)
      assetAfter  = blockchainAfter.assetDescription(asset)
    } yield AssetStateUpdate(asset.id, assetBefore, assetAfter)

    val updatedLeases = diff.leaseState.map {
      case (leaseId, newState) =>
        LeaseUpdate(
          leaseId,
          if (newState.isActive) LeaseStatus.Active else LeaseStatus.Inactive,
          newState.amount,
          newState.sender,
          blockchainAfter.resolveAlias(newState.recipient).explicitGet(),
          newState.sourceId
        )
    }.toVector

    StateUpdate(balances.toVector, leaseBalanceUpdates, dataEntries, assets, updatedLeases)
  }

  private[this] def transactionsMetadata(blockchain: Blockchain, diff: Diff): Seq[TransactionMetadata] = {
    diff.transactions.values
      .map[TransactionMetadata.Metadata] { tx =>
        implicit class AddressResolver(addr: AddressOrAlias) {
          def resolve: Address = blockchain.resolveAlias(addr).explicitGet()
        }

        tx.transaction match {
          case tt: TransferTransaction =>
            TransactionMetadata.Metadata.Transfer(TransactionMetadata.TransferMetadata(tt.recipient.resolve.toByteString))

          case mtt: MassTransferTransaction =>
            TransactionMetadata.Metadata.MassTransfer(TransactionMetadata.MassTransferMetadata(mtt.transfers.map(_.address.resolve.toByteString)))

          case lt: LeaseTransaction =>
            TransactionMetadata.Metadata.LeaseMeta(TransactionMetadata.LeaseMetadata(lt.recipient.resolve.toByteString))

          case ist: InvokeScriptTransaction =>
            import TransactionMetadata.InvokeScriptMetadata.Argument
            import TransactionMetadata.InvokeScriptMetadata.Argument.Value

            def argumentToPB(arg: Terms.EXPR): Value = arg match {
              case Terms.CONST_LONG(t)     => Value.IntegerValue(t)
              case bs: Terms.CONST_BYTESTR => Value.BinaryValue(bs.bs.toByteString)
              case str: Terms.CONST_STRING => Value.StringValue(str.s)
              case Terms.CONST_BOOLEAN(b)  => Value.BooleanValue(b)
              case Terms.ARR(xs)           => Value.List(Argument.List(xs.map(x => Argument(argumentToPB(x)))))
              case _                       => Value.Empty
            }

            TransactionMetadata.Metadata.InvokeScript(
              TransactionMetadata.InvokeScriptMetadata(
                ist.dAppAddressOrAlias.resolve.toByteString,
                ist.funcCall.function.funcName,
                ist.funcCall.args.map(x => Argument(argumentToPB(x))),
                ist.payments.map(p => Amount(PBAmounts.toPBAssetId(p.assetId), p.amount)),
                diff.scriptResults.get(ist.id()).map(InvokeScriptResult.toPB)
              )
            )

          case _ =>
            TransactionMetadata.Metadata.Empty
        }
      }
      .map(TransactionMetadata(_))
      .toVector
  }

  def referencedAssets(blockchain: Blockchain, txsStateUpdates: Seq[StateUpdate]): Seq[AssetInfo] =
    txsStateUpdates
      .flatMap(st => st.assets.map(_.assetId) ++ st.balances.flatMap(_.asset.compatId))
      .distinct
      .flatMap(id => blockchain.assetDescription(IssuedAsset(id)).map(ad => AssetInfo(id, ad.decimals, ad.name.toStringUtf8)))

  def container(
      blockchainBeforeWithMinerReward: Blockchain,
      diff: DetailedDiff,
      minerAddress: Address
  ): (StateUpdate, Seq[StateUpdate], Seq[TransactionMetadata], Seq[AssetInfo]) = {
    val DetailedDiff(parentDiff, txsDiffs) = diff
    val parentStateUpdate                  = atomic(blockchainBeforeWithMinerReward, parentDiff)

    // miner reward is already in the blockchainBeforeWithMinerReward
    // if miner balance has been changed in parentDiff, it is already included in balance updates
    // if it has not, it needs to be manually requested from the blockchain and added to balance updates
    val parentStateUpdateWithMinerReward = parentStateUpdate.balances.find(_.address == minerAddress) match {
      case Some(_) => parentStateUpdate
      case None =>
        val minerBalance = blockchainBeforeWithMinerReward.balance(minerAddress, Waves)
        val reward       = blockchainBeforeWithMinerReward.blockReward(blockchainBeforeWithMinerReward.height).getOrElse(0L)
        parentStateUpdate.copy(balances = parentStateUpdate.balances :+ BalanceUpdate(minerAddress, Waves, minerBalance - reward, minerBalance))
    }

    val (txsStateUpdates, totalDiff) = txsDiffs.reverse
      .foldLeft((Seq.empty[StateUpdate], parentDiff)) {
        case ((updates, accDiff), txDiff) =>
          (
            updates :+ atomic(CompositeBlockchain(blockchainBeforeWithMinerReward, accDiff), txDiff),
            accDiff.combine(txDiff)
          )
      }
    val blockchainAfter = CompositeBlockchain(blockchainBeforeWithMinerReward, totalDiff)
    val metadata        = transactionsMetadata(blockchainAfter, totalDiff)
    val refAssets       = referencedAssets(blockchainAfter, txsStateUpdates)
    (parentStateUpdateWithMinerReward, txsStateUpdates, metadata, refAssets)
  }
}

sealed trait BlockchainUpdated {
  def id: ByteStr
  def height: Int
}

object BlockchainUpdated {
  implicit class BlockchainUpdatedExt(private val bu: BlockchainUpdated) extends AnyVal {
    def references(other: BlockchainUpdated): Boolean = bu match {
      case b: BlockAppended       => b.block.header.reference == other.id
      case mb: MicroBlockAppended => mb.microBlock.reference == other.id
      case _                      => false
    }
  }
}

final case class BlockAppended(
    id: ByteStr,
    height: Int,
    block: Block,
    updatedWavesAmount: Long,
    blockStateUpdate: StateUpdate,
    transactionStateUpdates: Seq[StateUpdate],
    transactionMetadata: Seq[TransactionMetadata],
    referencedAssets: Seq[StateUpdate.AssetInfo]
) extends BlockchainUpdated {
  def reverseStateUpdate: StateUpdate = Monoid.combineAll((blockStateUpdate +: transactionStateUpdates).map(_.reverse).reverse)
}

object BlockAppended {
  def from(block: Block, diff: DetailedDiff, blockchainBeforeWithMinerReward: Blockchain): BlockAppended = {
    val (blockStateUpdate, txsStateUpdates, txsMetadata, refAssets) =
      StateUpdate.container(blockchainBeforeWithMinerReward, diff, block.sender.toAddress)

    // updatedWavesAmount can change as a result of either genesis transactions or miner rewards
    val updatedWavesAmount = blockchainBeforeWithMinerReward.height match {
      // genesis case
      case 0 => block.transactionData.collect { case GenesisTransaction(_, amount, _, _, _) => amount }.sum
      // miner reward case
      case height => blockchainBeforeWithMinerReward.wavesAmount(height).toLong
    }

    BlockAppended(
      block.id(),
      blockchainBeforeWithMinerReward.height + 1,
      block,
      updatedWavesAmount,
      blockStateUpdate,
      txsStateUpdates,
      txsMetadata,
      refAssets
    )
  }

}

final case class MicroBlockAppended(
    id: ByteStr,
    height: Int,
    microBlock: MicroBlock,
    microBlockStateUpdate: StateUpdate,
    transactionStateUpdates: Seq[StateUpdate],
    transactionMetadata: Seq[TransactionMetadata],
    totalTransactionsRoot: ByteStr,
    referencedAssets: Seq[StateUpdate.AssetInfo]
) extends BlockchainUpdated {
  def reverseStateUpdate: StateUpdate = Monoid.combineAll((microBlockStateUpdate +: transactionStateUpdates).map(_.reverse).reverse)
}

object MicroBlockAppended {
  def revertMicroBlocks(mbs: Seq[MicroBlockAppended]): StateUpdate =
    Monoid.combineAll(mbs.reverse.map(_.reverseStateUpdate))

  def from(
      microBlock: MicroBlock,
      diff: DetailedDiff,
      blockchainBeforeWithMinerReward: Blockchain,
      totalBlockId: ByteStr,
      totalTransactionsRoot: ByteStr
  ): MicroBlockAppended = {
    val (microBlockStateUpdate, txsStateUpdates, txsMetadata, refAssets) =
      StateUpdate.container(blockchainBeforeWithMinerReward, diff, microBlock.sender.toAddress)

    MicroBlockAppended(
      totalBlockId,
      blockchainBeforeWithMinerReward.height,
      microBlock,
      microBlockStateUpdate,
      txsStateUpdates,
      txsMetadata,
      totalTransactionsRoot,
      refAssets
    )
  }
}

final case class RollbackResult(removedBlocks: Seq[Block], removedTransactionIds: Seq[ByteStr], stateUpdate: StateUpdate)

object RollbackResult {
  def micro(removedTransactionIds: Seq[ByteStr], stateUpdate: StateUpdate): RollbackResult =
    RollbackResult(Nil, removedTransactionIds, stateUpdate)

  implicit val monoid: Monoid[RollbackResult] = new Monoid[RollbackResult] {
    override def empty: RollbackResult = RollbackResult(Nil, Nil, Monoid.empty[StateUpdate])

    override def combine(x: RollbackResult, y: RollbackResult): RollbackResult = {
      RollbackResult(
        x.removedBlocks ++ y.removedBlocks,
        x.removedTransactionIds ++ y.removedTransactionIds,
        Monoid.combine(x.stateUpdate, y.stateUpdate)
      )
    }
  }
}
final case class RollbackCompleted(id: ByteStr, height: Int, rollbackResult: RollbackResult, referencedAssets: Seq[StateUpdate.AssetInfo])
    extends BlockchainUpdated
final case class MicroBlockRollbackCompleted(id: ByteStr, height: Int, rollbackResult: RollbackResult, referencedAssets: Seq[StateUpdate.AssetInfo])
    extends BlockchainUpdated
