package com.wavesplatform.events

import cats.Monoid
import cats.implicits.catsSyntaxSemigroup
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressOrAlias, PublicKey}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.StateUpdate.{AssetStateUpdate, BalanceUpdate, DataEntryUpdate, LeaseUpdate, LeasingBalanceUpdate, ScriptUpdate}
import com.wavesplatform.events.protobuf.TransactionMetadata
import com.wavesplatform.events.protobuf.TransactionMetadata.EthereumMetadata
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.protobuf.*
import com.wavesplatform.protobuf.transaction.InvokeScriptResult.Call.Argument
import com.wavesplatform.protobuf.transaction.{PBAmounts, PBTransactions, InvokeScriptResult as PBInvokeScriptResult}
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionLike
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.transaction.{Asset, Authorized, CreateAliasTransaction, EthereumTransaction}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

final case class StateUpdate(
    balances: Seq[BalanceUpdate],
    leasingForAddress: Seq[LeasingBalanceUpdate],
    dataEntries: Seq[DataEntryUpdate],
    assets: Seq[AssetStateUpdate],
    leases: Seq[LeaseUpdate],
    scripts: Seq[ScriptUpdate],
    deletedAliases: Seq[String]
) {
  def isEmpty: Boolean = balances.isEmpty && leases.isEmpty && dataEntries.isEmpty && assets.isEmpty && scripts.isEmpty

  def reverse: StateUpdate = copy(
    balances.map(_.reverse).reverse,
    leasingForAddress.map(_.reverse).reverse,
    dataEntries.map(_.reverse).reverse,
    assets.map(_.reverse).reverse,
    leases.map(_.reverse).reverse,
    scripts.map(_.reverse).reverse
  )
}

object StateUpdate {
  case class BalanceUpdate(address: Address, asset: Asset, before: Long, after: Long) {
    def reverse: BalanceUpdate = copy(before = after, after = before)
  }

  object BalanceUpdate {
    import com.wavesplatform.events.protobuf.StateUpdate.BalanceUpdate as PBBalanceUpdate

    def fromPB(v: PBBalanceUpdate): BalanceUpdate = {
      val (asset, after) = PBAmounts.toAssetAndAmount(v.getAmountAfter)
      val before         = v.amountBefore
      BalanceUpdate(v.address.toAddress(), asset, before, after)
    }

    def toPB(v: BalanceUpdate): PBBalanceUpdate = {
      val afterAmount = PBAmounts.fromAssetAndAmount(v.asset, v.after)
      PBBalanceUpdate(v.address.toByteString, Some(afterAmount), v.before)
    }
  }

  case class DataEntryUpdate(address: Address, before: DataEntry[?], after: DataEntry[?]) {
    require(before.key == after.key)

    def key: String              = before.key
    def reverse: DataEntryUpdate = copy(before = after, after = before)
  }

  object DataEntryUpdate {
    import com.wavesplatform.events.protobuf.StateUpdate.DataEntryUpdate as PBDataEntryUpdate

    def fromPB(v: PBDataEntryUpdate): DataEntryUpdate = {
      DataEntryUpdate(
        v.address.toAddress(),
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
    import com.wavesplatform.events.protobuf.StateUpdate.LeasingUpdate as PBLeasingUpdate

    def fromPB(v: PBLeasingUpdate): LeasingBalanceUpdate = {
      LeasingBalanceUpdate(
        v.address.toAddress(),
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
      amount: Long,
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

    import com.wavesplatform.events.protobuf.StateUpdate.LeaseUpdate as PBLeaseUpdate
    import com.wavesplatform.events.protobuf.StateUpdate.LeaseUpdate.LeaseStatus as PBLeaseStatus

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
        v.recipient.toAddress(),
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

    import com.wavesplatform.events.protobuf.StateUpdate.AssetDetails.AssetScriptInfo as PBAssetScriptInfo
    import com.wavesplatform.events.protobuf.StateUpdate.{AssetDetails as PBAssetDetails, AssetStateUpdate as PBAssetStateUpdate}

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
          v.nft,
          v.sequenceInBlock,
          Height @@ v.issueHeight
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
          lastUpdated = v.lastUpdatedAt,
          sequenceInBlock = v.sequenceInBlock,
          issueHeight = v.issueHeight
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
    import com.wavesplatform.events.protobuf.StateUpdate.AssetInfo as PBAssetInfo

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

  final case class ScriptUpdate(dApp: ByteStr, before: Option[ByteStr], after: Option[ByteStr]) {
    def reverse: ScriptUpdate = copy(before = after, after = before)
  }

  object ScriptUpdate {
    import com.wavesplatform.events.protobuf.StateUpdate.ScriptUpdate as PBScriptUpdate

    def toPB(su: ScriptUpdate): PBScriptUpdate =
      PBScriptUpdate(su.dApp.toByteString, su.before.fold(ByteString.EMPTY)(_.toByteString), su.after.fold(ByteString.EMPTY)(_.toByteString))

    def fromPB(su: PBScriptUpdate): ScriptUpdate =
      ScriptUpdate(
        su.address.toByteStr,
        Option.unless(su.before.isEmpty)(su.before.toByteStr),
        Option.unless(su.after.isEmpty)(su.after.toByteStr)
      )
  }

  import com.wavesplatform.events.protobuf.StateUpdate as PBStateUpdate

  def fromPB(v: PBStateUpdate): StateUpdate = {
    StateUpdate(
      v.balances.map(BalanceUpdate.fromPB),
      v.leasingForAddress.map(LeasingBalanceUpdate.fromPB),
      v.dataEntries.map(DataEntryUpdate.fromPB),
      v.assets.map(AssetStateUpdate.fromPB),
      v.individualLeases.map(LeaseUpdate.fromPB),
      v.scripts.map(ScriptUpdate.fromPB),
      v.deletedAliases
    )
  }

  def toPB(v: StateUpdate): PBStateUpdate = {
    PBStateUpdate(
      v.balances.map(BalanceUpdate.toPB),
      v.leasingForAddress.map(LeasingBalanceUpdate.toPB),
      v.dataEntries.map(DataEntryUpdate.toPB),
      v.assets.map(AssetStateUpdate.toPB),
      v.leases.map(LeaseUpdate.toPB),
      v.scripts.map(ScriptUpdate.toPB),
      v.deletedAliases
    )
  }

  implicit val monoid: Monoid[StateUpdate] = new Monoid[StateUpdate] {
    override def empty: StateUpdate = StateUpdate(Seq.empty, Seq.empty, Seq.empty, Seq.empty, Seq.empty, Seq.empty, Seq.empty)

    override def combine(x: StateUpdate, y: StateUpdate): StateUpdate = {
      // merge balance updates, preserving order
      val balancesMap = mutable.LinkedHashMap.empty[(Address, Asset), BalanceUpdate]
      (x.balances ++ y.balances).foreach { case balance @ BalanceUpdate(addr, asset, _, _) =>
        balancesMap(addr -> asset) = balancesMap.get(addr -> asset) match {
          case Some(value) => balance.copy(before = value.before)
          case None        => balance
        }
      }
      // merge leases, preserving order
      val addrLeasesMap = mutable.LinkedHashMap.empty[Address, LeasingBalanceUpdate]
      (x.leasingForAddress ++ y.leasingForAddress).foreach { case balance @ LeasingBalanceUpdate(addr, _, _) =>
        addrLeasesMap(addr) = addrLeasesMap.get(addr) match {
          case Some(prevLease) =>
            balance.copy(before = prevLease.before)
          case None =>
            balance
        }
      }
      // merge data entries, preserving order
      val dataEntriesMap = mutable.LinkedHashMap.empty[(Address, String), DataEntryUpdate]
      (x.dataEntries ++ y.dataEntries).foreach { case entry @ DataEntryUpdate(addr, _, _) =>
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

      val scriptsMap = mutable.LinkedHashMap.empty[ByteStr, ScriptUpdate]
      (x.scripts ++ y.scripts).foreach { scriptUpdate =>
        scriptsMap(scriptUpdate.dApp) = scriptsMap.get(scriptUpdate.dApp) match {
          case Some(prevUpdate) => scriptUpdate.copy(before = prevUpdate.before)
          case None             => scriptUpdate
        }
      }

      StateUpdate(
        balances = balancesMap.values.toList,
        leasingForAddress = addrLeasesMap.values.toList,
        dataEntries = dataEntriesMap.values.toList,
        assets = assetsMap.values.toList,
        leases = leasesMap.values.toList,
        scripts = scriptsMap.values.toList,
        deletedAliases = x.deletedAliases ++ y.deletedAliases
      )
    }
  }

  def atomic(blockchainBeforeWithMinerReward: Blockchain, snapshot: StateSnapshot): StateUpdate = {
    val blockchain      = blockchainBeforeWithMinerReward
    val blockchainAfter = SnapshotBlockchain(blockchain, snapshot)

    val balances = ArrayBuffer.empty[BalanceUpdate]
    for {
      ((address, asset), after) <- snapshot.balances
      before = blockchain.balance(address, asset) if before != after
    } balances += BalanceUpdate(address, asset, before, after)

    val leaseBalanceUpdates = snapshot.leaseBalances
      .map { case (address, after) =>
        val before = blockchain.leaseBalance(address)
        LeasingBalanceUpdate(address, before, after)
      }
      .filterNot(b => b.before == b.after)
      .toVector

    val dataEntries = snapshot.accountData.toSeq.flatMap { case (address, data) =>
      data.toSeq.map { case (_, entry) =>
        val prev = blockchain.accountData(address, entry.key).getOrElse(EmptyDataEntry(entry.key))
        DataEntryUpdate(address, prev, entry)
      }
    }

    val assets: Seq[AssetStateUpdate] = for {
      asset <- (
        snapshot.assetStatics.keySet ++
          snapshot.assetVolumes.keySet ++
          snapshot.assetNamesAndDescriptions.keySet ++
          snapshot.assetScripts.keySet ++
          snapshot.sponsorships.keySet
      ).toSeq
      assetBefore = blockchainBeforeWithMinerReward.assetDescription(asset)
      assetAfter  = blockchainAfter.assetDescription(asset)
    } yield AssetStateUpdate(asset.id, assetBefore, assetAfter)

    val newLeaseUpdates = snapshot.newLeases.collect {
      case (newId, staticInfo) if !snapshot.cancelledLeases.contains(newId) =>
        LeaseUpdate(
          newId,
          LeaseStatus.Active,
          staticInfo.amount.value,
          staticInfo.sender,
          staticInfo.recipientAddress,
          staticInfo.sourceId
        )
    }

    val cancelledLeaseUpdates = snapshot.cancelledLeases.map { case (id, _) =>
      val si = snapshot.newLeases.get(id).orElse(blockchain.leaseDetails(id).map(_.static))
      LeaseUpdate(
        id,
        LeaseStatus.Inactive,
        si.fold(0L)(_.amount.value),
        si.fold(PublicKey(new Array[Byte](32)))(_.sender),
        si.fold(PublicKey(new Array[Byte](32)).toAddress)(_.recipientAddress),
        si.fold(ByteStr.empty)(_.sourceId)
      )
    }

    val updatedLeases = newLeaseUpdates ++ cancelledLeaseUpdates

    val updatedScripts = snapshot.accountScriptsByAddress.map { case (address, newScript) =>
      ScriptUpdate(ByteStr(address.bytes), blockchain.accountScript(address).map(_.script.bytes()), newScript.map(_.script.bytes()))
    }.toVector

    StateUpdate(balances.toVector, leaseBalanceUpdates, dataEntries, assets, updatedLeases.toSeq, updatedScripts, Seq.empty)
  }

  private[this] def transactionsMetadata(blockchain: Blockchain, snapshot: StateSnapshot): Seq[TransactionMetadata] = {
    implicit class AddressResolver(addr: AddressOrAlias) {
      def resolve: Address = blockchain.resolveAlias(addr).explicitGet()
    }

    def invokeScriptLikeToMetadata(ist: InvokeScriptTransactionLike) = {

      def argumentToPB(arg: Terms.EXPR): Argument.Value = arg match {
        case Terms.CONST_LONG(t)     => Argument.Value.IntegerValue(t)
        case bs: Terms.CONST_BYTESTR => Argument.Value.BinaryValue(bs.bs.toByteString)
        case str: Terms.CONST_STRING => Argument.Value.StringValue(str.s)
        case Terms.CONST_BOOLEAN(b)  => Argument.Value.BooleanValue(b)
        case Terms.ARR(xs)           => Argument.Value.List(Argument.List(xs.map(x => Argument(argumentToPB(x)))))
        case _                       => Argument.Value.Empty
      }

      TransactionMetadata.InvokeScriptMetadata(
        ist.dApp.resolve.toByteString,
        ist.funcCall.function.funcName,
        ist.funcCall.args.map(x => PBInvokeScriptResult.Call.Argument(argumentToPB(x))),
        ist.payments.map(p => Amount(PBAmounts.toPBAssetId(p.assetId), p.amount)),
        snapshot.scriptResults.get(ist.id()).map(InvokeScriptResult.toPB(_, addressForTransfer = true))
      )
    }

    snapshot.transactions.map { case (_, tx) =>
      TransactionMetadata(
        tx.transaction match {
          case a: Authorized => a.sender.toAddress.toByteString
          case _             => ByteString.EMPTY
        },
        tx.transaction match {
          case tt: TransferTransaction =>
            TransactionMetadata.Metadata.Transfer(TransactionMetadata.TransferMetadata(tt.recipient.resolve.toByteString))

          case mtt: MassTransferTransaction =>
            TransactionMetadata.Metadata.MassTransfer(TransactionMetadata.MassTransferMetadata(mtt.transfers.map(_.address.resolve.toByteString)))

          case lt: LeaseTransaction =>
            TransactionMetadata.Metadata.Lease(TransactionMetadata.LeaseMetadata(lt.recipient.resolve.toByteString))

          case ext: ExchangeTransaction =>
            TransactionMetadata.Metadata.Exchange(
              TransactionMetadata.ExchangeMetadata(
                Seq(ext.order1, ext.order2).map(_.id().toByteString),
                Seq(ext.order1, ext.order2).map(_.senderAddress.toByteString),
                Seq(ext.order1, ext.order2).map(_.senderPublicKey.toByteString)
              )
            )

          case ist: InvokeScriptTransaction =>
            TransactionMetadata.Metadata.InvokeScript(invokeScriptLikeToMetadata(ist))

          case et: EthereumTransaction =>
            val metadataOpt: Option[EthereumMetadata.Action] = et.payload match {
              case ett: EthereumTransaction.Transfer =>
                ett.toTransferLike(et, blockchain).toOption.map { transferLike =>
                  EthereumMetadata.Action.Transfer(
                    TransactionMetadata.EthereumTransferMetadata(
                      ett.recipient.toByteString,
                      Some(Amount(transferLike.assetId.fold(ByteString.EMPTY)(_.id.toByteString), ett.amount))
                    )
                  )
                }

              case inv @ EthereumTransaction.Invocation(_, _) =>
                for {
                  invoke <- inv.toInvokeScriptLike(et, blockchain).toOption
                } yield EthereumMetadata.Action.Invoke(invokeScriptLikeToMetadata(invoke))
            }
            metadataOpt
              .map { a =>
                TransactionMetadata.Metadata.Ethereum(EthereumMetadata(et.timestamp, et.fee, et.signerPublicKey().toByteString, a))
              }
              .getOrElse(TransactionMetadata.Metadata.Empty)

          case _ =>
            TransactionMetadata.Metadata.Empty
        }
      )
    }
  }.toSeq

  def referencedAssets(blockchain: Blockchain, txsStateUpdates: Seq[StateUpdate]): Seq[AssetInfo] =
    txsStateUpdates
      .flatMap(st => st.assets.map(_.assetId) ++ st.balances.flatMap(_.asset.compatId))
      .distinct
      .flatMap(id => blockchain.assetDescription(IssuedAsset(id)).map(ad => AssetInfo(id, ad.decimals, ad.name.toStringUtf8)))

  def container(
      blockchainBeforeWithReward: Blockchain,
      keyBlockSnapshot: StateSnapshot
  ): (StateUpdate, Seq[StateUpdate], Seq[TransactionMetadata], Seq[AssetInfo]) = {
    val (totalSnapshot, txsStateUpdates) =
      keyBlockSnapshot.transactions
        .foldLeft((keyBlockSnapshot, Seq.empty[StateUpdate])) { case ((accSnapshot, updates), (_, txInfo)) =>
          val accBlockchain = SnapshotBlockchain(blockchainBeforeWithReward, accSnapshot)
          (
            accSnapshot |+| txInfo.snapshot,
            updates :+ atomic(accBlockchain, txInfo.snapshot)
          )
        }
    val blockchainAfter = SnapshotBlockchain(blockchainBeforeWithReward, totalSnapshot)
    val metadata        = transactionsMetadata(blockchainAfter, totalSnapshot)
    val refAssets       = referencedAssets(blockchainAfter, txsStateUpdates)
    val keyBlockUpdate  = atomic(blockchainBeforeWithReward, keyBlockSnapshot)
    (keyBlockUpdate, txsStateUpdates, metadata, refAssets)
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

    def ref: String = {
      val eventType = bu match {
        case _: BlockAppended               => "block"
        case _: MicroBlockAppended          => "micro"
        case _: RollbackCompleted           => "rollback"
        case _: MicroBlockRollbackCompleted => "micro_rollback"
      }
      s"$eventType/${bu.height}/${bu.id}"
    }
  }
}

final case class BlockAppended(
    id: ByteStr,
    height: Int,
    block: Block,
    updatedWavesAmount: Long,
    vrf: Option[ByteStr],
    activatedFeatures: Seq[Int],
    rewardShares: Seq[(Address, Long)],
    blockStateUpdate: StateUpdate,
    transactionStateUpdates: Seq[StateUpdate],
    transactionMetadata: Seq[TransactionMetadata],
    referencedAssets: Seq[StateUpdate.AssetInfo]
) extends BlockchainUpdated {
  def reverseStateUpdate: StateUpdate =
    Monoid
      .combineAll((blockStateUpdate +: transactionStateUpdates).map(_.reverse).reverse)
      .copy(deletedAliases = block.transactionData.collect { case cat: CreateAliasTransaction => cat.aliasName })
}

object BlockAppended {
  def from(
      block: Block,
      snapshot: StateSnapshot,
      blockchainBeforeWithReward: Blockchain,
      reward: Option[Long],
      hitSource: ByteStr
  ): BlockAppended = {
    val height = blockchainBeforeWithReward.height
    val (blockStateUpdate, txsStateUpdates, txsMetadata, refAssets) =
      StateUpdate.container(blockchainBeforeWithReward, snapshot)

    // updatedWavesAmount can change as a result of either genesis transactions or miner rewards
    val wavesAmount        = blockchainBeforeWithReward.wavesAmount(height).toLong
    val updatedWavesAmount = wavesAmount + reward.filter(_ => height > 0).getOrElse(0L) * blockchainBeforeWithReward.blockRewardBoost(height + 1)
    val activatedFeatures = blockchainBeforeWithReward.activatedFeatures.collect {
      case (id, activationHeight) if activationHeight == height + 1 => id.toInt
    }.toSeq

    val rewardShares =
      BlockRewardCalculator.getSortedBlockRewardShares(height + 1, reward.getOrElse(0L), block.header.generator.toAddress, blockchainBeforeWithReward)

    BlockAppended(
      block.id(),
      height + 1,
      block,
      updatedWavesAmount,
      if (block.header.version >= Block.ProtoBlockVersion) Some(hitSource) else None,
      activatedFeatures,
      rewardShares,
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
  def reverseStateUpdate: StateUpdate = Monoid
    .combineAll((microBlockStateUpdate +: transactionStateUpdates).map(_.reverse).reverse)
    .copy(deletedAliases = microBlock.transactionData.collect { case cat: CreateAliasTransaction => cat.aliasName })
}

object MicroBlockAppended {
  def from(
      microBlock: MicroBlock,
      snapshot: StateSnapshot,
      blockchainBeforeWithReward: Blockchain,
      totalBlockId: ByteStr,
      totalTransactionsRoot: ByteStr
  ): MicroBlockAppended = {
    val (microBlockStateUpdate, txsStateUpdates, txsMetadata, refAssets) =
      StateUpdate.container(blockchainBeforeWithReward, snapshot)

    MicroBlockAppended(
      totalBlockId,
      blockchainBeforeWithReward.height,
      microBlock,
      microBlockStateUpdate,
      txsStateUpdates,
      txsMetadata,
      totalTransactionsRoot,
      refAssets
    )
  }
}

final case class RollbackResult(
    removedBlocks: Seq[Block],
    removedTransactionIds: Seq[ByteStr],
    stateUpdate: StateUpdate,
    deactivatedFeatures: Seq[Int]
)

object RollbackResult {
  def micro(removedTransactionIds: Seq[ByteStr], stateUpdate: StateUpdate): RollbackResult =
    RollbackResult(Nil, removedTransactionIds, stateUpdate, Nil)

  implicit val monoid: Monoid[RollbackResult] = new Monoid[RollbackResult] {
    override def empty: RollbackResult = RollbackResult(Nil, Nil, Monoid.empty[StateUpdate], Nil)

    override def combine(x: RollbackResult, y: RollbackResult): RollbackResult = {
      RollbackResult(
        x.removedBlocks ++ y.removedBlocks,
        x.removedTransactionIds ++ y.removedTransactionIds,
        Monoid.combine(x.stateUpdate, y.stateUpdate),
        x.deactivatedFeatures ++ y.deactivatedFeatures
      )
    }
  }
}
final case class RollbackCompleted(id: ByteStr, height: Int, rollbackResult: RollbackResult, referencedAssets: Seq[StateUpdate.AssetInfo])
    extends BlockchainUpdated
final case class MicroBlockRollbackCompleted(id: ByteStr, height: Int, rollbackResult: RollbackResult, referencedAssets: Seq[StateUpdate.AssetInfo])
    extends BlockchainUpdated
