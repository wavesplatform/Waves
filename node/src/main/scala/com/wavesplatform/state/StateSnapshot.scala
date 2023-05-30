package com.wavesplatform.state
import cats.data.Ior
import cats.implicits.{catsKernelStdMonoidForMap, catsSyntaxSemigroup}
import cats.kernel.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.database.protobuf.EthereumTransactionMeta.Payload
import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot
import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot.AssetStatic
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.protobuf.{AddressExt, Amount, ByteStrExt}
import com.wavesplatform.state.StateSnapshot.lastEstimator
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.reader.LeaseDetails.Status
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}

import scala.collection.immutable.VectorMap

case class StateSnapshot(
    transactions: Vector[NewTransactionInfo],
    balances: VectorMap[(Address, Asset), Long],
    leaseBalances: Map[Address, LeaseBalance],
    assetStatics: Map[IssuedAsset, AssetStatic],
    assetVolumes: Map[IssuedAsset, AssetVolumeInfo],
    assetNamesAndDescriptions: Map[IssuedAsset, AssetInfo],
    assetScripts: Map[IssuedAsset, Option[AssetScriptInfo]],
    sponsorships: Map[IssuedAsset, SponsorshipValue],
    leaseStates: Map[ByteStr, LeaseDetails],
    aliases: Map[Alias, Address],
    orderFills: Map[ByteStr, VolumeAndFee],
    accountScripts: Map[Address, Option[AccountScriptInfo]],
    accountData: Map[Address, Map[String, DataEntry[?]]],
    scriptResults: Map[ByteStr, InvokeScriptResult],
    ethereumTransactionMeta: Map[ByteStr, EthereumTransactionMeta],
    scriptsComplexity: Long
) {
  import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot as S

  def toProtobuf(blockchain: Blockchain): TransactionStateSnapshot =
    TransactionStateSnapshot(
      balances.map { case ((address, asset), balance) =>
        S.Balance(address.toByteString, Some(Amount(asset.fold(ByteString.EMPTY)(_.id.toByteString), balance)))
      }.toSeq,
      leaseBalances.map { case (address, balance) =>
        S.LeaseBalance(address.toByteString, balance.in, balance.out)
      }.toSeq,
      assetStatics.values.toSeq,
      assetVolumes.map { case (asset, info) =>
        S.AssetVolume(asset.id.toByteString, info.isReissuable, ByteString.copyFrom(info.volume.toByteArray))
      }.toSeq,
      assetNamesAndDescriptions.map { case (asset, info) =>
        S.AssetNameAndDescription(asset.id.toByteString, info.name.toStringUtf8, info.description.toStringUtf8, info.lastUpdatedAt)
      }.toSeq,
      assetScripts.map { case (asset, script) =>
        S.AssetScript(asset.id.toByteString, script.fold(ByteString.EMPTY)(_.script.bytes().toByteString), script.fold(0L)(_.complexity))
      }.toSeq,
      aliases.map { case (alias, address) => S.Alias(address.toByteString, alias.name) }.toSeq,
      orderFills.map { case (orderId, VolumeAndFee(volume, fee)) =>
        S.OrderFill(orderId.toByteString, volume, fee)
      }.toSeq,
      leaseStates.map { case (leaseId, LeaseDetails(sender, recipient, amount, status, sourceId, height)) =>
        val pbStatus = status match {
          case Status.Active =>
            S.LeaseState.Status.Active(S.LeaseState.Active())
          case Status.Cancelled(cancelHeight, txId) =>
            S.LeaseState.Status.Cancelled(S.LeaseState.Cancelled(cancelHeight, txId.fold(ByteString.EMPTY)(_.toByteString)))
          case Status.Expired(expiredHeight) =>
            S.LeaseState.Status.Cancelled(S.LeaseState.Cancelled(expiredHeight))
        }
        val resolvedRecipient = recipient match {
          case address: Address => address
          case alias: Alias     => aliases.getOrElse(alias, blockchain.resolveAlias(recipient).explicitGet())
        }
        S.LeaseState(
          leaseId.toByteString,
          pbStatus,
          amount,
          sender.toByteString,
          ByteString.copyFrom(resolvedRecipient.bytes),
          sourceId.toByteString,
          height
        )
      }.toSeq,
      accountScripts.map { case (address, scriptOpt) =>
        scriptOpt.fold(
          S.AccountScript(
            address.toByteString,
            ByteString.EMPTY,
            ByteString.EMPTY,
            0,
            Map[String, Long]()
          )
        )(script =>
          S.AccountScript(
            address.toByteString,
            script.publicKey.toByteString,
            script.script.bytes().toByteString,
            script.verifierComplexity,
            script.complexitiesByEstimator.getOrElse(lastEstimator, Map())
          )
        )
      }.toSeq,
      accountData.map { case (address, data) =>
        S.AccountData(address.toByteString, data.values.map(PBTransactions.toPBDataEntry).toSeq)
      }.toSeq,
      sponsorships.collect { case (asset, SponsorshipValue(minFee)) =>
        S.Sponsorship(asset.id.toByteString, minFee)
      }.toSeq,
      scriptResults.map { case (txId, script) =>
        S.ScriptResult(txId.toByteString, Some(InvokeScriptResult.toPB(script, addressForTransfer = true)))
      }.toSeq,
      ethereumTransactionMeta.map { case (txId, meta) =>
        val payload = meta.payload match {
          case Payload.Empty =>
            S.EthereumTransactionMeta.Payload.Empty
          case Payload.Invocation(value) =>
            S.EthereumTransactionMeta.Payload.Invocation(S.EthereumTransactionMeta.Invocation(value.functionCall, value.payments))
          case Payload.Transfer(value) =>
            S.EthereumTransactionMeta.Payload.Transfer(S.EthereumTransactionMeta.Transfer(value.publicKeyHash, value.amount))
        }
        S.EthereumTransactionMeta(txId.toByteString, payload)
      }.toSeq
    )

  lazy val indexedAssetStatics: Map[IssuedAsset, (AssetStatic, Int)] =
    assetStatics.zipWithIndex.map { case ((asset, static), i) => asset -> (static, i + 1) }.toMap

  lazy val hashString: String =
    Integer.toHexString(hashCode())
}

object StateSnapshot {
  val lastEstimator = 3

  def fromDiff(diff: Diff, blockchain: Blockchain): StateSnapshot =
    StateSnapshot(
      diff.transactions,
      balances(diff, blockchain),
      leaseBalances(diff, blockchain),
      assetStatics(diff),
      assetVolumes(diff, blockchain),
      assetNamesAndDescriptions(diff),
      diff.assetScripts,
      diff.sponsorship.collect { case (asset, value: SponsorshipValue) => (asset, value) },
      diff.leaseState,
      diff.aliases,
      orderFills(diff, blockchain),
      diff.scripts,
      diff.accountData,
      diff.scriptResults,
      diff.ethereumTransactionMeta,
      diff.scriptsComplexity
    )

  private def balances(diff: Diff, blockchain: Blockchain): VectorMap[(Address, Asset), Long] =
    VectorMap() ++ diff.portfolios.flatMap { case (address, Portfolio(wavesAmount, _, assets)) =>
      val assetBalances = assets.map { case (assetId, balance) =>
        val newBalance = blockchain.balance(address, assetId) + balance
        (address, assetId: Asset) -> newBalance
      }
      val newBalance   = blockchain.balance(address) + wavesAmount
      val wavesBalance = (address, Waves) -> newBalance
      assetBalances + wavesBalance
    }

  private def leaseBalances(diff: Diff, blockchain: Blockchain): Map[Address, LeaseBalance] =
    diff.portfolios.flatMap { case (address, Portfolio(_, lease, _)) =>
      if (lease != LeaseBalance.empty) {
        val bLease = blockchain.leaseBalance(address)
        Map(address -> LeaseBalance(bLease.in + lease.in, bLease.out + lease.out))
      } else
        Map()
    }

  private def assetStatics(diff: Diff): Map[IssuedAsset, AssetStatic] =
    diff.issuedAssets.map { case (asset, info) =>
      asset ->
        AssetStatic(
          asset.id.toByteString,
          info.static.source.toByteString,
          info.static.issuer.toByteString,
          info.static.decimals,
          info.static.nft
        )
    }

  private def assetVolumes(diff: Diff, blockchain: Blockchain): Map[IssuedAsset, AssetVolumeInfo] = {
    val issued = diff.issuedAssets.view.mapValues(_.volume).toMap
    val updated = diff.updatedAssets.collect {
      case (asset, Ior.Right(volume))   => (asset, volume)
      case (asset, Ior.Both(_, volume)) => (asset, volume)
    }
    (issued |+| updated).map { case (asset, volume) =>
      val blockchainAsset = blockchain.assetDescription(asset)
      val newIsReissuable = blockchainAsset.map(_.reissuable && volume.isReissuable).getOrElse(volume.isReissuable)
      val newVolume       = blockchainAsset.map(_.totalVolume + volume.volume).getOrElse(volume.volume)
      asset -> AssetVolumeInfo(newIsReissuable, newVolume)
    }
  }

  private def assetNamesAndDescriptions(diff: Diff): Map[IssuedAsset, AssetInfo] = {
    val issued = diff.issuedAssets.view.mapValues(_.dynamic).toMap
    val updated = diff.updatedAssets.collect {
      case (asset, Ior.Left(info))    => (asset, info)
      case (asset, Ior.Both(info, _)) => (asset, info)
    }
    issued ++ updated
  }

  private def orderFills(diff: Diff, blockchain: Blockchain): Map[ByteStr, VolumeAndFee] =
    diff.orderFills.map { case (orderId, value) =>
      val newInfo = value |+| blockchain.filledVolumeAndFee(orderId)
      orderId -> newInfo
    }

  implicit val monoid: Monoid[StateSnapshot] = new Monoid[StateSnapshot] {
    override val empty: StateSnapshot =
      StateSnapshot(Vector(), VectorMap(), Map(), Map(), Map(), Map(), Map(), Map(), Map(), Map(), Map(), Map(), Map(), Map(), Map(), 0)

    override def combine(s1: StateSnapshot, s2: StateSnapshot): StateSnapshot =
      StateSnapshot(
        s1.transactions ++ s2.transactions,
        s1.balances ++ s2.balances,
        s1.leaseBalances ++ s2.leaseBalances,
        s1.assetStatics ++ s2.assetStatics,
        s1.assetVolumes ++ s2.assetVolumes,
        s1.assetNamesAndDescriptions ++ s2.assetNamesAndDescriptions,
        s1.assetScripts ++ s2.assetScripts,
        s1.sponsorships ++ s2.sponsorships,
        s1.leaseStates ++ s2.leaseStates,
        s1.aliases ++ s2.aliases,
        s1.orderFills ++ s2.orderFills,
        s1.accountScripts ++ s2.accountScripts,
        combineDataEntries(s1.accountData, s2.accountData),
        s1.scriptResults ++ s2.scriptResults,
        s1.ethereumTransactionMeta ++ s2.ethereumTransactionMeta,
        s1.scriptsComplexity + s2.scriptsComplexity
      )

    private def combineDataEntries(
        entries1: Map[Address, Map[String, DataEntry[_]]],
        entries2: Map[Address, Map[String, DataEntry[_]]]
    ): Map[Address, Map[String, DataEntry[_]]] =
      entries2.foldLeft(entries1) { case (result, (address, addressEntries2)) =>
        val resultAddressEntries =
          result
            .get(address)
            .fold(address -> addressEntries2)(addressEntries1 => address -> (addressEntries1 ++ addressEntries2))
        result + resultAddressEntries
      }
  }

  val empty: StateSnapshot = monoid.empty
}
