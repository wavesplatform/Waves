package com.wavesplatform.state
import cats.data.Ior
import cats.implicits.catsSyntaxAlternativeSeparate
import com.google.protobuf.ByteString
import com.wavesplatform.account.{AddressScheme, Alias, PublicKey}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.database.protobuf.EthereumTransactionMeta.Payload
import com.wavesplatform.lang.script.ScriptReader
import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBTransactions}
import com.wavesplatform.protobuf.{AddressExt, Amount, ByteStrExt, ByteStringExt}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.reader.LeaseDetails.Status
import com.wavesplatform.transaction.Asset.IssuedAsset

import scala.collection.immutable.VectorMap

object StateSnapshot {
  import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot as S
  private val lastEstimator = 3

  def fromDiff(diff: Diff): TransactionStateSnapshot = {
    val (balances, leaseBalance) =
      diff.portfolios.toSeq.map { case (address, Portfolio(wavesAmount, lease, assets)) =>
        val assetBalances =
          assets.map { case (assetId, balance) =>
            S.Balance(address.toByteString, Some(Amount(assetId.id.toByteString, balance)))
          }
        val wavesBalance = S.Balance(address.toByteString, Some(Amount(ByteString.EMPTY, wavesAmount)))
        val leaseBalance = S.LeaseBalance(address.toByteString, in = lease.in, out = lease.out)
        (assetBalances.toSeq :+ wavesBalance, leaseBalance)
      }.separate
    val assetVolumes =
      diff.updatedAssets.collect {
        case (asset, Ior.Right(volume))   => (asset, volume)
        case (asset, Ior.Both(_, volume)) => (asset, volume)
      } ++ diff.issuedAssets.map { case (asset, info) => (asset, info.volume) }
    val assetNamesAndDescriptions =
      diff.updatedAssets.collect {
        case (asset, Ior.Left(info))    => (asset, info)
        case (asset, Ior.Both(info, _)) => (asset, info)
      } ++ diff.issuedAssets.map { case (asset, info) => (asset, info.dynamic) }
    TransactionStateSnapshot(
      balances.flatten,
      leaseBalance,
      diff.issuedAssets.map { case (asset, info) =>
        S.AssetStatic(asset.id.toByteString, info.static.source.toByteString, info.static.issuer.toByteString, info.static.decimals, info.static.nft)
      }.toSeq,
      assetVolumes.map { case (asset, volume) =>
        S.AssetVolume(asset.id.toByteString, volume.isReissuable, ByteString.copyFrom(volume.volume.toByteArray))
      }.toSeq,
      assetNamesAndDescriptions.map { case (asset, info) =>
        S.AssetNameAndDescription(asset.id.toByteString, info.name.toStringUtf8, info.description.toStringUtf8, info.lastUpdatedAt)
      }.toSeq,
      diff.assetScripts.map { case (asset, script) =>
        S.AssetScript(asset.id.toByteString, script.fold(ByteString.EMPTY)(_.script.bytes().toByteString), script.map(_.complexity).getOrElse(0))
      }.toSeq,
      diff.aliases.map { case (alias, address) => S.Alias(address.toByteString, alias.name) }.toSeq,
      diff.orderFills.map { case (orderId, VolumeAndFee(volume, fee)) => S.OrderFill(orderId.toByteString, volume, fee) }.toSeq,
      diff.leaseState.map { case (leaseId, LeaseDetails(sender, recipient, amount, status, sourceId, height)) =>
        val pbStatus = status match {
          case Status.Active =>
            S.LeaseState.Status.Active(S.LeaseState.Active())
          case Status.Cancelled(cancelHeight, txId) =>
            S.LeaseState.Status.Cancelled(S.LeaseState.Cancelled(cancelHeight, txId.fold(ByteString.EMPTY)(_.toByteString)))
          case Status.Expired(expiredHeight) =>
            S.LeaseState.Status.Cancelled(S.LeaseState.Cancelled(expiredHeight))
        }
        S.LeaseState(leaseId.toByteString, pbStatus, amount, sender.toByteString, Some(PBRecipients.create(recipient)), sourceId.toByteString, height)
      }.toSeq,
      diff.scripts.map { case (address, scriptOpt) =>
        S.AccountScript(
          address.toByteString,
          scriptOpt.fold(ByteString.EMPTY)(_.script.bytes().toByteString),
          scriptOpt.fold(0L)(_.verifierComplexity),
          scriptOpt.fold(Map[String, Long]())(_.complexitiesByEstimator(lastEstimator))
        )
      }.toSeq,
      diff.accountData.map { case (address, data) =>
        S.AccountData(address.toByteString, data.data.values.map(PBTransactions.toPBDataEntry).toSeq)
      }.toSeq,
      diff.sponsorship.map { case (asset, sponsorship) =>
        val minFee = sponsorship match {
          case SponsorshipValue(minFee) => minFee
          case SponsorshipNoInfo        => 0
        }
        S.Sponsorship(asset.id.toByteString, minFee)
      }.toSeq,
      diff.scriptResults.map { case (txId, script) =>
        S.ScriptResult(txId.toByteString, Some(InvokeScriptResult.toPB(script, addressForTransfer = true)))
      }.toSeq,
      diff.ethereumTransactionMeta.map { case (txId, meta) =>
        val payload = meta.payload match {
          case Payload.Empty =>
            S.EthereumTransactionMeta.Payload.Empty
          case Payload.Invocation(value) =>
            S.EthereumTransactionMeta.Payload.Invocation(S.EthereumTransactionMeta.Invocation(value.functionCall, value.payments))
          case Payload.Transfer(value) =>
            S.EthereumTransactionMeta.Payload.Transfer(S.EthereumTransactionMeta.Transfer(value.publicKeyHash, value.amount))
        }
        S.EthereumTransactionMeta(txId.toByteString, payload)
      }.toSeq,
      diff.scriptsComplexity
    )
  }

  def toDiff(s: TransactionStateSnapshot): Diff = {
    val balances =
      s.balances.map { b =>
        val portfolio =
          if (b.getAmount.assetId.isEmpty) Portfolio.waves(b.getAmount.amount)
          else Portfolio.build(b.getAmount.assetId.toAssetId, b.getAmount.amount)
        b.address.toAddress -> portfolio
      }.toMap
    val leaseBalances = s.leaseBalances.map(b => b.address.toAddress -> Portfolio(lease = LeaseBalance(in = b.in, out = b.out))).toMap
    val assetVolumes  = s.assetVolumes.map(v => v.assetId.toAssetId -> AssetVolumeInfo(v.reissuable, BigInt(v.volume.toByteArray))).toMap
    val assetDynamics =
      s.assetNamesAndDescriptions.map(a => a.assetId.toAssetId -> AssetInfo(a.name, a.description, Height @@ a.lastUpdated)).toMap
    val issuedAssets =
      VectorMap[IssuedAsset, NewAssetInfo]() ++ s.assetStatics.map { s =>
        val static = AssetStaticInfo(TransactionId @@ s.sourceTransactionId.toByteStr, PublicKey @@ s.issuer.toByteStr, s.decimals, s.nft)
        val asset  = s.assetId.toAssetId
        s.assetId.toAssetId -> NewAssetInfo(static, assetDynamics(asset), assetVolumes(asset))
      }
    val updatedAssets =
      (assetVolumes.keySet ++ assetDynamics.keySet)
        .filterNot(issuedAssets.contains)
        .map { asset =>
          val info =
            (assetDynamics.get(asset), assetVolumes.get(asset)) match {
              case (Some(dynamic), Some(volume)) => Ior.Both(dynamic, volume)
              case (Some(dynamic), None)         => Ior.Left(dynamic)
              case (None, Some(volume))          => Ior.Right(volume)
              case _                             => ???
            }
          asset -> info
        }
        .toMap
    Diff(
      Diff.combine(balances, leaseBalances).explicitGet(),
      issuedAssets,
      updatedAssets,
      s.aliases.map(a => Alias.create(a.alias).explicitGet() -> a.address.toAddress).toMap,
      s.orderFills.map(of => of.orderId.toByteStr -> VolumeAndFee(of.volume, of.fee)).toMap,
      s.leaseStates
        .map(ls =>
          ls.leaseId.toByteStr ->
            LeaseDetails(
              PublicKey @@ ls.sender.toByteStr,
              PBRecipients.toAddressOrAlias(ls.getRecipient, AddressScheme.current.chainId).explicitGet(),
              ls.amount,
              ls.status match {
                case S.LeaseState.Status.Cancelled(c) =>
                  LeaseDetails.Status.Cancelled(c.height, if (c.transactionId.isEmpty) None else Some(c.transactionId.toByteStr))
                case S.LeaseState.Status.Active(_) =>
                  LeaseDetails.Status.Active
                case _ =>
                  ???
              },
              ls.originTransactionId.toByteStr,
              ls.height
            )
        )
        .toMap,
      s.accountScripts.map { pbInfo =>
        val info =
          if (pbInfo.script.isEmpty)
            None
          else
            Some(
              AccountScriptInfo(
                pbInfo.senderPublicKey.toPublicKey,
                ScriptReader.fromBytes(pbInfo.script.toByteArray).explicitGet(),
                pbInfo.verifierComplexity,
                Map(lastEstimator -> pbInfo.callableComplexities)
              )
            )
        pbInfo.senderPublicKey.toPublicKey.toAddress -> info
      }.toMap,
      s.assetScripts.map { pbInfo =>
        val info =
          if (pbInfo.script.isEmpty)
            None
          else
            Some(AssetScriptInfo(ScriptReader.fromBytes(pbInfo.script.toByteArray).explicitGet(), pbInfo.complexity))
        pbInfo.assetId.toAssetId -> info
      }.toMap,
      s.accountData
        .map(data => data.address.toAddress -> AccountDataInfo(data.entry.map(e => e.key -> PBTransactions.toVanillaDataEntry(e)).toMap))
        .toMap,
      s.sponsorships
        .map(data => data.assetId.toAssetId -> (if (data.minFee == 0) SponsorshipNoInfo else SponsorshipValue(data.minFee)))
        .toMap,
      scriptsRun = 0,
      s.totalComplexity,
      s.scriptResults.map(r => r.transactionId.toByteStr -> InvokeScriptResult.fromPB(r.getResult)).toMap,
      s.ethereumTransactionMeta.map { m =>
        val payload = m.payload match {
          case S.EthereumTransactionMeta.Payload.Empty =>
            Payload.Empty
          case S.EthereumTransactionMeta.Payload.Invocation(S.EthereumTransactionMeta.Invocation(functionCall, payments, _)) =>
            Payload.Invocation(EthereumTransactionMeta.Invocation(functionCall, payments))
          case S.EthereumTransactionMeta.Payload.Transfer(S.EthereumTransactionMeta.Transfer(publicKeyHash, amount, _)) =>
            Payload.Transfer(EthereumTransactionMeta.Transfer(publicKeyHash, amount))
        }
        m.transactionId.toByteStr -> EthereumTransactionMeta(payload)
      }.toMap
    )
  }
}
