package com.wavesplatform.state
import cats.data.Ior
import cats.implicits.catsSyntaxSemigroup
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressScheme, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.database.protobuf.EthereumTransactionMeta.Payload
import com.wavesplatform.lang.script.ScriptReader
import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBTransactions}
import com.wavesplatform.protobuf.{AddressExt, Amount, ByteStrExt, ByteStringExt}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.reader.LeaseDetails.Status
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}

import scala.collection.immutable.VectorMap

object StateSnapshot {
  import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot as S
  private val lastEstimator = 3

  def create(diff: Diff, blockchain: Blockchain): TransactionStateSnapshot =
    TransactionStateSnapshot(
      balances(diff, blockchain),
      leaseBalances(diff, blockchain),
      assetStatics(diff),
      assetVolumes(diff, blockchain),
      assetNamesAndDescriptions(diff),
      assetScripts(diff),
      aliases(diff),
      orderFills(diff, blockchain),
      leaseStates(diff),
      accountScripts(diff),
      accountData(diff),
      sponsorships(diff),
      scriptResults(diff),
      ethereumTransactionMeta(diff),
      diff.scriptsComplexity
    )

  private def balances(diff: Diff, blockchain: Blockchain): Seq[S.Balance] =
    diff.portfolios.flatMap { case (address, Portfolio(wavesAmount, _, assets)) =>
      val assetBalances = assets.map { case (assetId, balance) =>
        val newBalance = blockchain.balance(address, assetId) + balance
        S.Balance(address.toByteString, Some(Amount(assetId.id.toByteString, newBalance)))
      }
      val newBalance   = blockchain.balance(address) + wavesAmount
      val wavesBalance = S.Balance(address.toByteString, Some(Amount(ByteString.EMPTY, newBalance)))
      assetBalances.toSeq :+ wavesBalance
    }.toSeq

  private def leaseBalances(diff: Diff, blockchain: Blockchain): Seq[S.LeaseBalance] =
    diff.portfolios.flatMap { case (address, Portfolio(_, lease, _)) =>
      if (lease != LeaseBalance.empty) {
        val bLease = blockchain.leaseBalance(address)
        Seq(S.LeaseBalance(address.toByteString, bLease.in + lease.in, bLease.out + lease.out))
      } else
        Nil
    }.toSeq

  private def assetStatics(diff: Diff): Seq[S.AssetStatic] =
    diff.issuedAssets.map { case (asset, info) =>
      S.AssetStatic(
        asset.id.toByteString,
        info.static.source.toByteString,
        info.static.issuer.toByteString,
        info.static.decimals,
        info.static.nft
      )
    }.toSeq

  private def assetVolumes(diff: Diff, blockchain: Blockchain): Seq[S.AssetVolume] = {
    val issued = diff.issuedAssets.view.mapValues(_.volume).toMap
    val updated = diff.updatedAssets.collect {
      case (asset, Ior.Right(volume))   => (asset, volume)
      case (asset, Ior.Both(_, volume)) => (asset, volume)
    }
    (issued |+| updated).map { case (asset, volume) =>
      val bVolume        = blockchain.assetDescription(asset).map(_.totalVolume).getOrElse(BigInt(0))
      val newVolume      = bVolume + volume.volume
      val newVolumeBytes = ByteString.copyFrom(newVolume.toByteArray)
      S.AssetVolume(asset.id.toByteString, volume.isReissuable, newVolumeBytes)
    }.toSeq
  }

  private def assetNamesAndDescriptions(diff: Diff): Seq[S.AssetNameAndDescription] = {
    val issued = diff.issuedAssets.view.mapValues(_.dynamic).toMap
    val updated = diff.updatedAssets.collect {
      case (asset, Ior.Left(info))    => (asset, info)
      case (asset, Ior.Both(info, _)) => (asset, info)
    }
    (issued ++ updated).map { case (asset, info) =>
      S.AssetNameAndDescription(asset.id.toByteString, info.name.toStringUtf8, info.description.toStringUtf8, info.lastUpdatedAt)
    }.toSeq
  }

  private def assetScripts(diff: Diff): Seq[S.AssetScript] =
    diff.assetScripts.map { case (asset, script) =>
      S.AssetScript(asset.id.toByteString, script.fold(ByteString.EMPTY)(_.script.bytes().toByteString), script.fold(0L)(_.complexity))
    }.toSeq

  private def aliases(diff: Diff): Seq[S.Alias] =
    diff.aliases.map { case (alias, address) => S.Alias(address.toByteString, alias.name) }.toSeq

  private def orderFills(diff: Diff, blockchain: Blockchain): Seq[S.OrderFill] =
    diff.orderFills.map { case (orderId, value) =>
      val VolumeAndFee(newVolume, newFee) = value |+| blockchain.filledVolumeAndFee(orderId)
      S.OrderFill(orderId.toByteString, newVolume, newFee)
    }.toSeq

  private def leaseStates(diff: Diff): Seq[S.LeaseState] =
    diff.leaseState.map { case (leaseId, LeaseDetails(sender, recipient, amount, status, sourceId, height)) =>
      val pbStatus = status match {
        case Status.Active =>
          S.LeaseState.Status.Active(S.LeaseState.Active())
        case Status.Cancelled(cancelHeight, txId) =>
          S.LeaseState.Status.Cancelled(S.LeaseState.Cancelled(cancelHeight, txId.fold(ByteString.EMPTY)(_.toByteString)))
        case Status.Expired(expiredHeight) =>
          S.LeaseState.Status.Cancelled(S.LeaseState.Cancelled(expiredHeight))
      }
      S.LeaseState(
        leaseId.toByteString,
        pbStatus,
        amount,
        sender.toByteString,
        Some(PBRecipients.create(recipient)),
        sourceId.toByteString,
        height
      )
    }.toSeq

  private def accountScripts(diff: Diff): Seq[S.AccountScript] =
    diff.scripts.map { case (address, scriptOpt) =>
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
    }.toSeq

  private def accountData(diff: Diff): Seq[S.AccountData] =
    diff.accountData.map { case (address, data) =>
      S.AccountData(address.toByteString, data.data.values.map(PBTransactions.toPBDataEntry).toSeq)
    }.toSeq

  private def sponsorships(diff: Diff): Seq[S.Sponsorship] =
    diff.sponsorship.collect { case (asset, SponsorshipValue(minFee)) =>
      S.Sponsorship(asset.id.toByteString, minFee)
    }.toSeq

  private def scriptResults(diff: Diff): Seq[S.ScriptResult] =
    diff.scriptResults.map { case (txId, script) =>
      S.ScriptResult(txId.toByteString, Some(InvokeScriptResult.toPB(script, addressForTransfer = true)))
    }.toSeq

  private def ethereumTransactionMeta(diff: Diff): Seq[S.EthereumTransactionMeta] =
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
    }.toSeq

  implicit class DiffOps(val diff: Diff) extends AnyVal {}

  implicit class TransactionStateSnapshotOps(val s: TransactionStateSnapshot) extends AnyVal {
    def toDiff: Diff =
      Diff(
        portfolios,
        issuedAssets,
        updatedAssets,
        aliases,
        orderFills,
        leaseStates,
        accountScripts,
        assetScripts,
        accountData,
        sponsorships,
        scriptsRun = 0,
        s.totalComplexity,
        scriptResults,
        ethereumTransactionMeta
      )

    private def portfolios: Map[Address, Portfolio] =
      Diff.combine(balancePortfolios, leasePortfolios).explicitGet()

    private def balancePortfolios: Map[Address, Portfolio] =
      s.balances
        .foldLeft(Map[Address, Portfolio]()) { (portfolios, nextBalance) =>
          val asset =
            if (nextBalance.getAmount.assetId.isEmpty) Waves
            else nextBalance.getAmount.assetId.toAssetId
          val portfolio = Portfolio.build(asset, nextBalance.getAmount.amount)
          val address   = nextBalance.address.toAddress
          Diff.combine(portfolios, Map(address -> portfolio)).explicitGet()
        }

    private def leasePortfolios: Map[Address, Portfolio] =
      s.leaseBalances
        .map(b => b.address.toAddress -> Portfolio(lease = LeaseBalance(in = b.in, out = b.out)))
        .toMap

    private def issuedAssets: VectorMap[IssuedAsset, NewAssetInfo] =
      VectorMap[IssuedAsset, NewAssetInfo]() ++ s.assetStatics.map { s =>
        val static = AssetStaticInfo(TransactionId @@ s.sourceTransactionId.toByteStr, PublicKey @@ s.issuer.toByteStr, s.decimals, s.nft)
        val asset  = s.assetId.toAssetId
        s.assetId.toAssetId -> NewAssetInfo(static, assetDynamics(asset), assetVolumes(asset))
      }

    private def updatedAssets: Map[IssuedAsset, Ior[AssetInfo, AssetVolumeInfo]] =
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

    private def assetVolumes: Map[IssuedAsset, AssetVolumeInfo] =
      s.assetVolumes.map(v => v.assetId.toAssetId -> AssetVolumeInfo(v.reissuable, BigInt(v.volume.toByteArray))).toMap

    private def assetDynamics: Map[IssuedAsset, AssetInfo] =
      s.assetNamesAndDescriptions.map(a => a.assetId.toAssetId -> AssetInfo(a.name, a.description, Height @@ a.lastUpdated)).toMap

    private def aliases: Map[Alias, Address] =
      s.aliases.map(a => Alias.create(a.alias).explicitGet() -> a.address.toAddress).toMap

    private def orderFills: Map[ByteStr, VolumeAndFee] =
      s.orderFills.map(of => of.orderId.toByteStr -> VolumeAndFee(of.volume, of.fee)).toMap

    private def leaseStates: Map[ByteStr, LeaseDetails] =
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
                case _ =>
                  LeaseDetails.Status.Active
              },
              ls.originTransactionId.toByteStr,
              ls.height
            )
        )
        .toMap

    private def accountScripts: Map[Address, Option[AccountScriptInfo]] =
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
                if (pbInfo.callableComplexities.nonEmpty) Map(lastEstimator -> pbInfo.callableComplexities) else Map()
              )
            )
        pbInfo.senderAddress.toAddress -> info
      }.toMap

    private def assetScripts: Map[IssuedAsset, Option[AssetScriptInfo]] =
      s.assetScripts.map { pbInfo =>
        val info =
          if (pbInfo.script.isEmpty)
            None
          else
            Some(AssetScriptInfo(ScriptReader.fromBytes(pbInfo.script.toByteArray).explicitGet(), pbInfo.complexity))
        pbInfo.assetId.toAssetId -> info
      }.toMap

    private def accountData: Map[Address, AccountDataInfo] =
      s.accountData
        .map(data => data.address.toAddress -> AccountDataInfo(data.entry.map(e => e.key -> PBTransactions.toVanillaDataEntry(e)).toMap))
        .toMap

    private def sponsorships: Map[IssuedAsset, SponsorshipValue] =
      s.sponsorships
        .map(data => data.assetId.toAssetId -> SponsorshipValue(data.minFee))
        .toMap

    private def scriptResults: Map[ByteStr, InvokeScriptResult] =
      s.scriptResults.map(r => r.transactionId.toByteStr -> InvokeScriptResult.fromPB(r.getResult)).toMap

    private def ethereumTransactionMeta: Map[ByteStr, EthereumTransactionMeta] =
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
  }
}
