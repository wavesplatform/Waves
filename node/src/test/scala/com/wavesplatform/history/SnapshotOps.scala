package com.wavesplatform.history

import cats.data.Ior
import com.wavesplatform.account.{Address, AddressScheme, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.database.protobuf.EthereumTransactionMeta.Payload
import com.wavesplatform.lang.script.ScriptReader
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBTransactions}
import com.wavesplatform.state.*
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}

import scala.collection.immutable.VectorMap

object SnapshotOps {
  implicit class TransactionStateSnapshotExt(val s: StateSnapshot) extends AnyVal {
    import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot as S

    def toDiff(blockchain: Blockchain): Diff =
      Diff.withTransactions(
        s.transactions,
        portfolios(blockchain),
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
        s.current.totalComplexity,
        scriptResults,
        ethereumTransactionMeta
      )

    private def portfolios(blockchain: Blockchain): Map[Address, Portfolio] =
      Diff.combine(balancePortfolios(blockchain), leasePortfolios(blockchain)).explicitGet()

    private def balancePortfolios(blockchain: Blockchain): Map[Address, Portfolio] =
      s.current.balances
        .foldLeft(Map[Address, Portfolio]()) { (portfolios, nextBalance) =>
          val asset =
            if (nextBalance.getAmount.assetId.isEmpty) Waves
            else nextBalance.getAmount.assetId.toAssetId
          val address   = nextBalance.address.toAddress
          val portfolio = Portfolio.build(asset, nextBalance.getAmount.amount - blockchain.balance(address, asset))
          Diff.combine(portfolios, Map(address -> portfolio)).explicitGet()
        }

    private def leasePortfolios(blockchain: Blockchain): Map[Address, Portfolio] =
      s.current.leaseBalances
        .map {
          current =>
            val init = blockchain.leaseBalance(current.address.toAddress)
            current.address.toAddress -> Portfolio(lease = LeaseBalance(in = current.in - init.in, out = current.out - init.out))
        }
        .toMap

    private def issuedAssets: VectorMap[IssuedAsset, NewAssetInfo] =
      VectorMap[IssuedAsset, NewAssetInfo]() ++ s.current.assetStatics.map { a =>
        val static = AssetStaticInfo(a.assetId.toByteStr, a.sourceTransactionId.toTxId, a.issuer.toPublicKey, a.decimals, a.nft)
        val asset  = a.assetId.toIssuedAssetId
        asset -> NewAssetInfo(static, assetDynamics(asset), assetVolumes(asset))
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
      s.current.assetVolumes.map(v => v.assetId.toIssuedAssetId -> AssetVolumeInfo(v.reissuable, BigInt(v.volume.toByteArray))).toMap

    private def assetDynamics: Map[IssuedAsset, AssetInfo] =
      s.current.assetNamesAndDescriptions.map(a => a.assetId.toIssuedAssetId -> AssetInfo(a.name, a.description, Height @@ a.lastUpdated)).toMap

    private def aliases: Map[Alias, Address] =
      s.current.aliases.map(a => Alias.create(a.alias).explicitGet() -> a.address.toAddress).toMap

    private def orderFills: Map[ByteStr, VolumeAndFee] =
      s.current.orderFills.map(of => of.orderId.toByteStr -> VolumeAndFee(of.volume, of.fee)).toMap

    private def leaseStates: Map[ByteStr, LeaseDetails] =
      s.current.leaseStates
        .map(ls =>
          ls.leaseId.toByteStr ->
            LeaseDetails(
              PublicKey @@ ls.sender.toByteStr,
              PBRecipients.toAddress(ls.recipient.toByteArray, AddressScheme.current.chainId).explicitGet(),
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
      s.current.accountScripts.map { pbInfo =>
        val info =
          if (pbInfo.script.isEmpty)
            None
          else
            Some(
              AccountScriptInfo(
                pbInfo.senderPublicKey.toPublicKey,
                ScriptReader.fromBytes(pbInfo.script.toByteArray).explicitGet(),
                pbInfo.verifierComplexity,
                if (pbInfo.callableComplexities.nonEmpty) Map(3 -> pbInfo.callableComplexities) else Map()
              )
            )
        pbInfo.senderAddress.toAddress -> info
      }.toMap

    private def assetScripts: Map[IssuedAsset, Option[AssetScriptInfo]] =
      s.current.assetScripts.map { pbInfo =>
        val info =
          if (pbInfo.script.isEmpty)
            None
          else
            Some(AssetScriptInfo(ScriptReader.fromBytes(pbInfo.script.toByteArray).explicitGet(), pbInfo.complexity))
        pbInfo.assetId.toIssuedAssetId -> info
      }.toMap

    private def accountData: Map[Address, Map[String, DataEntry[_]]] =
      s.current.accountData
        .map(data => data.address.toAddress -> data.entries.map(e => e.key -> PBTransactions.toVanillaDataEntry(e)).toMap)
        .toMap

    private def sponsorships: Map[IssuedAsset, SponsorshipValue] =
      s.current.sponsorships
        .map(data => data.assetId.toIssuedAssetId -> SponsorshipValue(data.minFee))
        .toMap

    private def scriptResults: Map[ByteStr, InvokeScriptResult] =
      s.current.scriptResults.map(r => r.transactionId.toByteStr -> InvokeScriptResult.fromPB(r.getResult)).toMap

    private def ethereumTransactionMeta: Map[ByteStr, EthereumTransactionMeta] =
      s.current.ethereumTransactionMeta.map { m =>
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
