package com.wavesplatform.protobuf

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.KeyLength
import com.wavesplatform.lang.script.ScriptReader
import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot
import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot.AssetStatic
import com.wavesplatform.protobuf.transaction.{PBAmounts, PBTransactions}
import com.wavesplatform.state.*
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.reader.LeaseDetails.Status
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset

import scala.collection.immutable.VectorMap

object PBSnapshots {

  import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot as S

  def toProtobuf(snapshot: StateSnapshot, txStatus: TxMeta.Status): TransactionStateSnapshot = {
    import snapshot.*
    TransactionStateSnapshot(
      balances.map { case ((address, asset), balance) =>
        S.Balance(address.toByteString, Some(PBAmounts.fromAssetAndAmount(asset, balance)))
      }.toSeq,
      leaseBalances.map { case (address, balance) =>
        S.LeaseBalance(address.toByteString, balance.in, balance.out)
      }.toSeq,
      assetStatics.map { case (id, st) =>
        AssetStatic(id.id.toByteString, st.issuer.toByteString, st.decimals, st.nft)
      }.toSeq,
      assetVolumes.map { case (asset, info) =>
        S.AssetVolume(asset.id.toByteString, info.isReissuable, ByteString.copyFrom(info.volume.toByteArray))
      }.toSeq,
      assetNamesAndDescriptions.map { case (asset, info) =>
        S.AssetNameAndDescription(asset.id.toByteString, info.name.toStringUtf8, info.description.toStringUtf8)
      }.toSeq,
      assetScripts.map { case (asset, script) =>
        S.AssetScript(asset.id.toByteString, script.script.bytes().toByteString)
      }.headOption,
      aliases.map { case (alias, address) => S.Alias(address.toByteString, alias.name) }.headOption,
      orderFills.map { case (orderId, VolumeAndFee(volume, fee)) =>
        S.OrderFill(orderId.toByteString, volume, fee)
      }.toSeq,
      leaseStates.map { case (leaseId, LeaseSnapshot(sender, recipient, amount, status)) =>
        val pbStatus = status match {
          case Status.Active =>
            S.LeaseState.Status.Active(S.LeaseState.Active(amount, sender.toByteString, recipient.asInstanceOf[Address].toByteString))
          case _: Status.Cancelled | _: Status.Expired =>
            S.LeaseState.Status.Cancelled(S.LeaseState.Cancelled())
        }
        S.LeaseState(leaseId.toByteString, pbStatus)
      }.toSeq,
      accountScripts.map { case (publicKey, scriptOpt) =>
        scriptOpt.fold(
          S.AccountScript(publicKey.toByteString)
        )(script =>
          S.AccountScript(
            publicKey.toByteString,
            script.script.bytes().toByteString,
            script.verifierComplexity
          )
        )
      }.headOption,
      accountData.map { case (address, data) =>
        S.AccountData(address.toByteString, data.values.map(PBTransactions.toPBDataEntry).toSeq)
      }.toSeq,
      sponsorships.collect { case (asset, SponsorshipValue(minFee)) =>
        S.Sponsorship(asset.id.toByteString, minFee)
      }.toSeq,
      txStatus.protobuf
    )
  }

  def fromProtobuf(pbSnapshot: TransactionStateSnapshot, txId: ByteStr, height: Int): (StateSnapshot, TxMeta.Status) = {
    val balances: VectorMap[(Address, Asset), Long] =
      VectorMap() ++ pbSnapshot.balances.map(b => (b.address.toAddress(), b.getAmount.assetId.toAssetId) -> b.getAmount.amount)

    val leaseBalances: Map[Address, LeaseBalance] =
      pbSnapshot.leaseBalances
        .map(b => b.address.toAddress() -> LeaseBalance(b.in, b.out))
        .toMap

    val assetScripts: Map[IssuedAsset, AssetScriptInfo] =
      pbSnapshot.assetScripts.map { s =>
        s.assetId.toIssuedAssetId -> AssetScriptInfo(ScriptReader.fromBytes(s.script.toByteArray).explicitGet(), 0)
      }.toMap

    val assetStatics: VectorMap[IssuedAsset, AssetStaticInfo] =
      VectorMap() ++ pbSnapshot.assetStatics.map(info =>
        info.assetId.toIssuedAssetId -> AssetStaticInfo(
          info.assetId.toByteStr,
          TransactionId(txId),
          PublicKey(info.issuerPublicKey.toByteStr),
          info.decimals,
          info.nft
        )
      )

    val assetVolumes: Map[IssuedAsset, AssetVolumeInfo] =
      pbSnapshot.assetVolumes
        .map(v => v.assetId.toIssuedAssetId -> AssetVolumeInfo(v.reissuable, BigInt(v.volume.toByteArray)))
        .toMap

    val assetNamesAndDescriptions: Map[IssuedAsset, AssetInfo] =
      pbSnapshot.assetNamesAndDescriptions
        .map(i => i.assetId.toIssuedAssetId -> AssetInfo(i.name, i.description, Height @@ height))
        .toMap

    val sponsorships: Map[IssuedAsset, SponsorshipValue] =
      pbSnapshot.sponsorships
        .map(s => s.assetId.toIssuedAssetId -> SponsorshipValue(s.minFee))
        .toMap

    val leaseStates: Map[ByteStr, LeaseSnapshot] =
      pbSnapshot.leaseStates.map { ls =>
        ls.status match {
          case TransactionStateSnapshot.LeaseState.Status.Active(value) =>
            ls.leaseId.toByteStr -> LeaseSnapshot(
              value.sender.toPublicKey,
              value.recipient.toAddress(),
              value.amount,
              LeaseDetails.Status.Active
            )
          case _: TransactionStateSnapshot.LeaseState.Status.Cancelled | TransactionStateSnapshot.LeaseState.Status.Empty =>
            ls.leaseId.toByteStr -> LeaseSnapshot(
              PublicKey(ByteStr.fill(KeyLength)(0)),
              Address(Array.fill(Address.HashLength)(0)),
              0,
              LeaseDetails.Status.Cancelled(0, None)
            )
        }
      }.toMap

    val aliases: Map[Alias, Address] =
      pbSnapshot.aliases
        .map(a => Alias.create(a.alias).explicitGet() -> a.address.toAddress())
        .toMap

    val orderFills: Map[ByteStr, VolumeAndFee] =
      pbSnapshot.orderFills
        .map(of => of.orderId.toByteStr -> VolumeAndFee(of.volume, of.fee))
        .toMap

    val accountScripts: Map[PublicKey, Option[AccountScriptInfo]] =
      pbSnapshot.accountScripts.map { pbInfo =>
        val info =
          if (pbInfo.script.isEmpty)
            None
          else
            Some(
              AccountScriptInfo(
                pbInfo.senderPublicKey.toPublicKey,
                ScriptReader.fromBytes(pbInfo.script.toByteArray).explicitGet(),
                pbInfo.verifierComplexity
              )
            )
        pbInfo.senderPublicKey.toPublicKey -> info
      }.toMap

    val accountData: Map[Address, Map[String, DataEntry[?]]] =
      pbSnapshot.accountData.map { data =>
        val entries =
          data.entries.map { pbEntry =>
            val entry = PBTransactions.toVanillaDataEntry(pbEntry)
            entry.key -> entry
          }.toMap
        data.address.toAddress() -> entries
      }.toMap

    (
      StateSnapshot(
        VectorMap(),
        balances,
        leaseBalances,
        assetStatics,
        assetVolumes,
        assetNamesAndDescriptions,
        assetScripts,
        sponsorships,
        leaseStates,
        aliases,
        orderFills,
        accountScripts,
        accountData
      ),
      TxMeta.Status.fromProtobuf(pbSnapshot.transactionStatus)
    )
  }

}
