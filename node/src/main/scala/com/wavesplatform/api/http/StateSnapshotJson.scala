package com.wavesplatform.api.http

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.http.StateSnapshotJson.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.bls.BlsPublicKey
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.*
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import play.api.libs.json.*
import play.api.libs.json.Json.MacroOptions
import play.api.libs.json.JsonConfiguration.Aux
import play.api.libs.json.OptionHandlers.WritesNull

case class StateSnapshotJson(
    applicationStatus: String,
    balances: Seq[BalanceJson],
    leaseBalances: Seq[LeaseBalanceJson],
    assetStatics: Seq[AssetStaticInfo],
    assetVolumes: Seq[AssetVolumeJson],
    assetNamesAndDescriptions: Seq[AssetInfoJson],
    assetScripts: Seq[AssetScriptJson],
    sponsorships: Seq[SponsorshipJson],
    newLeases: Seq[NewLeaseJson],
    cancelledLeases: Seq[CancelledLeaseJson],
    aliases: Seq[AliasJson],
    orderFills: Seq[OrderFillJson],
    accountScripts: Seq[AccountScriptJson],
    accountData: Seq[AccountDataJson],
    nextCommittedGenerators: Seq[NextCommittedGenerator]
)

object StateSnapshotJson {
  def fromSnapshot(s: StateSnapshot, txStatus: TxMeta.Status): StateSnapshotJson =
    StateSnapshotJson(
      TransactionJsonSerializer.applicationStatusFromTxStatus(txStatus),
      s.balances.map { case ((address, asset), balance) => BalanceJson(address, asset, balance) }.toSeq,
      s.leaseBalances.map { case (address, lease) => LeaseBalanceJson(address, lease.in, lease.out) }.toSeq,
      s.assetStatics.map(_._2._1).toSeq,
      s.assetVolumes.map { case (id, info) => AssetVolumeJson(id, info.isReissuable, info.volume) }.toSeq,
      s.assetNamesAndDescriptions.map { case (id, info) =>
        AssetInfoJson(id, info.name.toStringUtf8, info.description.toStringUtf8, info.lastUpdatedAt)
      }.toSeq,
      s.assetScripts.map { case (id, info) => AssetScriptJson(id, info.script, info.complexity) }.toSeq,
      s.sponsorships.map { case (id, value) => SponsorshipJson(id, value.minFee) }.toSeq,
      s.newLeases.map { case (id, info) =>
        NewLeaseJson(id, info.sender, info.recipientAddress, info.amount.value, info.sourceId, info.height)
      }.toSeq,
      s.cancelledLeases.map { case (id, status) => CancelledLeaseJson(id, status.cancelTransactionId.get, status.cancelHeight.get) }.toSeq,
      s.aliases.map { case (alias, address) => AliasJson(address, alias.name) }.toSeq,
      s.orderFills.map { case (id, info) => OrderFillJson(id, info.volume, info.fee) }.toSeq,
      s.accountScripts.map { case (pk, info) =>
        info.fold(AccountScriptJson(pk, None, 0))(i => AccountScriptJson(i.publicKey, Some(i.script), i.verifierComplexity))
      }.toSeq,
      s.accountData.map { case (address, data) => AccountDataJson(address, data.values.toSeq) }.toSeq,
      s.nextCommittedGenerators.map { case (publicKey, blsPk) => NextCommittedGenerator(publicKey, blsPk) }
    )
  given Writes[ByteStr]            = com.wavesplatform.utils.byteStrFormat
  given OWrites[StateSnapshotJson] = Json.writes

  case class BalanceJson(address: Address, asset: Asset, balance: Long)
  given OWrites[BalanceJson] = Json.writes

  case class LeaseBalanceJson(address: Address, in: Long, out: Long)
  given OWrites[LeaseBalanceJson] = Json.writes

  case class AssetVolumeJson(id: IssuedAsset, isReissuable: Boolean, volume: BigInt)
  given OWrites[AssetVolumeJson] = Json.writes

  case class AssetInfoJson(id: IssuedAsset, name: String, description: String, lastUpdatedAt: Height)
  given OWrites[AssetInfoJson] = Json.writes

  case class AssetScriptJson(id: IssuedAsset, script: Script, complexity: Long)
  given Writes[Script]           = Writes[Script](s => JsString(s.bytes().base64))
  given OWrites[AssetScriptJson] = Json.writes[AssetScriptJson]

  case class AccountScriptJson(publicKey: PublicKey, script: Option[Script], verifierComplexity: Long)
  given Aux[MacroOptions]          = JsonConfiguration(optionHandlers = WritesNull)
  given OWrites[AccountScriptJson] = Json.writes

  case class SponsorshipJson(id: IssuedAsset, minSponsoredAssetFee: Long)
  given OWrites[SponsorshipJson] = Json.writes

  case class NewLeaseJson(id: ByteStr, sender: PublicKey, recipient: Address, amount: Long, txId: TransactionId, height: Height)
  given OWrites[NewLeaseJson] = Json.writes

  case class CancelledLeaseJson(id: ByteStr, txId: TransactionId, height: Height)
  given OWrites[CancelledLeaseJson] = Json.writes

  case class AccountDataJson(address: Address, data: Seq[DataEntry[?]])
  given OWrites[AccountDataJson] = Json.writes

  case class AliasJson(address: Address, alias: String)
  given OWrites[AliasJson] = Json.writes

  case class OrderFillJson(id: ByteStr, volume: Long, fee: Long)
  given OWrites[OrderFillJson] = Json.writes

  case class NextCommittedGenerator(publicKey: PublicKey, blsPublicKey: BlsPublicKey)
  given Writes[BlsPublicKey]            = summon[Writes[ByteStr]].contramap(_.byteStr)
  given OWrites[NextCommittedGenerator] = Json.writes
}
