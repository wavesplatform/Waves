package com.wavesplatform.api.http
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.http.StateSnapshotJson.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.*
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import play.api.libs.json.*
import play.api.libs.json.Json.MacroOptions
import play.api.libs.json.JsonConfiguration.Aux
import play.api.libs.json.OptionHandlers.WritesNull

case class StateSnapshotJson(
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
    accountData: Seq[AccountDataJson]
)

object StateSnapshotJson {
  def fromSnapshot(s: StateSnapshot): StateSnapshotJson =
    StateSnapshotJson(
      s.balances.map { case ((address, asset), balance) => BalanceJson(address, asset, balance) }.toSeq,
      s.leaseBalances.map { case (address, lease) => LeaseBalanceJson(address, lease.in, lease.out) }.toSeq,
      s.assetStatics.values.toSeq,
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
      s.accountData.map { case (address, data) => AccountDataJson(address, data.values.toSeq) }.toSeq
    )
  implicit val byteStrWrites: Writes[ByteStr]     = com.wavesplatform.utils.byteStrFormat
  implicit val writes: OWrites[StateSnapshotJson] = Json.writes

  case class BalanceJson(address: Address, asset: Asset, balance: Long)
  object BalanceJson {
    implicit val writes: OWrites[BalanceJson] = Json.writes
  }

  case class LeaseBalanceJson(address: Address, in: Long, out: Long)
  object LeaseBalanceJson {
    implicit val writes: OWrites[LeaseBalanceJson] = Json.writes
  }

  case class AssetVolumeJson(id: IssuedAsset, isReissuable: Boolean, volume: BigInt)
  object AssetVolumeJson {
    implicit val writes: OWrites[AssetVolumeJson] = Json.writes
  }

  case class AssetInfoJson(id: IssuedAsset, name: String, description: String, lastUpdatedAt: Height)
  object AssetInfoJson {
    implicit val writes: OWrites[AssetInfoJson] = Json.writes
  }

  case class AssetScriptJson(id: IssuedAsset, script: Script, complexity: Long)
  object AssetScriptJson {
    implicit val scriptWrites: Writes[Script]     = Writes[Script](s => JsString(s.bytes().base64))
    implicit val writes: OWrites[AssetScriptJson] = Json.writes[AssetScriptJson]
  }

  case class AccountScriptJson(publicKey: PublicKey, script: Option[Script], verifierComplexity: Long)
  object AccountScriptJson {
    implicit val config: Aux[MacroOptions]          = JsonConfiguration(optionHandlers = WritesNull)
    implicit val scriptWrites: Writes[Script]       = Writes[Script](s => JsString(s.bytes().base64))
    implicit val writes: OWrites[AccountScriptJson] = Json.writes
  }

  case class SponsorshipJson(id: IssuedAsset, minSponsoredAssetFee: Long)
  object SponsorshipJson {
    implicit val writes: OWrites[SponsorshipJson] = Json.writes
  }

  case class NewLeaseJson(id: ByteStr, sender: PublicKey, recipient: Address, amount: Long, txId: ByteStr, height: Int)
  object NewLeaseJson {
    implicit val writes: OWrites[NewLeaseJson] = Json.writes
  }

  case class CancelledLeaseJson(id: ByteStr, txId: ByteStr, height: Int)
  object CancelledLeaseJson {
    implicit val writes: OWrites[CancelledLeaseJson] = Json.writes
  }

  case class AccountDataJson(address: Address, data: Seq[DataEntry[?]])
  object AccountDataJson {
    implicit val writes: OWrites[AccountDataJson] = Json.writes
  }

  case class AliasJson(address: Address, alias: String)
  object AliasJson {
    implicit val writes: OWrites[AliasJson] = Json.writes
  }

  case class OrderFillJson(id: ByteStr, volume: Long, fee: Long)
  object OrderFillJson {
    implicit val writes: OWrites[OrderFillJson] = Json.writes
  }
}
