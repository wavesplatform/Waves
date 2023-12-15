package com.wavesplatform.api.http
import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.*
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import play.api.libs.json.{Json, OWrites}

import scala.collection.immutable.VectorMap

case class StateSnapshotJson(
    balances: VectorMap[(Address, Asset), Long],
    leaseBalances: Map[Address, LeaseBalance],
    assetStatics: VectorMap[IssuedAsset, AssetStaticInfo],
    assetVolumes: Map[IssuedAsset, AssetVolumeInfo],
    assetNamesAndDescriptions: Map[IssuedAsset, AssetInfo],
    assetScripts: Map[IssuedAsset, AssetScriptInfo],
    sponsorships: Map[IssuedAsset, SponsorshipValue],
    newLeases: Map[ByteStr, LeaseStaticInfo],
    cancelledLeases: Map[ByteStr, LeaseDetails.Status.Inactive],
    aliases: Map[Alias, Address],
    orderFills: Map[ByteStr, VolumeAndFee],
    accountScripts: Map[PublicKey, Option[AccountScriptInfo]] = Map(),
    accountData: Map[Address, Map[String, DataEntry[?]]] = Map()
)

object StateSnapshotJson {
  def fromSnapshot(s: StateSnapshot): StateSnapshotJson =
    StateSnapshotJson(
      s.balances,
      s.leaseBalances,
      s.assetStatics,
      s.assetVolumes,
      s.assetNamesAndDescriptions,
      s.assetScripts,
      s.sponsorships,
      s.newLeases,
      s.cancelledLeases,
      s.aliases,
      s.orderFills,
      s.accountScripts,
      s.accountData
    )
  import LeaseStaticInfo.*
  implicit val writes: OWrites[StateSnapshotJson] = Json.writes[StateSnapshotJson]
}
