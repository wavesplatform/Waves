package com.wavesplatform.events

import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.{DataEntry, LeaseBalance}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset

sealed trait AssetStateUpdate extends Product with Serializable {
  def assetId: IssuedAsset
}
final case class Issue(
    assetId: IssuedAsset,
    name: Either[ByteStr, String],
    description: Either[ByteStr, String],
    decimals: Int,
    reissuable: Boolean,
    volume: Long,
    script: Option[Script],
    nft: Boolean
) extends AssetStateUpdate
final case class UpdateAssetVolume(
    assetId: IssuedAsset,
    volume: BigInt
) extends AssetStateUpdate
final case class ForbidReissue(assetId: IssuedAsset) extends AssetStateUpdate
final case class SetAssetScript(
    assetId: IssuedAsset,
    script: Option[Script]
) extends AssetStateUpdate
final case class SetSponsorship(
    assetId: IssuedAsset,
    sponsorship: Long
) extends AssetStateUpdate
final case class UpdateAssetInfo(
    assetId: IssuedAsset,
    name: String,
    description: String
) extends AssetStateUpdate

final case class StateUpdate(
    balances: Seq[(Address, Asset, Long)],
    leases: Seq[(Address, LeaseBalance)],
    dataEntries: Seq[(Address, DataEntry[_])],
    assets: Seq[AssetStateUpdate]
) {
  def isEmpty: Boolean = balances.isEmpty && leases.isEmpty && dataEntries.isEmpty && assets.isEmpty
}

sealed trait BlockchainUpdated extends Product with Serializable
final case class BlockAppended(
    block: Block,
    height: Int,
    blockStateUpdate: StateUpdate,
    transactionStateUpdates: Seq[StateUpdate]
) extends BlockchainUpdated
final case class MicroBlockAppended(
    microBlock: MicroBlock,
    height: Int,
    microBlockStateUpdate: StateUpdate,
    transactionStateUpdates: Seq[StateUpdate]
) extends BlockchainUpdated
final case class RollbackCompleted(to: ByteStr, height: Int) extends BlockchainUpdated
final case class MicroBlockRollbackCompleted(to: ByteStr)    extends BlockchainUpdated
