package com.wavesplatform.events

import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.{DataEntry, LeaseBalance}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset

sealed trait AssetStateUpdate extends Product with Serializable {
  def asset: IssuedAsset
}
final case class Issue(
    asset: IssuedAsset,
    name: Either[ByteStr, String],
    description: Either[ByteStr, String],
    decimals: Int,
    reissuable: Boolean,
    volume: Long,
    script: Option[Script],
    nft: Boolean
) extends AssetStateUpdate
final case class UpdateAssetVolume(
    asset: IssuedAsset,
    volume: BigInt
) extends AssetStateUpdate
final case class ForbidReissue(asset: IssuedAsset) extends AssetStateUpdate
final case class SetAssetScript(
    asset: IssuedAsset,
    script: Option[Script]
) extends AssetStateUpdate
final case class SetSponsorship(
    asset: IssuedAsset,
    sponsorship: Long
) extends AssetStateUpdate
final case class UpdateAssetInfo(
    asset: IssuedAsset,
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

sealed trait BlockchainUpdated extends Product with Serializable {
  def toId: ByteStr
  def toHeight: Int
}
final case class BlockAppended(
    toId: ByteStr,
    toHeight: Int,
    block: Block,
    blockStateUpdate: StateUpdate,
    transactionStateUpdates: Seq[StateUpdate]
) extends BlockchainUpdated
final case class MicroBlockAppended(
    toId: ByteStr,
    toHeight: Int,
    microBlock: MicroBlock,
    microBlockStateUpdate: StateUpdate,
    transactionStateUpdates: Seq[StateUpdate]
) extends BlockchainUpdated
final case class RollbackCompleted(toId: ByteStr, toHeight: Int)           extends BlockchainUpdated
final case class MicroBlockRollbackCompleted(toId: ByteStr, toHeight: Int) extends BlockchainUpdated
