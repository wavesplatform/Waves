package com.wavesplatform.ride.runner.storage

import com.google.protobuf.UnsafeByteOperations
import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.PBTransactions.toVanillaScript
import com.wavesplatform.ride.runner.storage.StorageContext.ReadWrite
import com.wavesplatform.ride.runner.storage.persistent.PersistentCache
import com.wavesplatform.state.{AssetDescription, AssetScriptInfo, Height}
import com.wavesplatform.transaction.Asset.IssuedAsset

import java.nio.charset.StandardCharsets

class AssetStorage[TagT](
    override val settings: ExactWithHeightStorage.Settings,
    blockchainApi: BlockchainApi,
    override val persistentCache: PersistentCache[IssuedAsset, AssetDescription]
) extends ExactWithHeightStorage[IssuedAsset, AssetDescription, TagT] {
  override def getFromBlockchain(key: IssuedAsset): Option[AssetDescription] = blockchainApi.getAssetDescription(key)

  def append(atHeight: Height, update: StateUpdate.AssetStateUpdate)(implicit ctx: ReadWrite): AffectedTags[TagT] = {
    val asset = getAsset(update)
    append(atHeight, asset, update.after.map(AssetStorage.toAssetDescription(asset, _)))
  }

  def undoAppend(toHeight: Height, update: StateUpdate.AssetStateUpdate)(implicit ctx: ReadWrite): AffectedTags[TagT] =
    undoAppend(toHeight, getAsset(update))

  def rollback(toHeight: Height, update: StateUpdate.AssetStateUpdate)(implicit ctx: ReadWrite): AffectedTags[TagT] = {
    val asset = getAsset(update)
    rollback(toHeight, asset, update.after.map(AssetStorage.toAssetDescription(asset, _)))
  }

  private def getAsset(update: StateUpdate.AssetStateUpdate): IssuedAsset =
    update.before
      .orElse(update.after)
      .getOrElse(throw new RuntimeException(s"Can't get asset id from update: $update"))
      .assetId
      .toIssuedAsset
}

object AssetStorage {
  def toAssetDescription(asset: IssuedAsset, update: StateUpdate.AssetDetails): AssetDescription = AssetDescription(
    originTransactionId = asset.id,
    issuer = update.issuer.toPublicKey,
    name = UnsafeByteOperations.unsafeWrap(update.name.getBytes(StandardCharsets.UTF_8)),
    description = UnsafeByteOperations.unsafeWrap(update.description.getBytes(StandardCharsets.UTF_8)),
    decimals = update.decimals,
    reissuable = update.reissuable,
    totalVolume = update.volume,
    lastUpdatedAt = Height(update.lastUpdated),
    script = for {
      pbScript <- update.scriptInfo
      script   <- toVanillaScript(pbScript.script)
    } yield AssetScriptInfo(script, pbScript.complexity),
    sponsorship = update.sponsorship,
    nft = update.nft
  )
}
