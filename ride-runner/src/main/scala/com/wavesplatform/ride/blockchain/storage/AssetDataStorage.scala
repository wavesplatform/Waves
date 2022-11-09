package com.wavesplatform.ride.blockchain.storage

import com.google.protobuf.UnsafeByteOperations
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.PBTransactions.toVanillaScript
import com.wavesplatform.ride.blockchain.DataKey
import com.wavesplatform.ride.blockchain.DataKey.AssetDescriptionDataKey
import com.wavesplatform.ride.blockchain.caches.PersistentCache
import com.wavesplatform.state.{AssetDescription, AssetScriptInfo, Height}
import com.wavesplatform.transaction.Asset.IssuedAsset

import java.nio.charset.StandardCharsets

class AssetDataStorage[TagT](
    blockchainApi: BlockchainGrpcApi,
    override val persistentCache: PersistentCache[IssuedAsset, AssetDescription]
) extends DataStorage[IssuedAsset, AssetDescription, TagT] {
  override def mkDataKey(key: IssuedAsset): DataKey = AssetDescriptionDataKey(key)

  override def getFromBlockchain(key: IssuedAsset): Option[AssetDescription] = blockchainApi.getAssetDescription(key)

  def append(height: Int, update: StateUpdate.AssetStateUpdate): AppendResult[TagT] = {
    val asset = getAsset(update)
    append(height, asset, update.after.map(AssetDataStorage.toAssetDescription(asset, _)))
  }

  // TODO looks similar to append
  def rollback(rollbackHeight: Int, update: StateUpdate.AssetStateUpdate): RollbackResult[TagT] = {
    val asset = getAsset(update)
    rollback(rollbackHeight, asset, update.after.map(AssetDataStorage.toAssetDescription(asset, _)))
  }

  def getAsset(update: StateUpdate.AssetStateUpdate): IssuedAsset =
    update.before
      .orElse(update.after)
      .getOrElse(throw new RuntimeException(s"Can't get asset id from update: $update"))
      .assetId
      .toIssuedAsset
}

object AssetDataStorage {
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
