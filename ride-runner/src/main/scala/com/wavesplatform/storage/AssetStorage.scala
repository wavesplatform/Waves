package com.wavesplatform.storage

import com.google.protobuf.UnsafeByteOperations
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.PBTransactions.toVanillaScript
import com.wavesplatform.state.{AssetDescription, AssetScriptInfo, Height}
import com.wavesplatform.storage.actions.{AppendResult, RollbackResult}
import com.wavesplatform.storage.persistent.PersistentCache
import com.wavesplatform.transaction.Asset.IssuedAsset

import java.nio.charset.StandardCharsets

class AssetStorage[TagT](
    blockchainApi: BlockchainGrpcApi,
    override val persistentCache: PersistentCache[IssuedAsset, AssetDescription]
) extends HeightStorage[IssuedAsset, AssetDescription, TagT]
    with HasAnyRefMap[IssuedAsset, AssetDescription, TagT] {
  override def getFromBlockchain(key: IssuedAsset): Option[AssetDescription] = blockchainApi.getAssetDescription(key)

  def append(height: Int, update: StateUpdate.AssetStateUpdate): AppendResult[TagT] = {
    val asset = getAsset(update)
    append(height, asset, update.after.map(AssetStorage.toAssetDescription(asset, _)))
  }

  // TODO looks similar to append
  def rollback(rollbackHeight: Int, update: StateUpdate.AssetStateUpdate): RollbackResult[TagT] = {
    val asset = getAsset(update)
    rollback(rollbackHeight, asset, update.after.map(AssetStorage.toAssetDescription(asset, _)))
  }

  def getAsset(update: StateUpdate.AssetStateUpdate): IssuedAsset =
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
