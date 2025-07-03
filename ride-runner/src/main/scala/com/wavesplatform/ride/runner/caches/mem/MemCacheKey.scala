package com.wavesplatform.ride.runner.caches.mem

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.explicitGet
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.protobuf.transaction.PBAmounts.toAssetAndAmount
import com.wavesplatform.protobuf.transaction.PBTransactions.{toVanillaDataEntry, toVanillaScript}
import com.wavesplatform.protobuf.transaction.{CreateAliasTransactionData, Transaction}
import com.wavesplatform.protobuf.{ByteStringExt, transaction as pb}
import com.wavesplatform.ride.runner.caches.{WeighedAccountScriptInfo, WeighedAssetDescription}
import com.wavesplatform.state.{AssetDescription, AssetScriptInfo, DataEntry, Height, LeaseBalance, TransactionId}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.utils.StringBytes
import com.wavesplatform.{account, state, transaction}

sealed trait MemCacheKey[ValueT]

object MemCacheKey {
  case class AccountData(address: Address, dataKey: String) extends MemCacheKey[DataEntry[?]]

  case class Transaction(id: TransactionId) extends MemCacheKey[state.Height]

  case object Height extends MemCacheKey[state.Height]

  case class Alias(alias: account.Alias) extends MemCacheKey[Address]

  case class Asset(asset: IssuedAsset) extends MemCacheKey[WeighedAssetDescription]

  case class AccountBalance(address: Address, asset: transaction.Asset) extends MemCacheKey[Long]

  case class AccountLeaseBalance(address: Address) extends MemCacheKey[LeaseBalance]

  case class AccountScript(address: Address) extends MemCacheKey[WeighedAccountScriptInfo]
}

class GrpcCacheKeyConverters(chainId: Byte) {
  def accountDataKey(update: StateUpdate.DataEntryUpdate): MemCacheKey.AccountData = {
    val dataKey = update.dataEntry.orElse(update.dataEntryBefore).map(_.key).getOrElse(throw new RuntimeException(s"Can't get data key of $update"))
    MemCacheKey.AccountData(update.address.toAddress(chainId), dataKey)
  }

  def accountDataValueBefore(update: StateUpdate.DataEntryUpdate): Option[DataEntry[?]] =
    update.dataEntryBefore.map(accountDataValue)
  def accountDataValueAfter(update: StateUpdate.DataEntryUpdate): Option[DataEntry[?]] = update.dataEntry.map(accountDataValue)
  def accountDataValue(dataEntry: pb.DataEntry): DataEntry[?]                          = toVanillaDataEntry(dataEntry)

  def transactionIdKey(id: ByteString): MemCacheKey.Transaction = MemCacheKey.Transaction(TransactionId(ByteStr(id.toByteArray)))

  // Can't fail, because we receive it verified
  def aliasKey(txData: CreateAliasTransactionData): MemCacheKey.Alias = aliasKey(txData.alias)
  def aliasKey(alias: String): MemCacheKey.Alias                      = MemCacheKey.Alias(Alias.createWithChainId(alias, chainId).explicitGet())
  def aliasValue(tx: Transaction): Address                            = tx.senderPublicKey.toPublicKey.toAddress(chainId)

  def assetKey(update: StateUpdate.AssetStateUpdate): MemCacheKey.Asset =
    MemCacheKey.Asset(
      update.before
        .orElse(update.after)
        .getOrElse(throw new RuntimeException(s"Can't get asset id from update: $update"))
        .assetId
        .toIssuedAsset
    )

  def assetValueBefore(asset: IssuedAsset, update: StateUpdate.AssetStateUpdate): Option[AssetDescription] = update.before.map(assetValue(asset, _))
  def assetValueAfter(asset: IssuedAsset, update: StateUpdate.AssetStateUpdate): Option[AssetDescription]  = update.after.map(assetValue(asset, _))
  def assetValue(asset: IssuedAsset, update: StateUpdate.AssetDetails): AssetDescription = AssetDescription(
    originTransactionId = asset.id,
    issuer = update.issuer.toPublicKey,
    name = update.name.toByteString,
    description = update.description.toByteString,
    decimals = update.decimals,
    reissuable = update.reissuable,
    totalVolume = update.volume,
    script = for {
      pbScript <- update.scriptInfo
      script   <- toVanillaScript(pbScript.script)
    } yield AssetScriptInfo(script, pbScript.complexity),
    sponsorship = update.sponsorship,
    // All next fields are not used, see: https://docs.waves.tech/en/ride/structures/common-structures/asset#fields
    lastUpdatedAt = Height(0),
    nft = false,
    sequenceInBlock = 0,
    issueHeight = Height(1)
  )

  def accountBalanceKeyAndValueBefore(update: StateUpdate.BalanceUpdate): (MemCacheKey.AccountBalance, Long) = {
    val address    = update.address.toAddress(chainId)
    val (asset, _) = toAssetAndAmount(update.getAmountAfter) // We have an asset only in getAmountAfter
    (MemCacheKey.AccountBalance(address, asset), update.amountBefore)
  }

  def accountBalanceKeyAndValueAfter(update: StateUpdate.BalanceUpdate): (MemCacheKey.AccountBalance, Long) = {
    val address        = update.address.toAddress(chainId)
    val (asset, after) = toAssetAndAmount(update.getAmountAfter)
    (MemCacheKey.AccountBalance(address, asset), after)
  }

  def accountLeaseBalanceKeyAndValueBefore(update: StateUpdate.LeasingUpdate): (MemCacheKey.AccountLeaseBalance, LeaseBalance) = {
    val address = update.address.toAddress(chainId)
    (MemCacheKey.AccountLeaseBalance(address), toVanillaBefore(update))
  }

  def accountLeaseBalanceKeyAndValueAfter(update: StateUpdate.LeasingUpdate): (MemCacheKey.AccountLeaseBalance, LeaseBalance) = {
    val address = update.address.toAddress(chainId)
    (MemCacheKey.AccountLeaseBalance(address), toVanillaAfter(update))
  }

  private def toVanillaBefore(x: StateUpdate.LeasingUpdate): LeaseBalance = LeaseBalance(x.inBefore, x.outBefore)
  private def toVanillaAfter(x: StateUpdate.LeasingUpdate): LeaseBalance  = LeaseBalance(x.inAfter, x.outAfter)

  def accountScriptKey(update: StateUpdate.ScriptUpdate): MemCacheKey.AccountScript = {
    val address = update.address.toAddress(chainId)
    MemCacheKey.AccountScript(address)
  }
}
