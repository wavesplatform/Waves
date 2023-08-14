package com.wavesplatform.ride.runner.caches

import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.PBAmounts.toAssetAndAmount
import com.wavesplatform.protobuf.transaction.PBTransactions.{toVanillaDataEntry, toVanillaScript}
import com.wavesplatform.protobuf.transaction.{CreateAliasTransactionData, DataTransactionData, Transaction}
import com.wavesplatform.state.{AssetDescription, AssetScriptInfo, DataEntry, Height, LeaseBalance, TransactionId}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.{account, state, transaction}

import java.nio.charset.StandardCharsets

// TODO Use KvPair instead?
sealed trait CacheKey extends Product with Serializable {
  type ValueT
  def keyWeight: Int
  def valueWeight(value: ValueT): Int
}

object CacheKey {
  case class AccountData(address: Address, dataKey: String) extends CacheKey {
    override type ValueT = DataEntry[?]
    // 24 = 12 (header) + 4 (ref) + 4 (ref) + 4 (align)
    override def keyWeight: Int                        = 24 + CacheWeights.OfAddress + CacheWeights.ofAsciiString(dataKey)
    override def valueWeight(value: DataEntry[?]): Int = CacheWeights.ofDataEntry(value)
  }

  case class Transaction(id: TransactionId) extends CacheKey {
    override type ValueT = state.Height
    override def keyWeight: Int                  = 16 + CacheWeights.OfTransactionId // 16 = 12 (header) + 4 (ref) + id
    override def valueWeight(value: Height): Int = 16                                // 12 (header) + 4 (int value)
  }

  case object Height extends CacheKey {
    override type ValueT = state.Height
    override def keyWeight: Int                  = 0
    override def valueWeight(value: Height): Int = 16 // 12 (header) + 4 (int value)
  }

  case class Alias(alias: account.Alias) extends CacheKey {
    override type ValueT = Address
    override def keyWeight: Int                   = 16 + CacheWeights.ofAlias(alias) // 16 = 12 (header) + 4 (ref) + alias
    override def valueWeight(value: Address): Int = CacheWeights.OfAddress
  }

  case class Asset(asset: IssuedAsset) extends CacheKey {
    override type ValueT = WeighedAssetDescription
    override def keyWeight: Int                                   = CacheWeights.OfIssuedAsset
    override def valueWeight(value: WeighedAssetDescription): Int = CacheWeights.ofWeighedAssetDescription(value)
  }

  case class AccountBalance(address: Address, asset: transaction.Asset) extends CacheKey {
    override type ValueT = Long
    // 24 = 12 (header) + 4*2 (ref: address, asset) + 4 (align)
    override def keyWeight: Int                = 24 + CacheWeights.OfAddress + CacheWeights.ofAsset(asset)
    override def valueWeight(value: Long): Int = 8
  }

  case class AccountLeaseBalance(address: Address) extends CacheKey {
    override type ValueT = LeaseBalance
    // 16 = 12 (header) + 4 (ref)
    override def keyWeight: Int                        = 16 + CacheWeights.OfAddress
    override def valueWeight(value: LeaseBalance): Int = CacheWeights.OfLeaseBalance
  }

  case class AccountScript(address: Address) extends CacheKey {
    override type ValueT = WeighedAccountScriptInfo
    // 16 = 12 (header) + 4 (ref)
    override def keyWeight: Int = 16 + CacheWeights.OfAddress
    // 24 = 12 (header) + 4 (size) + 4 (ref: scriptInfo) + 4 (align)
    override def valueWeight(value: WeighedAccountScriptInfo): Int = 24 + value.scriptInfoWeight
  }
}

class GrpcCacheKeyConverters(chainId: Byte) {
  // TODO CacheKey instead of CacheKey.AccountData
  def accountDataKey(update: StateUpdate.DataEntryUpdate): CacheKey.AccountData =
    CacheKey.AccountData(toVanillaAddress(update.address, chainId), accountDataRawKey(update))

  def accountDataRawKey(update: StateUpdate.DataEntryUpdate): String =
    update.dataEntry.orElse(update.dataEntryBefore).map(_.key).getOrElse(throw new RuntimeException(s"Can't get data key of $update"))

  def accountDataValueBefore(update: StateUpdate.DataEntryUpdate): Option[CacheKey.AccountData#ValueT] = update.dataEntryBefore.map(accountDataValue)
  def accountDataValueAfter(update: StateUpdate.DataEntryUpdate): Option[CacheKey.AccountData#ValueT]  = update.dataEntry.map(accountDataValue)
  def accountDataValue(dataEntry: DataTransactionData.DataEntry): CacheKey.AccountData#ValueT          = toVanillaDataEntry(dataEntry)

  def transactionIdKey(id: ByteString): CacheKey.Transaction = CacheKey.Transaction(TransactionId(ByteStr(id.toByteArray)))

  // Can't fail, because we receive it verified
  def aliasKey(txData: CreateAliasTransactionData): CacheKey.Alias = aliasKey(txData.alias)
  def aliasKey(alias: String): CacheKey.Alias                      = CacheKey.Alias(Alias.createWithChainId(alias, chainId).explicitGet())
  def aliasValue(tx: Transaction): Address                         = tx.senderPublicKey.toPublicKey.toAddress(chainId)

  def assetKey(update: StateUpdate.AssetStateUpdate): CacheKey.Asset =
    CacheKey.Asset(
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
    name = UnsafeByteOperations.unsafeWrap(update.name.getBytes(StandardCharsets.UTF_8)),
    description = UnsafeByteOperations.unsafeWrap(update.description.getBytes(StandardCharsets.UTF_8)),
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

  def accountBalanceKeyAndValueBefore(update: StateUpdate.BalanceUpdate): (CacheKey.AccountBalance, Long) = {
    val address    = toVanillaAddress(update.address, chainId)
    val (asset, _) = toAssetAndAmount(update.getAmountAfter) // We have an asset only in getAmountAfter
    (CacheKey.AccountBalance(address, asset), update.amountBefore)
  }

  def accountBalanceKeyAndValueAfter(update: StateUpdate.BalanceUpdate): (CacheKey.AccountBalance, Long) = {
    val address        = toVanillaAddress(update.address, chainId)
    val (asset, after) = toAssetAndAmount(update.getAmountAfter)
    (CacheKey.AccountBalance(address, asset), after)
  }

  def accountLeaseBalanceKeyAndValueBefore(update: StateUpdate.LeasingUpdate): (CacheKey.AccountLeaseBalance, LeaseBalance) = {
    val address = toVanillaAddress(update.address, chainId)
    (CacheKey.AccountLeaseBalance(address), toVanillaBefore(update))
  }

  def accountLeaseBalanceKeyAndValueAfter(update: StateUpdate.LeasingUpdate): (CacheKey.AccountLeaseBalance, LeaseBalance) = {
    val address = toVanillaAddress(update.address, chainId)
    (CacheKey.AccountLeaseBalance(address), toVanillaAfter(update))
  }

  private def toVanillaBefore(x: StateUpdate.LeasingUpdate): LeaseBalance = LeaseBalance(x.inBefore, x.outBefore)
  private def toVanillaAfter(x: StateUpdate.LeasingUpdate): LeaseBalance  = LeaseBalance(x.inAfter, x.outAfter)

  def accountScriptKey(update: StateUpdate.ScriptUpdate): CacheKey.AccountScript = {
    val address = toVanillaAddress(update.address, chainId)
    CacheKey.AccountScript(address)
  }
}
