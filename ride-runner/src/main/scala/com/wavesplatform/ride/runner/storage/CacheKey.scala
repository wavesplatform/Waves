package com.wavesplatform.ride.runner.storage

import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.DataTransactionData
import com.wavesplatform.protobuf.transaction.PBAmounts.toAssetAndAmount
import com.wavesplatform.protobuf.transaction.PBTransactions.{toVanillaDataEntry, toVanillaScript}
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, AssetScriptInfo, DataEntry, Height, LeaseBalance, TransactionId}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.{account, state, transaction}

import java.nio.charset.StandardCharsets

// TODO Use KvPair instead?
sealed trait CacheKey extends Product with Serializable {
  type ValueT
}

object CacheKey {
  case class AccountData(address: Address, dataKey: String) extends CacheKey {
    override type ValueT = DataEntry[?]
  }

  case class Transaction(id: TransactionId) extends CacheKey {
    override type ValueT = state.Height
  }

  case object Height extends CacheKey {
    override type ValueT = state.Height
  }

  case class Alias(alias: account.Alias) extends CacheKey {
    override type ValueT = Address
  }

  case class Asset(asset: IssuedAsset) extends CacheKey {
    override type ValueT = AssetDescription
  }

  case class AccountBalance(address: Address, asset: transaction.Asset) extends CacheKey {
    override type ValueT = Long
  }

  case class AccountLeaseBalance(address: Address) extends CacheKey {
    override type ValueT = LeaseBalance
  }

  case class AccountScript(address: Address) extends CacheKey {
    override type ValueT = AccountScriptInfo
  }
}

class GrpcCacheKeyConverters(chainId: Byte) {
  // TODO CacheKey instead of CacheKey.AccountData
  def accountDataKey(update: StateUpdate.DataEntryUpdate): CacheKey.AccountData =
    CacheKey.AccountData(toVanillaAddress(update.address, chainId), accountDataRawKey(update))

  def accountDataRawKey(update: StateUpdate.DataEntryUpdate): String =
    update.dataEntry.orElse(update.dataEntryBefore).map(_.key).getOrElse(throw new RuntimeException(s"Can't get data key of $update"))

  def accountDataValue(update: StateUpdate.DataEntryUpdate): Option[CacheKey.AccountData#ValueT] = update.dataEntry.map(accountDataValue)
  def accountDataValue(dataEntry: DataTransactionData.DataEntry): CacheKey.AccountData#ValueT    = toVanillaDataEntry(dataEntry)

  def transactionIdKey(id: ByteString): CacheKey.Transaction =
    CacheKey.Transaction(TransactionId(ByteStr(id.toByteArray)))

  // Can't fail, because receive verified
  def aliasKey(name: String): CacheKey.Alias   = CacheKey.Alias(Alias.createWithChainId(name, chainId).explicitGet())
  def aliasValue(pkBytes: ByteString): Address = pkBytes.toPublicKey.toAddress(chainId)

  def assetKey(update: StateUpdate.AssetStateUpdate): CacheKey.Asset =
    CacheKey.Asset(
      update.before
        .orElse(update.after)
        .getOrElse(throw new RuntimeException(s"Can't get asset id from update: $update"))
        .assetId
        .toIssuedAsset
    )

  def assetValue(asset: IssuedAsset, update: StateUpdate.AssetStateUpdate): Option[AssetDescription] =
    update.after.map(assetValue(asset, _))

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

  def accountBalanceKeyAndValue(update: StateUpdate.BalanceUpdate): (CacheKey.AccountBalance, Long) = {
    val address        = toVanillaAddress(update.address, chainId)
    val (asset, after) = toAssetAndAmount(update.getAmountAfter)
    (CacheKey.AccountBalance(address, asset), after)
  }

  def accountLeaseBalanceKeyAndValue(update: StateUpdate.LeasingUpdate): (CacheKey.AccountLeaseBalance, LeaseBalance) = {
    val address = toVanillaAddress(update.address, chainId)
    (CacheKey.AccountLeaseBalance(address), toVanilla(update))
  }

  private def toVanilla(x: StateUpdate.LeasingUpdate): LeaseBalance = LeaseBalance(x.inAfter, x.outAfter)

  def accountScriptKey(update: StateUpdate.ScriptUpdate): CacheKey.AccountScript = {
    val address = toVanillaAddress(update.address, chainId)
    CacheKey.AccountScript(address)
  }
}
