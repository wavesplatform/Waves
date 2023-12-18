package com.wavesplatform.ride.runner.environments

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.ride.runner.caches.CacheKeyTags
import com.wavesplatform.ride.runner.caches.mem.MemCacheKey
import com.wavesplatform.state.TransactionId
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.ScorexLogging

class DefaultDAppEnvironmentTracker[TagT](allTags: CacheKeyTags[TagT], tag: TagT) extends DAppEnvironmentTracker with ScorexLogging {
  override def height(): Unit = {
    // log.trace(s"[$tag] height")
    allTags.addDependent(MemCacheKey.Height, tag)
  }

  override def lastBlockOpt(): Unit = {
    // log.trace(s"[$tag] lastBlockOpt")
    height()
  }

  // We don't support this, see SupportedBlockchain.transactionInfo
  override def transactionById(id: Array[Byte]): Unit = {
    // val txId = mkTxCacheKey(id)
    // log.trace(s"[$tag] transactionById($txId)")
    // allTags.addDependent(txId, tag)
  }

  // We don't support this, see SupportedBlockchain.transferById
  override def transferTransactionById(id: Array[Byte]): Unit = {
    // val txId = mkTxCacheKey(id)
    // log.trace(s"[$tag] transferTransactionById($txId)")
    // allTags.addDependent(txId, tag)
  }

  override def transactionHeightById(id: Array[Byte]): Unit = {
    val txId = mkTxCacheKey(id)
    // log.trace(s"[$tag] transactionHeightById($txId)")
    allTags.addDependent(txId, tag)
  }

  private def mkTxCacheKey(txId: Array[Byte]) = MemCacheKey.Transaction(TransactionId(ByteStr(txId)))

  override def assetInfoById(id: Array[Byte]): Unit = {
    val asset = Asset.IssuedAsset(ByteStr(id))
    // log.trace(s"[$tag] transactionHeightById($asset)")
    allTags.addDependent(MemCacheKey.Asset(asset), tag)
  }

  override def blockInfoByHeight(height: Int): Unit = {
    // log.trace(s"[$tag] blockInfoByHeight($height)")
    // HACK: we don't know, what is used in the script: blockInfoByHeight or lastBlock.
    // So we will force update the scripts those use one (or both) of these functions.
    // If this will be an issue, consider two different cases for H (height),
    // where H < currHeight - 100 or H >= currHeight - 100.
    allTags.addDependent(MemCacheKey.Height, tag)
  }

  override def data(address: Address, key: String): Unit = {
    // log.trace(s"[$tag] data($address, $key)")
    allTags.addDependent(MemCacheKey.AccountData(address, key), tag)
  }

  // We don't support it for now because of no demand. Use GET /utils/script/evaluate if needed.
  override def hasData(address: Address): Unit = {}

  override def resolveAlias(name: String): Unit = {
    // log.trace(s"[$tag] resolveAlias($name)")
    com.wavesplatform.account.Alias.create(name).foreach(x => allTags.addDependent(MemCacheKey.Alias(x), tag))
  }

  override def accountBalanceOf(address: Address, assetId: Option[Array[Byte]]): Unit = {
    val asset = Asset.fromCompatId(assetId.map(ByteStr(_)))
    // log.trace(s"[$tag] accountBalanceOf($address, $asset)")
    allTags.addDependent(MemCacheKey.AccountBalance(address, asset), tag)
  }

  override def accountWavesBalanceOf(address: Address): Unit = {
    // log.trace(s"[$tag] accountWavesBalanceOf($address)")
    allTags.addDependent(MemCacheKey.AccountBalance(address, Asset.Waves), tag)
    allTags.addDependent(MemCacheKey.AccountLeaseBalance(address), tag)
    allTags.addDependent(MemCacheKey.MaliciousMinerBanHeights(address), tag)
  }

  override def accountScript(address: Address): Unit = {
    // log.trace(s"[$tag] accountScript($address)")
    allTags.addDependent(MemCacheKey.AccountScript(address), tag)
  }

  override def callScript(dApp: Address): Unit = {
    // log.trace(s"[$tag] callScript($address)")
    accountScript(dApp)
  }
}
