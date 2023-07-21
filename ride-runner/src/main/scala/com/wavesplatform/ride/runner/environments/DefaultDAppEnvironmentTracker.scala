package com.wavesplatform.ride.runner.environments

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.ride.runner.storage.{CacheKey, SharedBlockchainStorage}
import com.wavesplatform.state.TransactionId
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.ScorexLogging

class DefaultDAppEnvironmentTracker[TagT](sharedBlockchain: SharedBlockchainStorage[TagT], tag: TagT)
    extends DAppEnvironmentTracker
    with ScorexLogging {
  override def height(): Unit = {
    // log.trace(s"[$tag] height")
    sharedBlockchain.allTags.addDependent(CacheKey.Height, tag)
  }

  override def lastBlockOpt(): Unit = {
//    log.trace(s"[$tag] lastBlockOpt")
    height()
  }

  // TODO A: this won't change
  override def transactionById(id: Array[Byte]): Unit = {
    val txId = CacheKey.Transaction(TransactionId @@ ByteStr(id))
    // log.trace(s"[$tag] transactionById($txId)")
    sharedBlockchain.allTags.addDependent(txId, tag)
  }

  override def transferTransactionById(id: Array[Byte]): Unit = {
    val txId = CacheKey.Transaction(TransactionId @@ ByteStr(id))
    // log.trace(s"[$tag] transferTransactionById($txId)")
    sharedBlockchain.allTags.addDependent(txId, tag)
  }

  // TODO A: this can change
  override def transactionHeightById(id: Array[Byte]): Unit = {
    val txId = CacheKey.Transaction(TransactionId @@ ByteStr(id))
    // log.trace(s"[$tag] transactionHeightById($txId)")
    sharedBlockchain.allTags.addDependent(txId, tag)
  }

  override def assetInfoById(id: Array[Byte]): Unit = {
    val asset = Asset.IssuedAsset(ByteStr(id))
    // log.trace(s"[$tag] transactionHeightById($asset)")
    sharedBlockchain.allTags.addDependent(CacheKey.Asset(asset), tag)
  }

  // TODO Two different cases for H, where H < currHeight - 100 and H >= currHeight - 100
  override def blockInfoByHeight(height: Int): Unit = {
    // log.trace(s"[$tag] blockInfoByHeight($height)")
    // if (height < sharedBlockchain.height - 100)
    // sharedBlockchain.blockHeaders.add(height, tag)
  }

  override def data(address: Address, key: String): Unit = {
    // log.trace(s"[$tag] data($addr, $key)")
    sharedBlockchain.allTags.addDependent(CacheKey.AccountData(address, key), tag)
  }

  // TODO #16 We don't support it for now, use GET /utils/script/evaluate , see ScriptBlockchain
  override def hasData(address: Address): Unit = {}

  override def resolveAlias(name: String): Unit = {
    // log.trace(s"[$tag] resolveAlias($name)")
    com.wavesplatform.account.Alias.create(name).foreach(x => sharedBlockchain.allTags.addDependent(CacheKey.Alias(x), tag))
  }

  override def accountBalanceOf(address: Address, assetId: Option[Array[Byte]]): Unit = {
    val asset = Asset.fromCompatId(assetId.map(ByteStr(_)))
    // log.trace(s"[$tag] accountBalanceOf($addr, $asset)")
    sharedBlockchain.allTags.addDependent(CacheKey.AccountBalance(address, asset), tag)
  }

  override def accountWavesBalanceOf(address: Address): Unit = {
    // log.trace(s"[$tag] accountWavesBalanceOf($addr)")
    // TODO merge?
    sharedBlockchain.allTags.addDependent(CacheKey.AccountBalance(address, Asset.Waves), tag)
    sharedBlockchain.allTags.addDependent(CacheKey.AccountLeaseBalance(address), tag)
  }

  override def accountScript(address: Address): Unit = {
    // log.trace(s"[$tag] accountScript($addr)")
    sharedBlockchain.allTags.addDependent(CacheKey.AccountScript(address), tag)
  }

  override def callScript(dApp: Address): Unit = {
    // log.trace(s"[$tag] callScript($addr)")
    accountScript(dApp)
  }
}
