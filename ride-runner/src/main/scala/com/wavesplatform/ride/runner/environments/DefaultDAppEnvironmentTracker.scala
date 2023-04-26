package com.wavesplatform.ride.runner.environments

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.ride.runner.storage.{CacheKey, SharedBlockchainStorage}
import com.wavesplatform.state.TransactionId
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.ScorexLogging

class DefaultDAppEnvironmentTracker[TagT](sharedBlockchain: SharedBlockchainStorage[TagT], tag: TagT)
    extends DAppEnvironmentTracker
    with ScorexLogging {
  override def height(): Unit = {
    // log.trace(s"[$tag] height")
    sharedBlockchain.addDependent(CacheKey.Height, tag)
  }

  override def lastBlockOpt(): Unit = {
//    log.trace(s"[$tag] lastBlockOpt")
    height()
  }

  // TODO A: this won't change
  override def transactionById(id: Array[Byte]): Unit = {
    val txId = CacheKey.Transaction(TransactionId @@ ByteStr(id))
    // log.trace(s"[$tag] transactionById($txId)")
    sharedBlockchain.addDependent(txId, tag)
  }

  override def transferTransactionById(id: Array[Byte]): Unit = {
    val txId = CacheKey.Transaction(TransactionId @@ ByteStr(id))
    // log.trace(s"[$tag] transferTransactionById($txId)")
    sharedBlockchain.addDependent(txId, tag)
  }

  // TODO A: this can change
  override def transactionHeightById(id: Array[Byte]): Unit = {
    val txId = CacheKey.Transaction(TransactionId @@ ByteStr(id))
    // log.trace(s"[$tag] transactionHeightById($txId)")
    sharedBlockchain.addDependent(txId, tag)
  }

  override def assetInfoById(id: Array[Byte]): Unit = {
    val asset = Asset.IssuedAsset(ByteStr(id))
    // log.trace(s"[$tag] transactionHeightById($asset)")
    sharedBlockchain.addDependent(CacheKey.Asset(asset), tag)
  }

  // TODO Two different cases for H, where H < currHeight - 100 and H >= currHeight - 100
  override def blockInfoByHeight(height: Int): Unit = {
    // log.trace(s"[$tag] blockInfoByHeight($height)")
    // if (height < sharedBlockchain.height - 100)
    // sharedBlockchain.blockHeaders.add(height, tag)
  }

  override def data(addressOrAlias: Recipient, key: String): Unit =
    withResolvedAlias(addressOrAlias).foreach { addr =>
      // log.trace(s"[$tag] data($addr, $key)")
      sharedBlockchain.addDependent(CacheKey.AccountData(addr, key), tag)
    }

  // TODO #16 We don't support it for now, use GET /utils/script/evaluate , see ScriptBlockchain
  override def hasData(addressOrAlias: Recipient): Unit = {}

  override def resolveAlias(name: String): Unit = {
    // log.trace(s"[$tag] resolveAlias($name)")
    com.wavesplatform.account.Alias.create(name).foreach(x => sharedBlockchain.addDependent(CacheKey.Alias(x), tag))
  }

  override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Unit = {
    withResolvedAlias(addressOrAlias).foreach { addr =>
      val asset = Asset.fromCompatId(assetId.map(ByteStr(_)))
      // log.trace(s"[$tag] accountBalanceOf($addr, $asset)")
      sharedBlockchain.addDependent(CacheKey.AccountBalance(addr, asset), tag)
    }
  }

  override def accountWavesBalanceOf(addressOrAlias: Recipient): Unit =
    withResolvedAlias(addressOrAlias).foreach { addr =>
      // log.trace(s"[$tag] accountWavesBalanceOf($addr)")
      // TODO merge?
      sharedBlockchain.addDependent(CacheKey.AccountBalance(addr, Asset.Waves), tag)
      sharedBlockchain.addDependent(CacheKey.AccountLeaseBalance(addr), tag)
    }

  override def accountScript(addressOrAlias: Recipient): Unit =
    withResolvedAlias(addressOrAlias).foreach { addr =>
      // log.trace(s"[$tag] accountScript($addr)")
      sharedBlockchain.addDependent(CacheKey.AccountScript(addr), tag)
    }

  override def callScript(dApp: Recipient.Address): Unit =
    // log.trace(s"[$tag] callScript($addr)")
    accountScript(dApp)

  private def withResolvedAlias(addressOrAlias: Recipient): Option[Address] = {
    addressOrAlias match {
      case addressOrAlias: Recipient.Address => toWavesAddress(addressOrAlias)
      case Recipient.Alias(name)             =>
        // TODO Add dependency?
        com.wavesplatform.account.Alias
          .create(name)
          .flatMap(sharedBlockchain.resolveAlias)
          .toOption
    }
  }

  private def toWavesAddress(addr: Recipient.Address): Option[Address] =
    com.wavesplatform.account.Address
      .fromBytes(addr.bytes.arr, sharedBlockchain.chainId)
      .toOption
}
