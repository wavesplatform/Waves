package com.wavesplatform.ride.runner.environments

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.ride.runner.storage.SharedBlockchainStorage
import com.wavesplatform.state.TransactionId
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.ScorexLogging

trait DAppEnvironmentTracker {
  def height(): Unit
  def transactionById(id: Array[Byte]): Unit
  def transferTransactionById(id: Array[Byte]): Unit
  def transactionHeightById(id: Array[Byte]): Unit
  def assetInfoById(id: Array[Byte]): Unit
  def lastBlockOpt(): Unit
  def blockInfoByHeight(height: Int): Unit
  def data(addressOrAlias: Recipient, key: String): Unit
  def hasData(addressOrAlias: Recipient): Unit
  def resolveAlias(name: String): Unit
  def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Unit
  def accountWavesBalanceOf(addressOrAlias: Recipient): Unit
  def accountScript(addressOrAlias: Recipient): Unit
  def callScript(dApp: Recipient.Address): Unit
}

// TODO It seems, we can eliminate sharedBlockchain
class DefaultDAppEnvironmentTracker[TagT](sharedBlockchain: SharedBlockchainStorage[TagT], tag: TagT)
    extends DAppEnvironmentTracker
    with ScorexLogging {
  override def height(): Unit = {
    // log.trace(s"[$tag] height")
    sharedBlockchain.heightS.addDependent(tag)
  }

  override def lastBlockOpt(): Unit = {
    log.trace(s"[$tag] lastBlockOpt")
    sharedBlockchain.heightS.addDependent(tag)
  }

  // TODO A: this won't change
  override def transactionById(id: Array[Byte]): Unit = {
    val txId = TransactionId @@ ByteStr(id)
    // log.trace(s"[$tag] transactionById($txId)")
    sharedBlockchain.transactions.addDependent(txId, tag)
  }

  override def transferTransactionById(id: Array[Byte]): Unit = {
    val txId = TransactionId @@ ByteStr(id)
    // log.trace(s"[$tag] transferTransactionById($txId)")
    sharedBlockchain.transactions.addDependent(txId, tag)
  }

  // TODO A: this can change
  override def transactionHeightById(id: Array[Byte]): Unit = {
    val txId = TransactionId @@ ByteStr(id)
    // log.trace(s"[$tag] transactionHeightById($txId)")
    sharedBlockchain.transactions.addDependent(txId, tag)
  }

  override def assetInfoById(id: Array[Byte]): Unit = {
    val asset = Asset.IssuedAsset(ByteStr(id))
    // log.trace(s"[$tag] transactionHeightById($asset)")
    sharedBlockchain.assets.addDependent(asset, tag)
  }

  // TODO Two different cases for H, where H < currHeight - 100 and H >= currHeight - 100
  override def blockInfoByHeight(height: Int): Unit = {
    // log.trace(s"[$tag] blockInfoByHeight($height)")
//      if (height < sharedBlockchain.height - 100)
//      sharedBlockchain.blockHeaders(height, tag)
  }

  override def data(addressOrAlias: Recipient, key: String): Unit =
    withResolvedAlias(addressOrAlias).foreach { addr =>
      // log.trace(s"[$tag] data($addr, $key)")
      sharedBlockchain.data.addDependent((addr, key), tag)
    }

  // TODO #16 We don't support it for now, use GET /utils/script/evaluate , see ScriptBlockchain
  override def hasData(addressOrAlias: Recipient): Unit = {}

  override def resolveAlias(name: String): Unit = {
    // log.trace(s"[$tag] resolveAlias($name)")
    com.wavesplatform.account.Alias.create(name).foreach(sharedBlockchain.aliases.addDependent(_, tag))
  }

  override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Unit =
    withResolvedAlias(addressOrAlias).foreach { addr =>
      val asset = Asset.fromCompatId(assetId.map(ByteStr(_)))
      // log.trace(s"[$tag] accountBalanceOf($addr, $asset)")
      sharedBlockchain.accountBalances.addDependent((addr, asset), tag)
    }

  override def accountWavesBalanceOf(addressOrAlias: Recipient): Unit =
    withResolvedAlias(addressOrAlias).foreach { addr =>
      // log.trace(s"[$tag] accountWavesBalanceOf($addr)")
      sharedBlockchain.accountBalances.addDependent((addr, Asset.Waves), tag)
      sharedBlockchain.accountLeaseBalances.addDependent(addr, tag)
    }

  override def accountScript(addressOrAlias: Recipient): Unit =
    withResolvedAlias(addressOrAlias).foreach { addr =>
      // log.trace(s"[$tag] accountScript($addr)")
      sharedBlockchain.accountScripts.addDependent(addr, tag)
    }

  override def callScript(dApp: Recipient.Address): Unit =
    toWavesAddress(dApp).foreach { addr =>
      // log.trace(s"[$tag] callScript($addr)")
      sharedBlockchain.accountScripts.addDependent(addr, tag)
    }

  private def withResolvedAlias(addressOrAlias: Recipient): Option[Address] = {
    addressOrAlias match {
      case addressOrAlias: Recipient.Address => toWavesAddress(addressOrAlias)
      case Recipient.Alias(name) =>
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
