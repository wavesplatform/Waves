package com.wavesplatform.blockchain

import cats.syntax.option.*
import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.blockchain.SharedBlockchainData.Settings
import com.wavesplatform.features.EstimatorProvider
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.ride.estimate
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.storage.*
import com.wavesplatform.storage.persistent.PersistentCaches
import com.wavesplatform.utils.ScorexLogging

import scala.util.chaining.scalaUtilChainingOps

class SharedBlockchainData[TagT](settings: Settings, persistentCaches: PersistentCaches, blockchainApi: BlockchainApi) extends ScorexLogging {
  val blockchainSettings: BlockchainSettings = settings.blockchain

  private val chainId = blockchainSettings.addressSchemeCharacter.toByte

  val data = new AccountDataStorage[TagT](settings.caches.accountData, chainId, blockchainApi, persistentCaches.accountDataEntries)

  val accountScripts = new AccountScriptStorage[TagT](
    settings.caches.accountScript,
    chainId,
    script => Map(estimator.version -> estimate(height, activatedFeatures, estimator, script, isAsset = false)),
    blockchainApi,
    persistentCaches.accountScripts
  )

  val blockHeaders = new BlockHeadersStorage(blockchainApi, persistentCaches.blockHeaders)

  val vrf = new VrfStorage(settings.caches.vrf, blockchainApi, persistentCaches.vrf, height)

  // Ride: wavesBalance, height, lastBlock
  def height: Int = blockHeaders.last.height

  // No way to get this from blockchain updates
  var activatedFeatures =
    load[Unit, Map[Short, Int]](
      _ => persistentCaches.getActivatedFeatures(),
      _ => blockchainApi.getActivatedFeatures(height).some,
      (_, xs) => xs.mayBeValue.foreach(persistentCaches.setActivatedFeatures)
    )(())
      .getOrElse(throw new RuntimeException("Impossible: activated features are empty"))

  val assets = new AssetStorage[TagT](settings.caches.asset, blockchainApi, persistentCaches.assetDescriptions)

  val aliases = new AliasStorage[TagT](settings.caches.alias, chainId, blockchainApi, persistentCaches.aliases)

  val accountBalances = new AccountBalanceStorage[TagT](settings.caches.accountBalance, chainId, blockchainApi, persistentCaches.accountBalances)

  val accountLeaseBalances =
    new AccountLeaseBalanceStorage[TagT](settings.caches.accountLease, chainId, blockchainApi, persistentCaches.accountLeaseBalances)

  val transactions = new TransactionStorage[TagT](settings.caches.transaction, blockchainApi, persistentCaches.transactions)

  private def estimator: ScriptEstimator = EstimatorProvider.byActivatedFeatures(blockchainSettings.functionalitySettings, activatedFeatures, height)

  private def load[KeyT, ValueT](
      fromCache: KeyT => RemoteData[ValueT],
      fromBlockchain: KeyT => Option[ValueT],
      updateCache: (KeyT, RemoteData[ValueT]) => Unit
  )(key: KeyT): Option[ValueT] =
    fromCache(key)
      .orElse(RemoteData.loaded(fromBlockchain(key)).tap(updateCache(key, _)))
      .mayBeValue
}

object SharedBlockchainData {
  case class Settings(blockchain: BlockchainSettings, caches: CachesSettings)

  case class CachesSettings(
      accountData: ExactWithHeightStorage.Settings,
      vrf: ExactWithHeightStorage.Settings,
      accountScript: ExactWithHeightStorage.Settings,
      asset: ExactWithHeightStorage.Settings,
      alias: ExactWithHeightStorage.Settings,
      accountBalance: ExactWithHeightStorage.Settings,
      accountLease: ExactWithHeightStorage.Settings,
      transaction: ExactWithHeightStorage.Settings
  )
}
