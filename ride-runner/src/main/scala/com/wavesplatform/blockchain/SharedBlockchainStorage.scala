package com.wavesplatform.blockchain

import cats.syntax.option.*
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.blockchain.caches.PersistentCaches
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.EstimatorProvider
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.storage.*
import com.wavesplatform.utils.ScorexLogging

import scala.util.chaining.scalaUtilChainingOps

class SharedBlockchainStorage[TagT](val settings: BlockchainSettings, caches: PersistentCaches, blockchainApi: BlockchainGrpcApi)
    extends ScorexLogging {
  private val chainId = settings.addressSchemeCharacter.toByte

  val data = new AccountDataStorage[TagT](blockchainApi, caches.accountDataEntries)

  val accountScripts = new AccountScriptStorage[TagT](chainId, estimator, blockchainApi, caches.accountScripts)

  // It seems, we don't need to update this. Only for some optimization needs
  private val blockHeaders = RideData.mapReadOnly[Int, SignedBlockHeader, TagT] { h =>
    if (h > height) throw new RuntimeException(s"Can't receive a block with height=$h > current height=$height")
    else load(caches.getBlockHeader, blockchainApi.getBlockHeader, caches.setBlockHeader)(h)
  }
  def getBlockHeader(height: Int, tag: TagT): Option[SignedBlockHeader] = blockHeaders.get(height, tag)

  private val vrf = RideData.mapReadOnly[Int, ByteStr, TagT] { h =>
    if (h > height) throw new RuntimeException(s"Can't receive a block VRF with height=$h > current height=$height")
    else load(caches.getVrf, blockchainApi.getVrf, caches.setVrf)(h)
  }
  def getVrf(height: Int, tag: TagT): Option[ByteStr] = vrf.get(height, tag)

  // Ride: wavesBalance, height, lastBlock TODO: a binding in Ride?
  private var _height: Int = caches.getHeight.getOrElse {
    blockchainApi.getCurrentBlockchainHeight().tap(caches.setHeight)
  }
  def height: Int = _height
  def setHeight(height: Int): Unit = {
    caches.setHeight(height)
    _height = height
  }

  // No way to get this from blockchain updates
  var activatedFeatures =
    load[Unit, Map[Short, Int]](
      _ => caches.getActivatedFeatures(),
      _ => blockchainApi.getActivatedFeatures(height).some,
      (_, xs) => xs.mayBeValue.foreach(caches.setActivatedFeatures)
    )(())
      .getOrElse(throw new RuntimeException("Impossible: activated features are empty"))

  val assets = new AssetStorage[TagT](blockchainApi, caches.assetDescriptions)

  // It seems, we don't need to update this. Only for some optimization needs
  private val aliases = RideData.anyRefMap[Alias, Address, TagT] {
    load(caches.resolveAlias, blockchainApi.resolveAlias, caches.setAlias)
  }

  def getAlias(alias: Alias, tag: TagT): Option[Address] = aliases.get(alias, tag)

  val portfolios = new PortfolioStorage[TagT](blockchainApi, caches.balances)

  val transactions = new TransactionsStorage[TagT](blockchainApi, caches.transactions)

  private def estimator: ScriptEstimator = EstimatorProvider.byActivatedFeatures(settings.functionalitySettings, activatedFeatures, height)

  private def load[KeyT, ValueT](
      fromCache: KeyT => RemoteData[ValueT],
      fromBlockchain: KeyT => Option[ValueT],
      updateCache: (KeyT, RemoteData[ValueT]) => Unit
  )(key: KeyT): Option[ValueT] =
    fromCache(key)
      .or(RemoteData.loaded(fromBlockchain(key)).tap(updateCache(key, _)))
      .mayBeValue
}
