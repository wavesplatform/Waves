package com.wavesplatform.ride.blockchain

import cats.syntax.option.*
import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.features.EstimatorProvider
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.PBAmounts.toAssetAndAmount
import com.wavesplatform.protobuf.transaction.PBTransactions.{toVanillaDataEntry, toVanillaScript}
import com.wavesplatform.ride.blockchain.caches.BlockchainCaches
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, AssetScriptInfo, DataEntry, Height, LeaseBalance, Portfolio, TxMeta}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.transaction.{Asset, EthereumTransaction, Transaction}
import com.wavesplatform.utils.ScorexLogging

import java.nio.charset.StandardCharsets
import scala.util.chaining.scalaUtilChainingOps

class SharedBlockchainStorage[TagT](val settings: BlockchainSettings, caches: BlockchainCaches, blockchainApi: BlockchainGrpcApi)
    extends ScorexLogging {
  private val chainId = settings.addressSchemeCharacter.toByte

  private val data = RideData.anyRefMap[(Address, String), DataEntry[?], TagT] {
    load[(Address, String), DataEntry[_]](
      Function.tupled(caches.getAccountDataEntry),
      Function.tupled(blockchainApi.getAccountDataEntry),
      (key, value) => caches.setAccountDataEntry(key._1, key._2, value)
    )
  }

  def getData(address: Address, key: String, tag: TagT): Option[DataEntry[_]] = data.get((address, key), tag)
  def replaceAccountData(update: StateUpdate.DataEntryUpdate): Set[TagT] = {
    val address = update.address.toAddress
    val key     = update.getDataEntry.key
    data.replaceIfKnown((address, key)) { _ =>
      log.debug(s"[$address, $key] Updated data")
      Some(toVanillaDataEntry(update.getDataEntry))
        .tap(r => caches.setAccountDataEntry(address, key, BlockchainData.loaded(r)))
    }
  }

  private val accountScripts = RideData.anyRefMap[Address, AccountScriptInfo, TagT] {
    load(caches.getAccountScript, blockchainApi.getAccountScript(_, estimator), caches.setAccountScript)
  }

  def getAccountScript(address: Address, tag: TagT): Option[AccountScriptInfo] = accountScripts.get(address, tag)
  def replaceAccountScript(account: PublicKey, newScript: ByteString): Set[TagT] = {
    val address = account.toAddress(chainId)
    accountScripts.replaceIfKnown(address) { _ =>
      log.debug(s"[$address] Updated account script")

      val script = toVanillaScript(newScript)

      // TODO dup, see BlockchainGrpcApi

      // DiffCommons
      val fixEstimateOfVerifier    = true // blockchain.isFeatureActivated(BlockchainFeatures.RideV6)
      val useContractVerifierLimit = true // !isAsset && blockchain.useReducedVerifierComplexityLimit

      script
        .map { script =>
          // TODO explicitGet?
          val complexityInfo = Script.complexityInfo(script, estimator, fixEstimateOfVerifier, useContractVerifierLimit).explicitGet()

          AccountScriptInfo(
            publicKey = account,
            script = script, // Only this field matters in Ride Runner, see MutableBlockchain.accountScript
            verifierComplexity = complexityInfo.verifierComplexity,
            complexitiesByEstimator = Map(estimator.version -> complexityInfo.callableComplexities)
          )
        }
        .tap(r => caches.setAccountScript(address, BlockchainData.loaded(r)))
    }
  }

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
  // TODO How do we known that this field is used in a script?
  def setHeight(height: Int): Unit = {
    log.debug(s"Updated height = $height")
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

  private val assets = RideData.anyRefMap[IssuedAsset, AssetDescription, TagT] {
    load(caches.getAssetDescription, blockchainApi.getAssetDescription, caches.setAssetDescription)
  }
  def getAssetDescription(asset: IssuedAsset, tag: TagT): Option[AssetDescription] = assets.get(asset, tag)
  def replaceAssetDescription(update: StateUpdate.AssetDetails): Set[TagT] = {
    val asset = update.assetId.toIssuedAsset
    assets.replaceIfKnown(asset) { _ =>
      log.debug(s"[$asset] Updated asset")
      Some(
        AssetDescription(
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
      )
        .tap(r => caches.setAssetDescription(asset, BlockchainData.loaded(r)))
    }
  }

  // It seems, we don't need to update this. Only for some optimization needs
  private val aliases = RideData.anyRefMap[Alias, Address, TagT] {
    load(caches.resolveAlias, blockchainApi.resolveAlias, caches.setAlias)
  }

  def getAlias(alias: Alias, tag: TagT): Option[Address] = aliases.get(alias, tag)

  private val portfolios = RideData.anyRefMap[Address, Portfolio, TagT] {
    load(caches.getBalances, blockchainApi.getBalances(_).some, caches.setBalances)
  }

  def getPortfolio(address: Address, tag: TagT): Option[Portfolio] = portfolios.get(address, tag)
  def replaceBalance(toReplace: StateUpdate.BalanceUpdate): Set[TagT] = {
    val address = toReplace.address.toAddress
    portfolios.replaceIfKnown(address) { mayBeOrig =>
      val (asset, after) = toAssetAndAmount(toReplace.getAmountAfter)
      log.debug(s"[$address, $asset] Updated balance: $after")

      val orig = mayBeOrig.getOrElse(Portfolio.empty)
      Some(asset match {
        case Asset.Waves        => orig.copy(balance = after)
        case asset: IssuedAsset => orig.copy(assets = orig.assets.updated(asset, after))
      })
        .tap(r => caches.setBalances(address, BlockchainData.loaded(r)))
    }
  }
  def replaceLeasing(toReplace: StateUpdate.LeasingUpdate): Set[TagT] = {
    val address = toReplace.address.toAddress
    portfolios.replaceIfKnown(address) { orig =>
      log.debug(s"[$address] Updated leasing")
      Some(orig.getOrElse(Portfolio.empty).copy(lease = LeaseBalance(toReplace.inAfter, toReplace.outAfter)))
        .tap(r => caches.setBalances(address, BlockchainData.loaded(r)))
    }
  }

  private val transactions = RideData.anyRefMap[ByteStr, (TxMeta, Option[Transaction]), TagT] {
    load(caches.getTransaction, blockchainApi.getTransferLikeTransaction, caches.setTransaction)
  }

  def getTransaction(id: ByteStr, tag: TagT): Option[(TxMeta, Option[TransferTransactionLike])] =
    transactions
      .get(id, tag)
      .map { case (meta, maybeTx) =>
        val tx = maybeTx.flatMap {
          case tx: TransferTransactionLike => tx.some
          case tx: EthereumTransaction =>
            tx.payload match {
              case payload: EthereumTransaction.Transfer =>
                // tx.toTransferLike()
                // payload.toTransferLike(tx, this).toOption
                none
              case _ => none
            }
          case _ => none
        }
        (meta, tx)
      }

  // Got a transaction, got a rollback, same transaction on new height/failed/removed
  def replaceTransactionMeta(pbId: ByteString, height: Int): Set[TagT] = {
    val id = pbId.toByteStr
    transactions.replaceIfKnown(id) { mayBeOrig =>
      log.debug(s"[$id] Updated transaction")
      val (_, tx) = mayBeOrig.getOrElse((TxMeta.empty, None))
      Some(
        (
          TxMeta(
            height = Height(height),
            succeeded = true,
            spentComplexity = 0
          ),
          tx
        )
      )
        .tap(r => caches.setTransaction(id, BlockchainData.loaded(r)))
    }
  }

  private def estimator: ScriptEstimator = EstimatorProvider.byActivatedFeatures(settings.functionalitySettings, activatedFeatures, height)

  private def load[KeyT, ValueT](
      fromCache: KeyT => BlockchainData[ValueT],
      fromBlockchain: KeyT => Option[ValueT],
      updateCache: (KeyT, BlockchainData[ValueT]) => Unit
  )(key: KeyT): Option[ValueT] =
    fromCache(key)
      .or(BlockchainData.loaded(fromBlockchain(key)).tap(updateCache(key, _)))
      .mayBeValue
}
