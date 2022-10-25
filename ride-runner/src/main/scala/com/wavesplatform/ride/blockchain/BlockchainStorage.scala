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
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, AssetScriptInfo, DataEntry, Height, LeaseBalance, Portfolio, TxMeta}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.utils.ScorexLogging

import java.nio.charset.StandardCharsets

class BlockchainStorage[TagT](val settings: BlockchainSettings, blockchainApi: BlockchainGrpcApi) extends ScorexLogging {
  private val chainId = settings.addressSchemeCharacter.toByte

  val data = RideData.anyRefMap[(Address, String), DataEntry[?], TagT](Function.tupled(blockchainApi.getAccountDataEntry))
  def replaceAccountData(update: StateUpdate.DataEntryUpdate): Set[TagT] = {
    val address = update.address.toAddress
    val key     = update.getDataEntry.key
    data.replaceIfKnown((address, key)) { _ =>
      log.debug(s"[$address, $key] Updated data")
      Some(toVanillaDataEntry(update.getDataEntry))
    }
  }

  val accountScripts = RideData.anyRefMap[Address, AccountScriptInfo, TagT](blockchainApi.getAccountScript(_, estimator))
  def replaceAccountScript(account: PublicKey, newScript: ByteString): Set[TagT] = {
    val address = account.toAddress(chainId)
    accountScripts.replaceIfKnown(address) { _ =>
      log.debug(s"[$address] Updated account script")

      val script = toVanillaScript(newScript)

      // TODO dup, see BlockchainGrpcApi

      // DiffCommons
      val fixEstimateOfVerifier    = true // blockchain.isFeatureActivated(BlockchainFeatures.RideV6)
      val useContractVerifierLimit = true // !isAsset && blockchain.useReducedVerifierComplexityLimit

      script.map { script =>
        // TODO explicitGet?
        val complexityInfo = Script.complexityInfo(script, estimator, fixEstimateOfVerifier, useContractVerifierLimit).explicitGet()

        AccountScriptInfo(
          publicKey = account,
          script = script, // Only this field matters in Ride Runner, see MutableBlockchain.accountScript
          verifierComplexity = complexityInfo.verifierComplexity,
          complexitiesByEstimator = Map(estimator.version -> complexityInfo.callableComplexities)
        )
      }
    }
  }

  // It seems, we don't need to update this. Only for some optimization needs
  val blockHeaders = RideData.mapReadOnly[Int, (SignedBlockHeader, ByteStr), TagT] { h =>
    if (h > height) throw new RuntimeException(s"Can't receive a block with height=$h > current height=$height")
    else blockchainApi.getBlockHeader(h)
  }

  // Ride: wavesBalance, height, lastBlock TODO: a binding in Ride?
  private var _height: Int = blockchainApi.getCurrentBlockchainHeight()

  def height: Int = _height

  // TODO How do we known that this field is used in a script?
  def setHeight(height: Int): Unit = {
    log.debug(s"Updated height = $height")
    _height = height
  }

  // No way to get this from blockchain updates
  var activatedFeatures = blockchainApi.getActivatedFeatures(height)

  val assets = RideData.anyRefMap[IssuedAsset, AssetDescription, TagT](blockchainApi.getAssetDescription)
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
    }
  }

  // It seems, we don't need to update this. Only for some optimization needs
  val aliases = RideData.anyRefMap[Alias, Address, TagT](blockchainApi.resolveAlias)

  val portfolios = RideData.anyRefMap[Address, Portfolio, TagT](blockchainApi.getBalances(_).some)

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
    }
  }

  def replaceLeasing(toReplace: StateUpdate.LeasingUpdate): Set[TagT] = {
    val address = toReplace.address.toAddress
    portfolios.replaceIfKnown(address) { orig =>
      log.debug(s"[$address] Updated leasing")
      Some(orig.getOrElse(Portfolio.empty).copy(lease = LeaseBalance(toReplace.inAfter, toReplace.outAfter)))
    }
  }

  val transactions = RideData.anyRefMap[ByteStr, (TxMeta, Option[TransferTransactionLike]), TagT](blockchainApi.getTransaction)

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
            succeeded = true,   // Not used in Ride
            spentComplexity = 0 // TODO: It seems, not used
          ),
          tx
        )
      )
    }
  }

  private def estimator: ScriptEstimator = EstimatorProvider.byActivatedFeatures(settings.functionalitySettings, activatedFeatures, height)
}
