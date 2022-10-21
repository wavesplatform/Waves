package com.wavesplatform.ride

import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.features.EstimatorProvider.EstimatorBlockchainExt
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.PBAmounts.toAssetAndAmount
import com.wavesplatform.protobuf.transaction.PBTransactions.{toVanillaDataEntry, toVanillaScript}
import com.wavesplatform.ride.MutableBlockchain.CachedData
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{
  AccountScriptInfo,
  AssetDescription,
  AssetScriptInfo,
  BalanceSnapshot,
  Blockchain,
  DataEntry,
  Height,
  LeaseBalance,
  Portfolio,
  TxMeta,
  VolumeAndFee
}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.AliasDoesNotExist
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.transaction.{Asset, ERC20Address, Transaction}
import com.wavesplatform.utils.ScorexLogging

import java.nio.charset.StandardCharsets
import scala.collection.mutable

class MutableBlockchain(override val settings: BlockchainSettings, blockchainApi: BlockchainGrpcApi) extends Blockchain with ScorexLogging {
  private val chainId = settings.addressSchemeCharacter.toByte

  private def kill(methodName: String) = throw new RuntimeException(methodName)

  private val data = mutable.AnyRefMap.empty[Address, mutable.AnyRefMap[String, CachedData[DataEntry[_]]]]

  // TODO return Boolean to know: we updated the data or not/
  def replaceAccountData(update: StateUpdate.DataEntryUpdate): Unit = {
    val address = update.address.toAddress
    data.get(address).foreach { data =>
      val key = update.getDataEntry.key
      data.updateWith(key) {
        case Some(_) =>
          log.debug(s"[$address, $key] Updated data")
          Some(CachedData.Cached(toVanillaDataEntry(update.getDataEntry)))
        case x => x
      }
    }
  }

  // TODO use utils/evaluate through REST API
  // Ride: isDataStorageUntouched
  override def hasData(address: Address): Boolean = data.contains(address)

  // Ride: get*Value (data), get* (data)
  override def accountData(address: Address, key: String): Option[DataEntry[_]] = {
    val xs = data.updateWith(address) {
      case None => Some(mutable.AnyRefMap.empty[String, CachedData[DataEntry[_]]])
      case x    => x
    }

    xs.get
      .updateWith(key) {
        case None => Some(CachedData.loaded(blockchainApi.getAccountDataEntry(address, key)))
        case x    => x
      }
      .flatMap(_.mayBeValue)
  }

  // None means "we don't know". Some(None) means "we know, there is no script"
  private val accountScripts = mutable.AnyRefMap.empty[Address, CachedData[AccountScriptInfo]]
  def replaceAccountScript(account: PublicKey, newScript: ByteString): Unit = {
    val address = account.toAddress(chainId)
    accountScripts.updateWith(address) {
      case Some(_) =>
        log.debug(s"[$address] Updated account script")

        val script = toVanillaScript(newScript)

        // TODO dup, see BlockchainGrpcApi

        // DiffCommons
        val fixEstimateOfVerifier    = true // blockchain.isFeatureActivated(BlockchainFeatures.RideV6)
        val useContractVerifierLimit = true // !isAsset && blockchain.useReducedVerifierComplexityLimit

        Some(CachedData.loaded(script.map { script =>
          // TODO explicitGet?
          val complexityInfo = Script.complexityInfo(script, this.estimator, fixEstimateOfVerifier, useContractVerifierLimit).explicitGet()

          AccountScriptInfo(
            publicKey = account,
            script = script, // Only this field matters in Ride Runner, see MutableBlockchain.accountScript
            verifierComplexity = complexityInfo.verifierComplexity,
            complexitiesByEstimator = Map(this.estimator.version -> complexityInfo.callableComplexities)
          )
        }))
      case x => x
    }
  }

  private def withAccountScript(address: Address): Option[CachedData[AccountScriptInfo]] =
    accountScripts.updateWith(address) {
      case None => Some(CachedData.loaded(blockchainApi.getAccountScript(address, this.estimator)))
      case x    => x
    }

  // Ride: scriptHash
  override def accountScript(address: Address): Option[AccountScriptInfo] = withAccountScript(address).get.mayBeValue

  override def hasAccountScript(address: Address) = withAccountScript(address).get.mayBeValue.nonEmpty

  // It seems, we don't need to update this. Only for some optimization needs
  private val blockHeaders = mutable.LongMap.empty[(SignedBlockHeader, ByteStr)]

  // Ride: blockInfoByHeight, lastBlock
  override def blockHeader(height: Int): Option[SignedBlockHeader] = {
    if (this.height < height) None
    else {
      val header = blockHeaders
        .updateWith(height) {
          case None => blockchainApi.getBlockHeader(height)
          case x    => x
        }
        .map(_._1)

      // Dirty, but we have a clear error instead of "None.get"
      if (header.isEmpty) throw new RuntimeException(s"blockHeader($height): can't find a block, please specify or check your script")
      else header
    }
  }

  // Ride: blockInfoByHeight
  override def hitSource(height: Int): Option[ByteStr] =
    if (this.height < height) None
    else {
      val header = blockHeaders
        .updateWith(height) {
          case None => blockchainApi.getBlockHeader(height)
          case x    => x
        }
        .map(_._2)

      // Dirty, but we have a clear error instead of "None.get"
      if (header.isEmpty) throw new RuntimeException(s"blockHeader($height): can't find VRF, please specify or check your script")
      else header
    }

  // Ride: wavesBalance, height, lastBlock TODO: a binding in Ride?
  private var _height: Int = blockchainApi.getCurrentBlockchainHeight()

  override def height: Int = _height
  // TODO How do we known that this field is used in a script?
  def setHeight(height: Int): Unit = {
    log.debug(s"Updated height = $height")
    _height = height
  }

  var _activatedFeatures = blockchainApi.getActivatedFeatures(height)
  // No way to get this from blockchain updates
  def setActivatedFeature(featureId: Short, height: Int): Unit =
    _activatedFeatures = _activatedFeatures.updated(featureId, height)

  override def activatedFeatures: Map[Short, Int] = _activatedFeatures

  private val assets = mutable.AnyRefMap.empty[IssuedAsset, AssetDescription]
  def replaceAssetDescription(update: StateUpdate.AssetDetails): Unit = {
    val asset = update.assetId.toIssuedAsset
    assets.updateWith(asset) {
      case Some(_) =>
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
      case x => x
    }
  }

  // Ride: assetInfo
  override def assetDescription(id: Asset.IssuedAsset): Option[AssetDescription] =
    assets.updateWith(id) {
      case None => blockchainApi.getAssetDescription(id)
      case x    => x
    }

  // Ride (indirectly): asset script validation
  override def assetScript(id: Asset.IssuedAsset): Option[AssetScriptInfo] = assetDescription(id).flatMap(_.script)

  // It seems, we don't need to update this. Only for some optimization needs
  private val aliases = mutable.AnyRefMap.empty[Alias, Address]
  // Ride: get*Value (data), get* (data), isDataStorageUntouched, balance, scriptHash, wavesBalance
  override def resolveAlias(a: Alias): Either[ValidationError, Address] =
    aliases
      .updateWith(a) {
        case None => blockchainApi.resolveAlias(a)
        case x    => x
      }
      .toRight(AliasDoesNotExist(a): ValidationError)

  private val portfolios = mutable.AnyRefMap.empty[Address, Portfolio]
  def replaceBalance(toReplace: StateUpdate.BalanceUpdate): Unit = {
    val address = toReplace.address.toAddress
    portfolios.updateWith(address) {
      case Some(prev) =>
        val (asset, after) = toAssetAndAmount(toReplace.getAmountAfter)
        log.debug(s"[$address, $asset] Updated balance: $after")
        Some(asset match {
          case Asset.Waves        => prev.copy(balance = after)
          case asset: IssuedAsset => prev.copy(assets = prev.assets.updated(asset, after))
        })
      case x => x
    }
  }
  def replaceLeasing(toReplace: StateUpdate.LeasingUpdate): Unit = {
    val address = toReplace.address.toAddress
    portfolios.updateWith(address) {
      case Some(prev) =>
        log.debug(s"[$address] Updated leasing")
        Some(prev.copy(lease = LeaseBalance(toReplace.inAfter, toReplace.outAfter)))
      case x => x
    }
  }

  private def withPortfolios(address: Address): Portfolio =
    portfolios
      .updateWith(address) {
        case None => Some(blockchainApi.getBalances(address))
        case x    => x
      }
      .get

  // Ride: wavesBalance
  override def leaseBalance(address: Address): LeaseBalance = withPortfolios(address).lease

  // Ride: assetBalance, wavesBalance
  override def balance(address: Address, mayBeAssetId: Asset): Long = withPortfolios(address).balanceOf(mayBeAssetId)

  // Ride: wavesBalance (specifies to=None)
  /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
  override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] =
    // "to" always None
    // TODO this should work correctly
    List(BalanceSnapshot(height, withPortfolios(address)))
  // input.balanceSnapshots.getOrElse(address, Seq(BalanceSnapshot(height, 0, 0, 0))).filter(_.height >= from)

  // It seems, we don't need to update this. Only for some optimization needs
  private val transactions = mutable.AnyRefMap.empty[ByteStr, (TxMeta, Option[TransferTransactionLike])]

  // Got a transaction, got a rollback, same transaction on new height/failed/removed
  def replaceTransactionMeta(pbId: ByteString, height: Int): Unit = {
    val id = pbId.toByteStr
    transactions.updateWith(id) {
      case Some((_, tx)) =>
        log.debug(s"[$id] Updated transaction")
        Some(
          (
            TxMeta(
              height = Height(height),
              succeeded = true,   // Not used in Ride
              spentComplexity = 0 // TODO ???
            ),
            tx
          )
        )
      case x => x
    }
  }

  private def withTransactions(id: ByteStr): Option[(TxMeta, Option[TransferTransactionLike])] =
    transactions.updateWith(id) {
      case None => blockchainApi.getTransaction(id)
      case x    => x
    }

  // Ride: transactionHeightById
  override def transactionMeta(id: ByteStr): Option[TxMeta] = withTransactions(id).map(_._1)

  // Ride: transferTransactionById
  override def transferById(id: ByteStr): Option[(Int, TransferTransactionLike)] =
    withTransactions(id).flatMap { case (meta, tx) => tx.map((meta.height, _)) }

  override def score: BigInt = kill("score")

  override def carryFee: Long = kill("carryFee")

  override def heightOf(blockId: ByteStr): Option[Int] = kill("heightOf")

  /** Features related */
  override def approvedFeatures: Map[Short, Int] = kill("approvedFeatures")

  override def featureVotes(height: Int): Map[Short, Int] = kill("featureVotes")

  override def containsTransaction(tx: Transaction): Boolean = kill("containsTransaction")

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = kill("leaseDetails")

  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee = kill("filledVolumeAndFee")

  override def transactionInfo(id: BlockId) = kill("transactionInfo")

  /** Block reward related */
  override def blockReward(height: Int): Option[Long] = kill("blockReward")

  override def blockRewardVotes(height: Int): Seq[Long] = kill("blockRewardVotes")

  override def wavesAmount(height: Int): BigInt = kill("wavesAmount")

  override def balanceAtHeight(address: Address, height: Int, assetId: Asset): Option[(Int, Long)] = kill("balanceAtHeight")

  // GET /eth/assets
  override def resolveERC20Address(address: ERC20Address): Option[Asset.IssuedAsset] = kill("resolveERC20Address")
}

object MutableBlockchain {
  sealed trait CachedData[+T] extends Product with Serializable {
    def isLoaded: Boolean
    def mayBeValue: Option[T]
  }
  object CachedData {
    case object NotLoaded extends CachedData[Nothing] {
      override val isLoaded   = false
      override val mayBeValue = None
    }
    case class Cached[T](value: T) extends CachedData[T] {
      override def isLoaded: Boolean     = true
      override def mayBeValue: Option[T] = Some(value)
    }
    case object Absence extends CachedData[Nothing] {
      override val isLoaded   = true
      override val mayBeValue = None
    }

    def loaded[T](x: Option[T]): CachedData[T] = x match {
      case Some(x) => Cached(x)
      case None    => Absence
    }
  }
}
