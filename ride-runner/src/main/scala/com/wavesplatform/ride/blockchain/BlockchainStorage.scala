package com.wavesplatform.ride.blockchain

import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.collections.syntax.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.PBAmounts.toAssetAndAmount
import com.wavesplatform.protobuf.transaction.PBTransactions.toVanillaScript
import com.wavesplatform.settings.BlockchainSettings
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
  TxMeta
}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.AliasDoesNotExist
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.utils.ScorexLogging

import java.nio.charset.StandardCharsets
import scala.collection.mutable

class BlockchainStorage(val settings: BlockchainSettings, blockchainApi: BlockchainGrpcApi) extends ScorexLogging {
  private type TagT = Blockchain

  private val chainId = settings.addressSchemeCharacter.toByte

  val data = new RideData[(Address, String), DataEntry[?], TagT](Function.tupled(blockchainApi.getAccountDataEntry))

  val accountScripts = new RideData[Address, AccountScriptInfo, TagT](blockchainApi.getAccountScript(_, this.estimator))

  // It seems, we don't need to update this. Only for some optimization needs
//  private val blockHeaders = mutable.LongMap.empty[(SignedBlockHeader, ByteStr)]
  val blockHeaders = new ReadOnlyRideData[Integer, (SignedBlockHeader, ByteStr), TagT](height => )

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
  def replaceAssetDescription(update: StateUpdate.AssetDetails): Boolean = {
    val asset = update.assetId.toIssuedAsset
    assets.replaceIfExists(asset) { _ =>
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
  def replaceBalance(toReplace: StateUpdate.BalanceUpdate): Boolean = {
    val address = toReplace.address.toAddress
    portfolios.replaceIfExists(address) { prev =>
      val (asset, after) = toAssetAndAmount(toReplace.getAmountAfter)
      log.debug(s"[$address, $asset] Updated balance: $after")
      Some(asset match {
        case Asset.Waves        => prev.copy(balance = after)
        case asset: IssuedAsset => prev.copy(assets = prev.assets.updated(asset, after))
      })
    }
  }
  def replaceLeasing(toReplace: StateUpdate.LeasingUpdate): Boolean = {
    val address = toReplace.address.toAddress
    portfolios.replaceIfExists(address) { prev =>
      log.debug(s"[$address] Updated leasing")
      Some(prev.copy(lease = LeaseBalance(toReplace.inAfter, toReplace.outAfter)))
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
  def replaceTransactionMeta(pbId: ByteString, height: Int): Boolean = {
    val id = pbId.toByteStr
    transactions.replaceIfExists(id) { case ((_, tx)) =>
      log.debug(s"[$id] Updated transaction")
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
}
