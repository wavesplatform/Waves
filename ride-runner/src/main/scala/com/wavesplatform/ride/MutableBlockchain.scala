package com.wavesplatform.ride

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.EstimatorProvider.EstimatorBlockchainExt
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.lang.ValidationError
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
  LeaseBalance,
  Portfolio,
  TxMeta,
  VolumeAndFee
}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.AliasDoesNotExist
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.transaction.{Asset, ERC20Address, Transaction}

import scala.collection.mutable

class MutableBlockchain(override val settings: BlockchainSettings, blockchainApi: BlockchainGrpcApi) extends Blockchain {
  private def kill(methodName: String) = throw new RuntimeException(methodName)

  private val data = mutable.AnyRefMap.empty[Address, mutable.AnyRefMap[String, CachedData[DataEntry[_]]]]

  // TODO use utils/evaluate through REST API
  // Ride: isDataStorageUntouched
  override def hasData(address: Address): Boolean = data.contains(address)
  def putHasData(address: Address): Unit          = {}

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

  def putAccountData(acc: Address, key: String, data: Option[DataEntry[_]]): Unit = {

  }

  // None means "we don't know". Some(None) means "we know, there is no script"
  private val accountScripts = mutable.AnyRefMap.empty[Address, CachedData[AccountScriptInfo]]
  private def withAccountScript(address: Address): Option[CachedData[AccountScriptInfo]] =
    accountScripts.updateWith(address) {
      case None => Some(CachedData.loaded(blockchainApi.getAccountScript(address, this.estimator)))
      case x    => x
    }

  // Ride: scriptHash
  override def accountScript(address: Address): Option[AccountScriptInfo] = withAccountScript(address).get.mayBeValue

  override def hasAccountScript(address: Address) = withAccountScript(address).get.mayBeValue.nonEmpty

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
  var _height: Int = blockchainApi.getCurrentBlockchainHeight()

  override def height: Int = _height

  var _activatedFeatures = blockchainApi.getActivatedFeatures(height)

  override def activatedFeatures: Map[Short, Int] = _activatedFeatures

  private val assets = mutable.AnyRefMap.empty[IssuedAsset, AssetDescription]
  // Ride: assetInfo
  override def assetDescription(id: Asset.IssuedAsset): Option[AssetDescription] =
    assets.updateWith(id) {
      case None => blockchainApi.getAssetDescription(id)
      case x    => x
    }

  // Ride (indirectly): asset script validation
  override def assetScript(id: Asset.IssuedAsset): Option[AssetScriptInfo] = assetDescription(id).flatMap(_.script)

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

  private val transactions = mutable.AnyRefMap.empty[ByteStr, (TxMeta, Option[TransferTransactionLike])]
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
