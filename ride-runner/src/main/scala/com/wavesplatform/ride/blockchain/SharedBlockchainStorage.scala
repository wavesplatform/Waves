package com.wavesplatform.ride.blockchain

import cats.syntax.option.*
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.EstimatorProvider
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.ride.blockchain.caches.BlockchainCaches
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.{Height, TransactionId, TxMeta}
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.transaction.{EthereumTransaction, Transaction}
import com.wavesplatform.utils.ScorexLogging

import scala.util.chaining.scalaUtilChainingOps

class SharedBlockchainStorage[TagT](val settings: BlockchainSettings, caches: BlockchainCaches, blockchainApi: BlockchainGrpcApi)
    extends ScorexLogging {
  private val chainId = settings.addressSchemeCharacter.toByte

  val data = new AccountDataDataStorage[TagT](caches, blockchainApi)

  val accountScripts = new AccountScriptDataStorage[TagT](chainId, caches, blockchainApi, estimator)

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

  val assets = new AssetDataStorage[TagT](caches, blockchainApi)

  // It seems, we don't need to update this. Only for some optimization needs
  private val aliases = RideData.anyRefMap[Alias, Address, TagT] {
    load(caches.resolveAlias, blockchainApi.resolveAlias, caches.setAlias)
  }

  def getAlias(alias: Alias, tag: TagT): Option[Address] = aliases.get(alias, tag)

  val portfolios = new PortfolioDataStorage[TagT](caches, blockchainApi)

  private val transactions = RideData.anyRefMap[TransactionId, (TxMeta, Option[Transaction]), TagT] {
    load(caches.getTransaction, blockchainApi.getTransferLikeTransaction, caches.setTransaction)
  }

  def getTransaction(id: TransactionId, tag: TagT): Option[(TxMeta, Option[TransferTransactionLike])] =
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
  def appendTransactionMeta(height: Int, pbTxId: ByteString): AppendResult[TagT] = {
    val txId = TransactionId(pbTxId.toByteStr)
    transactions.replaceIfKnown(txId) { mayBeOrig =>
      log.debug(s"[$txId] Updated transaction")
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
        .tap(r => caches.setTransaction(txId, BlockchainData.loaded(r)))
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

case class AppendResult[TagT](mayBeChangedKey: Option[DataKey], affectedTags: Set[TagT])
object AppendResult {
  def appended[TagT](changedKey: DataKey, affectedTags: Set[TagT]): AppendResult[TagT] = new AppendResult[TagT](changedKey.some, affectedTags)
  def ignored[TagT]: AppendResult[TagT]                                                = new AppendResult[TagT](none, Set.empty)
}

case class RollbackResult[TagT](mayBeUncertainKey: Option[DataKey], affectedTags: Set[TagT])
object RollbackResult {
  def uncertain[TagT](uncertainKey: DataKey, affectedTags: Set[TagT]): RollbackResult[TagT] =
    new RollbackResult[TagT](uncertainKey.some, affectedTags)
  def ignored[TagT]: RollbackResult[TagT]                             = new RollbackResult[TagT](none, Set.empty)
  def rolledBack[TagT](affectedTags: Set[TagT]): RollbackResult[TagT] = new RollbackResult[TagT](none, affectedTags)
}
