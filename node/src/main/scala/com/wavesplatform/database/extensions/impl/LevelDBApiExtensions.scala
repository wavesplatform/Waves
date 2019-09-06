package com.wavesplatform.database.extensions.impl

import java.io.IOException

import cats.effect.Resource
import com.google.common.primitives.{Ints, Shorts}
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{Keys, LevelDBWriter, ReadOnlyDB}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.extensions.ApiExtensions
import com.wavesplatform.state.{AddressId, AssetDistribution, AssetDistributionPage, Height, Portfolio, TransactionId, TxNum}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Transaction.Type
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.{Transaction, TransactionParser, TransactionParsers}
import com.wavesplatform.utils.Paged
import monix.eval.Task
import monix.reactive.Observable

import scala.util.Success
import scala.util.control.NonFatal

final class LevelDBApiExtensions(ldb: LevelDBWriter) extends ApiExtensions {
  import LevelDBWriter._
  import com.wavesplatform.features.FeatureProvider.FeatureProviderExt
  import ldb._

  def portfolio(a: Address): Portfolio =
    readOnly { db =>
      def loadFullPortfolio(db: ReadOnlyDB, addressId: BigInt) = loadLposPortfolio(db, addressId).copy(
        assets = (for {
          asset <- db.get(Keys.assetList(addressId))
        } yield asset -> db.fromHistory(Keys.assetBalanceHistory(addressId, asset), Keys.assetBalance(addressId, asset)).getOrElse(0L)).toMap
      )

      def loadPortfolioWithoutNFT(db: ReadOnlyDB, addressId: AddressId) = loadLposPortfolio(db, addressId).copy(
        assets = (for {
          issuedAsset <- db.get(Keys.assetList(addressId))
          asset <- transactionInfo(issuedAsset.id).collect {
            case (_, it: IssueTransaction) if !it.isNFT => issuedAsset
          }
        } yield asset -> db.fromHistory(Keys.assetBalanceHistory(addressId, asset), Keys.assetBalance(addressId, asset)).getOrElse(0L)).toMap
      )

      val excludeNFT = ldb.isFeatureActivated(BlockchainFeatures.ReduceNFTFee, height)

      addressId(a).fold(Portfolio.empty) { addressId =>
        if (excludeNFT) loadPortfolioWithoutNFT(db, AddressId @@ addressId)
        else loadFullPortfolio(db, addressId)
      }
    }

  override def accountDataKeys(address: Address): Set[String] = readOnly { db =>
    (for {
      addressId <- addressId(address).toVector
      keyChunkCount = db.get(Keys.dataKeyChunkCount(addressId))
      chunkNo <- Range(0, keyChunkCount)
      key     <- db.get(Keys.dataKeyChunk(addressId, chunkNo))
    } yield key).toSet
  }

  def nftObservable(address: Address, from: Option[IssuedAsset]): Observable[IssueTransaction] = {
    def openIterator() = readOnlyNoClose { (snapshot, db) =>
      def issueTxIterator = {
        val assetIds = db
          .get(Keys.addressId(address))
          .fold(Seq.empty[IssuedAsset]) { id =>
            val addressId = AddressId @@ id
            db.get(Keys.assetList(addressId))
          }

        assetIds.iterator
          .flatMap(ia => transactionInfo(ia.id).map(_._2))
          .collect {
            case itx: IssueTransaction if itx.isNFT => itx
          }
      }

      val result = from
        .flatMap(ia => transactionInfo(ia.id))
        .fold(issueTxIterator) {
          case (_, afterTx) =>
            issueTxIterator
              .dropWhile(_.id() != afterTx.id())
              .drop(1)
        }

      (result, snapshot)
    }

    val resource = Resource(Task {
      val (iter, snapshot) = openIterator()
      (iter, Task(snapshot.close()))
    })
    Observable.fromIterator(resource)
  }

  override def assetDistribution(asset: IssuedAsset): AssetDistribution = readOnly { db =>
    val dst = (for {
      seqNr     <- (1 to db.get(Keys.addressesForAssetSeqNr(asset))).par
      addressId <- db.get(Keys.addressesForAsset(asset, seqNr)).par
      actualHeight <- db
        .get(Keys.assetBalanceHistory(addressId, asset))
        .filterNot(_ > height)
        .headOption
      balance = db.get(Keys.assetBalance(addressId, asset)(actualHeight))
      if balance > 0
    } yield db.get(Keys.idToAddress(addressId)) -> balance).toMap.seq

    AssetDistribution(dst)
  }

  override def assetDistributionAtHeight(
      asset: IssuedAsset,
      height: Int,
      count: Int,
      fromAddress: Option[Address]
  ): Either[ValidationError, AssetDistributionPage] = readOnly { db =>
    val canGetAfterHeight = db.get(Keys.safeRollbackHeight)

    lazy val maybeAddressId = fromAddress.flatMap(addr => db.get(Keys.addressId(addr)))

    def takeAfter(s: Seq[BigInt], a: Option[BigInt]): Seq[BigInt] = {
      a match {
        case None    => s
        case Some(v) => s.dropWhile(_ != v).drop(1)
      }
    }

    lazy val addressIds: Seq[BigInt] = {
      val all = for {
        seqNr <- 1 to db.get(Keys.addressesForAssetSeqNr(asset))
        addressId <- db
          .get(Keys.addressesForAsset(asset, seqNr))
      } yield addressId

      takeAfter(all, maybeAddressId)
    }

    lazy val distribution: Stream[(Address, Long)] =
      for {
        addressId <- addressIds.toStream
        history = db.get(Keys.assetBalanceHistory(addressId, asset))
        actualHeight <- history.filterNot(_ > height).headOption
        balance = db.get(Keys.assetBalance(addressId, asset)(actualHeight))
        if balance > 0
      } yield db.get(Keys.idToAddress(addressId)) -> balance

    lazy val page: AssetDistributionPage = {
      val dst = distribution.take(count + 1)

      val hasNext = dst.length > count
      val items   = if (hasNext) dst.init else dst
      val lastKey = items.lastOption.map(_._1)

      val result: Paged[Address, AssetDistribution] =
        Paged(hasNext, lastKey, AssetDistribution(items.toMap))

      AssetDistributionPage(result)
    }

    Either
      .cond(
        height > canGetAfterHeight,
        page,
        GenericError(s"Cannot get asset distribution at height less than ${canGetAfterHeight + 1}")
      )
  }

  override def wavesDistribution(height: Int): Either[ValidationError, Map[Address, Long]] = readOnly { db =>
    val canGetAfterHeight = db.get(Keys.safeRollbackHeight)

    def createMap() =
      (for {
        seqNr     <- (1 to db.get(Keys.addressesForWavesSeqNr)).par
        addressId <- db.get(Keys.addressesForWaves(seqNr)).par
        history = db.get(Keys.wavesBalanceHistory(addressId))
        actualHeight <- history.partition(_ > height)._2.headOption
        balance = db.get(Keys.wavesBalance(addressId)(actualHeight))
        if balance > 0
      } yield db.get(Keys.idToAddress(addressId)) -> balance).toMap.seq

    Either.cond(
      height > canGetAfterHeight,
      createMap(),
      GenericError(s"Cannot get waves distribution at height less than ${canGetAfterHeight + 1}")
    )
  }

  override def addressTransactionsObservable(
      address: Address,
      types: Set[TransactionParser],
      fromId: Option[ByteStr]
  ): Observable[(Height, Transaction)] = readOnlyNoClose { (snapshot, db) =>
    val maybeAfter = fromId.flatMap(id => db.get(Keys.transactionHNById(TransactionId(id))))

    def takeAfter[T](txRefs: Iterator[T])(getHeight: T => Height, getNum: T => TxNum): Iterator[T] =
      maybeAfter match {
        case None => txRefs
        case Some((filterHeight, filterNum)) =>
          txRefs
            .dropWhile(v => getHeight(v) > filterHeight)
            .dropWhile(v => getNum(v) >= filterNum && getHeight(v) >= filterHeight)
      }

    def readOptimized(addressId: AddressId): Iterator[(Height, TxNum, Transaction)] = {
      def takeTypes(txRefs: Iterator[(Height, TxNum, Type)], types: Set[Type]) =
        if (types.nonEmpty) txRefs.filter { case (_, _, tp) => types.contains(tp) } else txRefs

      val heightNumStream = for {
        seqNr            <- (db.get(Keys.addressTransactionSeqNr(addressId)) to 1 by -1).toIterator
        (height, txNums) <- db.get(Keys.addressTransactionHN(addressId, seqNr)).toIterable
        (txType, txNum)  <- txNums
      } yield (height, txNum, txType)

      takeAfter(takeTypes(heightNumStream, types.map(_.typeId)))(_._1, _._3)
        .flatMap { case (height, _, txNum) => db.get(Keys.transactionAt(height, txNum)).map((height, txNum, _)) }
    }

    db.get(Keys.addressId(address)).map(AddressId @@ _) match {
      case Some(addressId) =>
        val (heightAndTxs, closeF) = if (dbSettings.storeTransactionsByAddress) {
          (readOptimized(addressId), () => ())
        } else {
          val (iter, close) = transactionsIterator(types.toVector, reverse = true)
          (takeAfter(iter)(_._1, _._2), close)
        }

        val resource = Resource(Task((heightAndTxs.map { case (height, _, tx) => (height, tx) }, Task {
          closeF()
          snapshot.close()
        })))

        Observable.fromIterator(resource)
    }
  }

  private[this] def transactionsIterator(ofTypes: Seq[TransactionParser], reverse: Boolean): (Iterator[(Height, TxNum, Transaction)], () => Unit) =
    readOnlyNoClose { (snapshot, db) =>
      def iterateOverStream(prefix: Array[Byte]) = {
        import scala.collection.JavaConverters._
        val dbIter = db.iterator
        try {
          dbIter.seek(prefix)
          (dbIter.asScala.takeWhile(_.getKey.startsWith(prefix)), { () =>
            dbIter.close()
            snapshot.close()
          })
        } catch {
          case NonFatal(err) =>
            dbIter.close()
            throw new IOException("Couldn't create DB iterator", err)
        }
      }

      val (iter, close) = iterateOverStream(Shorts.toByteArray(Keys.TransactionHeightNumByIdPrefix))

      val result = iter
        .map { kv =>
          val heightNumBytes = kv.getValue

          val height = Height(Ints.fromByteArray(heightNumBytes.take(4)))
          val txNum  = TxNum(Shorts.fromByteArray(heightNumBytes.takeRight(2)))

          (height, txNum)
        }
        .flatMap {
          case (height, txNum) =>
            db.get(Keys.transactionBytesAt(height, txNum))
              .flatMap { txBytes =>
                if (ofTypes.isEmpty)
                  TransactionParsers.parseBytes(txBytes).toOption
                else {
                  ofTypes.iterator
                    .map(_.parseBytes(txBytes))
                    .collectFirst { case Success(tx) => tx }
                }
              }
              .map((height, txNum, _))
        }

      (result, close)
    }
}
