package com.wavesplatform.database

import java.io.IOException

import cats.effect.Resource
import com.google.common.primitives.{Ints, Shorts}
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.extensions.AddressTransactions
import com.wavesplatform.state.{AddressId, Height, TransactionId, TxNum}
import com.wavesplatform.transaction.Transaction.Type
import com.wavesplatform.transaction.{Transaction, TransactionParser, TransactionParsers}
import monix.eval.Task
import monix.reactive.Observable

import scala.util.Success
import scala.util.control.NonFatal

private[database] final class LevelDBWriterAddressTransactions(levelDBWriter: LevelDBWriter) extends AddressTransactions {

  import levelDBWriter.{dbSettings, readOnlyNoClose}

  override def addressTransactionsObservable(address: Address,
                                             types: Set[TransactionParser],
                                             fromId: Option[ByteStr]): Observable[(Height, Transaction)] = readOnlyNoClose { (snapshot, db) =>
    val maybeAfter = fromId.flatMap(id => db.get(Keys.transactionHNById(TransactionId(id))))

    db.get(Keys.addressId(address)).fold(Observable.empty[(Height, Transaction)]) { id =>
      val (heightAndTxs, closeF) = if (dbSettings.storeTransactionsByAddress) {
        def takeTypes(txNums: Iterator[(Height, Type, TxNum)], maybeTypes: Set[Type]) =
          if (maybeTypes.nonEmpty) txNums.filter { case (_, tp, _) => maybeTypes.contains(tp) } else txNums

        def takeAfter(txNums: Iterator[(Height, Type, TxNum)], maybeAfter: Option[(Height, TxNum)]) =
          maybeAfter match {
            case None => txNums
            case Some((filterHeight, filterNum)) =>
              txNums
                .dropWhile { case (streamHeight, _, _) => streamHeight > filterHeight }
                .dropWhile { case (streamHeight, _, streamNum) => streamNum >= filterNum && streamHeight >= filterHeight }
          }

        val addressId = AddressId(id)
        val heightNumStream = (db.get(Keys.addressTransactionSeqNr(addressId)) to 1 by -1).toIterator
          .flatMap(seqNr =>
            db.get(Keys.addressTransactionHN(addressId, seqNr)) match {
              case Some((height, txNums)) => txNums.map { case (txType, txNum) => (height, txType, txNum) }
              case None                   => Nil
          })

        (takeAfter(takeTypes(heightNumStream, types.map(_.typeId)), maybeAfter)
           .flatMap { case (height, _, txNum) => db.get(Keys.transactionAt(height, txNum)).map((height, txNum, _)) },
         () => ())
      } else {
        def takeAfter(txNums: Iterator[(Height, TxNum, Transaction)], maybeAfter: Option[(Height, TxNum)]) = maybeAfter match {
          case None => txNums
          case Some((filterHeight, filterNum)) =>
            txNums
              .dropWhile { case (streamHeight, _, _) => streamHeight > filterHeight }
              .dropWhile { case (streamHeight, streamNum, _) => streamNum >= filterNum && streamHeight >= filterHeight }
        }

        val (iter, close) = transactionsIterator(types.toVector, reverse = true)
        (takeAfter(iter, maybeAfter), close)
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
