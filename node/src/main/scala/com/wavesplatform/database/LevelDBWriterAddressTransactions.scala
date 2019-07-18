package com.wavesplatform.database

import com.google.common.primitives.{Ints, Shorts}
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.extensions.AddressTransactions
import com.wavesplatform.state.{AddressId, Height, TransactionId, TxNum}
import com.wavesplatform.transaction.Transaction.Type
import com.wavesplatform.transaction.{Transaction, TransactionParser, TransactionParsers}
import com.wavesplatform.utils.CloseableIterator

import scala.util.Success

private[database] final class LevelDBWriterAddressTransactions(levelDBWriter: LevelDBWriter) extends AddressTransactions {
  import levelDBWriter.{dbSettings, writableDB}

  override def addressTransactionsIterator(address: Address,
                                           types: Set[TransactionParser],
                                           fromId: Option[ByteStr]): CloseableIterator[(Height, Transaction)] = writableDB.readOnlyStream { db =>
    val maybeAfter = fromId.flatMap(id => db.get(Keys.transactionHNById(TransactionId(id))))

    db.get(Keys.addressId(address)).fold(CloseableIterator.empty[(Height, Transaction)]) { id =>
      val heightAndTxs: CloseableIterator[(Height, TxNum, Transaction)] = if (dbSettings.storeTransactionsByAddress) {
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

        CloseableIterator.fromIterator(takeAfter(takeTypes(heightNumStream, types.map(_.typeId)), maybeAfter))
          .flatMap { case (height, _, txNum) => db.get(Keys.transactionAt(height, txNum)).map((height, txNum, _)) }
      } else {
        def takeAfter(txNums: Iterator[(Height, TxNum, Transaction)], maybeAfter: Option[(Height, TxNum)]) = maybeAfter match {
          case None => txNums
          case Some((filterHeight, filterNum)) =>
            txNums
              .dropWhile { case (streamHeight, _, _) => streamHeight > filterHeight }
              .dropWhile { case (streamHeight, streamNum, _) => streamNum >= filterNum && streamHeight >= filterHeight }
        }

        transactionsIterator(types.toVector, reverse = true)
          .transform(takeAfter(_, maybeAfter))
      }

      heightAndTxs
        .transform(_.map { case (height, _, tx) => (height, tx) })
    }
  }

  private[this] def transactionsIterator(ofTypes: Seq[TransactionParser], reverse: Boolean): CloseableIterator[(Height, TxNum, Transaction)] =
    writableDB.readOnlyStream { db =>
      val baseIterator: CloseableIterator[(Height, TxNum)] =
        if (reverse) {
          for {
            height  <- CloseableIterator.fromIterator((levelDBWriter.height to 0 by -1).iterator)
            (bh, _) <- levelDBWriter.blockHeaderAndSize(height).iterator
            txNum   <- bh.transactionCount to 0 by -1
          } yield (Height(height), TxNum(txNum.toShort))
        } else {
          db.iterateOverStream(Keys.TransactionHeightNumByIdPrefix)
            .transform(_.map { kv =>
              val heightNumBytes = kv.getValue

              val height = Height(Ints.fromByteArray(heightNumBytes.take(4)))
              val txNum  = TxNum(Shorts.fromByteArray(heightNumBytes.takeRight(2)))

              (height, txNum)
            })
        }

      baseIterator
        .transform(_.flatMap {
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
        })
    }
}
