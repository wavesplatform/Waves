package com.wavesplatform

import java.util.concurrent.ConcurrentHashMap

import cats.Monoid
import com.google.common.cache.CacheBuilder
import com.wavesplatform.network._
import com.wavesplatform.settings.{FunctionalitySettings, UtxSettings}
import com.wavesplatform.state2.diffs.TransactionDiffer
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import com.wavesplatform.state2.{ByteStr, Diff}
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import scorex.consensus.TransactionsOrdering
import scorex.transaction.ValidationError.{AlreadyInThePool, GenericError}
import scorex.transaction.{FeeCalculator, Transaction, ValidationError}
import scorex.utils.{ScorexLogging, Time}

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.{Left, Right}


class UtxPool(
    allChannels: ChannelGroup,
    time: Time,
    stateReader: StateReader,
    feeCalculator: FeeCalculator,
    fs: FunctionalitySettings,
    utxSettings: UtxSettings) extends ScorexLogging {

  private val transactions = new ConcurrentHashMap[ByteStr, Transaction]
  private lazy val knownTransactions = CacheBuilder.newBuilder()
    .maximumSize(utxSettings.maxSize * 2)
    .build[ByteStr, Either[ValidationError, Transaction]]()

  private def removeExpired(currentTs: Long): Unit =
    transactions.entrySet().removeIf(tx => (currentTs - tx.getValue.timestamp).millis > utxSettings.maxTransactionAge)

  def putIfNew(tx: Transaction, source: Option[Channel] = None): Either[ValidationError, Transaction] =
    if (transactions.size >= utxSettings.maxSize) {
      Left(GenericError("Transaction pool size limit is reached"))
    } else knownTransactions.get(tx.id, () => {
      val validationResult = for {
        _ <- feeCalculator.enoughFee(tx)
        _ <- TransactionDiffer.apply(fs, time.correctedTime(), stateReader.height)(stateReader, tx)
        _ = transactions.putIfAbsent(tx.id, tx)
        _ = allChannels.broadcast(RawBytes(TransactionMessageSpec.messageCode, tx.bytes), source)
      } yield tx

      validationResult
  })

  def removeAll(tx: Traversable[Transaction]): Unit = {
    removeExpired(time.correctedTime())
    tx.view.map(_.id).foreach { id =>
      knownTransactions.invalidate(id)
      transactions.remove(id)
    }
  }

  def all(): Seq[Transaction] = transactions.values.asScala.toSeq.sorted(TransactionsOrdering.InUTXPool)

  def packUnconfirmed(): Seq[Transaction] = {
    val currentTs = time.correctedTime()
    removeExpired(currentTs)
    val differ = TransactionDiffer.apply(fs, currentTs, stateReader.height) _
    val (invalidTxs, validTxs, _) = transactions.asScala
      .values.toSeq
      .sorted(TransactionsOrdering.InUTXPool)
      .foldLeft((Seq.empty[ByteStr], Seq.empty[Transaction], Monoid[Diff].empty)) {
        case ((invalid, valid, diff), tx) if valid.size < 100 =>
          differ(new CompositeStateReader(stateReader, diff.asBlockDiff), tx) match {
            case Right(newDiff) =>
              (invalid, tx +: valid, Monoid.combine(diff, newDiff))
            case Left(e) =>
              log.debug(s"Removing invalid transaction ${tx.id} from UTX: $e")
              (tx.id +: invalid, valid, diff)
          }
        case (r, _) => r
      }

    invalidTxs.foreach(transactions.remove)
    validTxs.sorted(TransactionsOrdering.InBlock)
  }
}
