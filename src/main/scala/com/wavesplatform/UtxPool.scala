package com.wavesplatform

import java.util.concurrent.ConcurrentHashMap

import cats._
import com.google.common.cache.CacheBuilder
import com.wavesplatform.settings.{FunctionalitySettings, UtxSettings}
import com.wavesplatform.state2.diffs.TransactionDiffer
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import com.wavesplatform.state2.{ByteStr, Diff, Portfolio}
import scorex.account.Address
import scorex.consensus.TransactionsOrdering
import scorex.transaction.ValidationError.GenericError
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.{Left, Right}

/**
  * A pool with unconfirmed transactions
  */
class UtxPool(
                 time: Time,
                 stateReader: StateReader,
                 history: History,
                 feeCalculator: FeeCalculator,
                 fs: FunctionalitySettings,
                 utxSettings: UtxSettings) extends ScorexLogging {

  private type Portfolios = Map[Address, Portfolio]

  private val transactions = new ConcurrentHashMap[ByteStr, Transaction]

  private lazy val knownTransactions = CacheBuilder.newBuilder()
    .maximumSize(utxSettings.maxSize * 2)
    .build[ByteStr, Either[ValidationError, Transaction]]()

  /**
    * Transaction.id -> diff
    * Transactions which withdraw
    */
  private val txPessimisticPortfolios = new ConcurrentHashMap[ByteStr, Portfolios]

  private def removeExpired(currentTs: Long): Unit = {
    def isExpired(tx: Transaction) = (currentTs - tx.timestamp).millis > utxSettings.maxTransactionAge

    if (!transactions.isEmpty) {
      val expiredTxs = transactions.reduceValues[Vector[Transaction]](
        1, // maximal parallelism
        { tx => if (isExpired(tx)) Vector(tx) else Vector.empty },
        _ ++ _
      )

      expiredTxs.foreach { tx =>
        transactions.remove(tx.id)
        txPessimisticPortfolios.remove(tx.id)
      }
    }
  }

  def putIfNew(tx: Transaction): Either[ValidationError, Transaction] =
    if (transactions.size >= utxSettings.maxSize) {
      Left(GenericError("Transaction pool size limit is reached"))
    } else knownTransactions.get(tx.id, () => {
      val validationResult = for {
        _ <- feeCalculator.enoughFee(tx)
        diff <- TransactionDiffer(fs, history.lastBlock.map(_.timestamp), time.correctedTime(), stateReader.height)(stateReader, tx)
        _ = {
          val nonEmptyPessimisticPortfolios = diff.portfolios
            .mapValues(_.pessimistic)
            .filterNot {
              case (_, portfolio) => portfolio.isEmpty
            }

          if (nonEmptyPessimisticPortfolios.nonEmpty) {
            txPessimisticPortfolios.put(tx.id, nonEmptyPessimisticPortfolios)
          }
        }
        _ = transactions.putIfAbsent(tx.id, tx)
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

  def portfolio(addr: Address): Portfolio = {
    val empty = Monoid.empty[Portfolio]
    val base = stateReader.accountPortfolio(addr)

    val foundInUtx = if (txPessimisticPortfolios.isEmpty) {
      empty
    } else {
      txPessimisticPortfolios.reduceValues[Portfolio](
        1,
        _.getOrElse(addr, empty),
        Monoid.combine[Portfolio]
      )
    }

    Monoid.combine(base, foundInUtx)
  }

  def all(): Seq[Transaction] = transactions.values.asScala.toSeq.sorted(TransactionsOrdering.InUTXPool)

  def size: Int = transactions.size()

  def transactionById(transactionId: ByteStr): Option[Transaction] = Option(transactions.get(transactionId))

  def packUnconfirmed(): Seq[Transaction] = {
    val currentTs = time.correctedTime()
    removeExpired(currentTs)
    val differ = TransactionDiffer.apply(fs, history.lastBlock.map(_.timestamp), currentTs, stateReader.height) _
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
