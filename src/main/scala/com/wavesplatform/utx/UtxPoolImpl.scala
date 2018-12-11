package com.wavesplatform.utx

import java.util.concurrent.ConcurrentHashMap

import cats._
import com.wavesplatform.account.Address
import com.wavesplatform.mining.{MiningConstraint, MultiDimensionalMiningConstraint}
import com.wavesplatform.settings.UtxSettings
import com.wavesplatform.state.{ByteStr, Diff, Portfolio}
import com.wavesplatform.transaction.ValidationError.SenderIsBlacklisted
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.{ScorexLogging, Time}

import scala.collection.JavaConverters._
import scala.util.Right

class UtxPoolImpl(time: Time, utxSettings: UtxSettings) extends ScorexLogging with AutoCloseable with UtxPool {
  outer =>

  override def putIfNew(tx: Transaction): Either[ValidationError, (Boolean, Diff)] = {
    for {
      _ <- checkNotBlacklisted(tx)
      r = tx match {
        case stx: SignedTransaction =>
          val v = stx.signatureValid()
          if (v) log.info(s"UTX Transaction processed: ${stx.id.value.base58}")
          else
            log.warn(s"INVALID UTX transaction: ${stx.id.value.base58}")
          v
        case _ => false
      }
    } yield (r, Diff.empty)
  }

  private def checkNotBlacklisted(tx: Transaction): Either[ValidationError, Unit] = {
    if (utxSettings.blacklistSenderAddresses.isEmpty) {
      Right(())
    } else {
      val sender: Option[String] = tx match {
        case x: Authorized => Some(x.sender.address)
        case _             => None
      }

      sender match {
        case Some(addr) if utxSettings.blacklistSenderAddresses.contains(addr) =>
          val recipients = tx match {
            case tt: TransferTransaction      => Seq(tt.recipient)
            case mtt: MassTransferTransaction => mtt.transfers.map(_.address)
            case _                            => Seq()
          }
          val allowed =
            recipients.nonEmpty &&
              recipients.forall(r => utxSettings.allowBlacklistedTransferTo.contains(r.stringRepr))
          Either.cond(allowed, (), SenderIsBlacklisted(addr))
        case _ => Right(())
      }
    }
  }

  override def removeAll(txs: Traversable[Transaction]): Unit = {}

  override def accountPortfolio(addr: Address): Portfolio = Portfolio.empty

  override def portfolio(addr: Address): Portfolio = Portfolio.empty

  override def all: Seq[Transaction] = Seq.empty

  override def size: Int = 0

  override def transactionById(transactionId: ByteStr): Option[Transaction] = None

  override private[utx] def createBatchOps: UtxBatchOps = new BatchOpsImpl()

  private class BatchOpsImpl() extends UtxBatchOps {
    override def putIfNew(tx: Transaction): Either[ValidationError, (Boolean, Diff)] = outer.putIfNew(tx)
  }

  override def close(): Unit = {}

  override def packUnconfirmed(rest: MultiDimensionalMiningConstraint): (Seq[Transaction], MultiDimensionalMiningConstraint) = {
    (Seq(), MultiDimensionalMiningConstraint(MiningConstraint.Unlimited, MiningConstraint.Unlimited))
  }
}

object UtxPoolImpl {

  private class PessimisticPortfolios {
    private type Portfolios = Map[Address, Portfolio]
    private val transactionPortfolios = new ConcurrentHashMap[ByteStr, Portfolios]()
    private val transactions          = new ConcurrentHashMap[Address, Set[ByteStr]]()

    def add(txId: ByteStr, txDiff: Diff): Unit = {
      val nonEmptyPessimisticPortfolios = txDiff.portfolios
        .map {
          case (addr, portfolio) => addr -> portfolio.pessimistic
        }
        .filterNot {
          case (_, portfolio) => portfolio.isEmpty
        }

      if (nonEmptyPessimisticPortfolios.nonEmpty &&
          Option(transactionPortfolios.put(txId, nonEmptyPessimisticPortfolios)).isEmpty) {
        nonEmptyPessimisticPortfolios.keys.foreach { address =>
          transactions.put(address, transactions.getOrDefault(address, Set.empty) + txId)
        }
      }
    }

    def getAggregated(accountAddr: Address): Portfolio = {
      val portfolios = for {
        txId <- transactions.getOrDefault(accountAddr, Set.empty).toSeq
        txPortfolios = transactionPortfolios.getOrDefault(txId, Map.empty[Address, Portfolio])
        txAccountPortfolio <- txPortfolios.get(accountAddr).toSeq
      } yield txAccountPortfolio

      Monoid.combineAll[Portfolio](portfolios)
    }

    def remove(txId: ByteStr): Unit = {
      if (Option(transactionPortfolios.remove(txId)).isDefined) {
        transactions.keySet().asScala.foreach { addr =>
          transactions.put(addr, transactions.getOrDefault(addr, Set.empty) - txId)
        }
      }
    }
  }

}
