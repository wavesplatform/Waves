package com.wavesplatform.state2.reader

import com.google.common.base.Charsets
import com.wavesplatform.state2._
import scorex.account.{Account, AccountOrAlias, Alias}
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.ValidationError.AliasNotExists
import scorex.transaction._
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order}
import scorex.transaction.lease.LeaseTransaction
import scorex.utils.{ScorexLogging, SynchronizedOver}

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Right

trait StateReader extends SynchronizedOver {

  def accountPortfolios: Map[Account, Portfolio]

  def transactionInfo(id: ByteArray): Option[(Int, Transaction)]

  def containsTransaction(id: ByteArray): Boolean

  def accountPortfolio(a: Account): Portfolio

  def assetInfo(id: ByteArray): Option[AssetInfo]

  def height: Int

  def accountTransactionIds(a: Account): Seq[ByteArray]

  def paymentTransactionIdByHash(hash: ByteArray): Option[ByteArray]

  def aliasesOfAddress(a: Account): Seq[Alias]

  def resolveAlias(a: Alias): Option[Account]

  def findPreviousExchangeTxs(orderId: EqByteArray): Set[ExchangeTransaction]

  def isLeaseActive(leaseTx: LeaseTransaction): Boolean

  def getAssetIdByUniqueName(assetName: ByteArray): Option[ByteArray]

  def activeLeases(): Seq[ByteArray]

  def lastUpdateHeight(acc: Account): Option[Int]

  def snapshotAtHeight(acc: Account, h: Int): Option[Snapshot]
}

object StateReader {

  implicit class StateReaderExt(s: StateReader) extends ScorexLogging {
    def assetDistribution(assetId: ByteArray): Map[Account, Long] =
      s.accountPortfolios
        .mapValues(portfolio => portfolio.assets.get(assetId))
        .collect { case (acc, Some(amt)) => acc -> amt }

    def findTransaction[T <: Transaction](signature: Array[Byte])(implicit ct: ClassTag[T]): Option[T]
    = s.transactionInfo(EqByteArray(signature)).map(_._2)
      .flatMap(tx => {
        if (ct.runtimeClass.isAssignableFrom(tx.getClass))
          Some(tx.asInstanceOf[T])
        else None
      })

    def resolveAliasEi[T <: Transaction](aoa: AccountOrAlias): Either[StateValidationError, Account] = {
      aoa match {
        case a: Account => Right(a)
        case a: Alias => s.resolveAlias(a) match {
          case None => Left(AliasNotExists(a))
          case Some(acc) => Right(acc)
        }
      }
    }

    def findPreviousExchangeTxs(order: Order): Set[ExchangeTransaction] =
      s.findPreviousExchangeTxs(EqByteArray(order.id))

    def included(signature: Array[Byte]): Option[Int] = s.transactionInfo(EqByteArray(signature)).map(_._1)

    def accountTransactions(account: Account, limit: Int): Seq[_ <: Transaction] = s.read { implicit l =>
      s.accountTransactionIds(account).take(limit).flatMap(s.transactionInfo).map(_._2)
    }

    def balance(account: Account): Long = s.accountPortfolio(account).balance

    def assetBalance(account: AssetAcc): Long = {
      val accountPortfolio = s.accountPortfolio(account.account)
      account.assetId match {
        case Some(assetId) => accountPortfolio.assets.getOrElse(EqByteArray(assetId), 0)
        case None => accountPortfolio.balance
      }
    }

    def getAccountBalance(account: Account): Map[AssetId, (Long, Boolean, Long, IssueTransaction)] = s.read { implicit l =>
      s.accountPortfolio(account).assets.map { case (id, amt) =>
        val assetInfo = s.assetInfo(id).get
        id.arr -> (amt, assetInfo.isReissuable, assetInfo.volume, findTransaction[IssueTransaction](id.arr).get)
      }
    }

    def assetDistribution(assetId: Array[Byte]): Map[String, Long] =
      s.assetDistribution(EqByteArray(assetId))
        .map { case (acc, amt) => (acc.address, amt) }

    def effectiveBalance(account: Account): Long = s.accountPortfolio(account).effectiveBalance

    def isReissuable(id: Array[Byte]): Boolean =
      s.assetInfo(EqByteArray(id)).get.isReissuable

    def totalAssetQuantity(assetId: AssetId): Long =
      s.assetInfo(EqByteArray(assetId)).get.volume

    def getAssetName(assetId: AssetId): String = {
      s.findTransaction[IssueTransaction](assetId)
        .map(tx => new String(tx.name, Charsets.UTF_8))
        .getOrElse("Unknown")
    }

    def stateHash(): Int = (BigInt(FastCryptographicHash(s.accountPortfolios.toString().getBytes)) % Int.MaxValue).toInt

    private def minBySnapshot(acc: Account, atHeight: Int, confirmations: Int)(extractor: Snapshot => Long): Long = s.read { implicit l =>
      val bottomNotIncluded = atHeight - confirmations

      @tailrec
      def loop(deeperHeight: Int, list: Seq[Snapshot]): Seq[Snapshot] = {
        if (deeperHeight == 0) list
        else {
          val snapshot = s.snapshotAtHeight(acc, deeperHeight).get
          if (deeperHeight <= bottomNotIncluded)
            snapshot +: list
          else if (deeperHeight > atHeight && snapshot.prevHeight > atHeight) {
            loop(snapshot.prevHeight, list)
          } else
            loop(snapshot.prevHeight, snapshot +: list)
        }
      }

      val snapshots: Seq[Snapshot] = s.lastUpdateHeight(acc) match {
        case None => Seq(Snapshot(0, 0, 0))
        case Some(h) if h < atHeight - confirmations =>
          val pf = s.accountPortfolio(acc)
          Seq(Snapshot(h, pf.balance, pf.effectiveBalance))
        case Some(h) => loop(h, Seq.empty)
      }

      snapshots.map(extractor).min
    }

    def effectiveBalanceAtHeightWithConfirmations(acc: Account, atHeight: Int, confirmations: Int): Long =
      minBySnapshot(acc, atHeight, confirmations)(_.effectiveBalance)

    def balanceWithConfirmations(acc: Account, confirmations: Int): Long =
      minBySnapshot(acc, s.height, confirmations)(_.balance)

    def balanceAtHeight(acc: Account, height: Int): Long = s.read { implicit l =>

      @tailrec
      def loop(lookupHeight: Int): Long = s.snapshotAtHeight(acc, lookupHeight) match {
        case None if lookupHeight == 0 => 0
        case Some(snapshot) if lookupHeight <= height => snapshot.balance
        case Some(snapshot) => loop(snapshot.prevHeight)
        case None =>
          throw new Exception(s"Cannot lookup account $acc for height $height(current=${s.height}). " +
            s"No history found at requested lookupHeight=$lookupHeight")
      }
      loop(s.lastUpdateHeight(acc).getOrElse(0))
    }
  }

}
