package com.wavesplatform.state2.reader

import com.google.common.base.Charsets
import com.wavesplatform.state2._
import scorex.account.{Address, AddressOrAlias, Alias}
import scorex.transaction.DataTransaction.DataItem
import scorex.transaction.ValidationError.AliasNotExists
import scorex.transaction._
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.smart.Script
import scorex.utils.{ScorexLogging, Synchronized}

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.{Right, Try}

trait SnapshotStateReader extends Synchronized {

  def accountPortfolios: Map[Address, Portfolio]

  def transactionInfo(id: ByteStr): Option[(Int, Option[Transaction])]

  def containsTransaction(id: ByteStr): Boolean

  def accountPortfolio(a: Address): Portfolio

  def assetInfo(id: ByteStr): Option[AssetInfo]

  def wavesBalance(a: Address): (Long, LeaseInfo)

  def assetBalance(a: Address, asset: ByteStr): Long

  def height: Int

  def accountTransactionIds(a: Address, limit: Int): Seq[ByteStr]

  def aliasesOfAddress(a: Address): Seq[Alias]

  def resolveAlias(a: Alias): Option[Address]

  def isLeaseActive(leaseTx: LeaseTransaction): Boolean

  def activeLeases(): Seq[ByteStr]

  def lastUpdateHeight(acc: Address): Option[Int]

  def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot]

  def filledVolumeAndFee(orderId: ByteStr): OrderFillInfo

  def accountScript(address: Address): Option[Script]

  def accountData(acc: Address): AccountDataInfo

  def accountData(acc: Address, key: String): Option[DataItem[_]]
}

object SnapshotStateReader {

  implicit class StateReaderExt(s: SnapshotStateReader) extends ScorexLogging {
    def assetDistribution(assetId: ByteStr): Map[Address, Long] =
      s.accountPortfolios
        .mapValues(portfolio => portfolio.assets.get(assetId))
        .collect { case (acc, Some(amt)) => acc -> amt }

    def findTransaction[T <: Transaction : ClassTag](signature: ByteStr): Option[T] =
      s.transactionInfo(signature) match {
        case Some((_, Some(t: T))) => Some(t)
        case _ => None
      }

    def resolveAliasEi[T <: Transaction](aoa: AddressOrAlias): Either[ValidationError, Address] = {
      aoa match {
        case a: Address => Right(a)
        case a: Alias => s.resolveAlias(a) match {
          case None => Left(AliasNotExists(a))
          case Some(acc) => Right(acc)
        }
      }
    }

    def included(signature: ByteStr): Option[Int] = s.transactionInfo(signature).map(_._1)

    def accountTransactions(account: Address, limit: Int): Seq[(Int, _ <: Transaction)] = s.read { _ =>
      s.accountTransactionIds(account, limit)
        .flatMap(s.transactionInfo)
        .flatMap { case (h, txopt) => txopt.map((h, _)) }
    }

    def balance(account: Address): Long = s.wavesBalance(account)._1


    def getAccountBalance(account: Address): Map[AssetId, (Long, Boolean, Long, IssueTransaction)] = s.read { _ =>
      s.accountPortfolio(account).assets.collect { case (id, amt) if amt > 0 =>
        val assetInfo = s.assetInfo(id).get
        val issueTransaction = findTransaction[IssueTransaction](id).get
        id -> ((amt, assetInfo.isReissuable, assetInfo.volume, issueTransaction))
      }
    }

    def assetDistribution(assetId: Array[Byte]): Map[String, Long] =
      s.assetDistribution(ByteStr(assetId))
        .map { case (acc, amt) => (acc.address, amt) }

    def effectiveBalance(account: Address): Long = s.partialPortfolio(account).effectiveBalance

    def spendableBalance(assetAcc: AssetAcc): Long = {
      val accountPortfolio = s.partialPortfolio(assetAcc.account, assetAcc.assetId.toSet)
      assetAcc.assetId match {
        case Some(assetId) => accountPortfolio.assets.getOrElse(assetId, 0)
        case None => accountPortfolio.spendableBalance
      }
    }

    def isReissuable(id: Array[Byte]): Boolean =
      s.assetInfo(ByteStr(id)).get.isReissuable

    def totalAssetQuantity(assetId: AssetId): Long =
      s.assetInfo(assetId).get.volume

    def assetExists(assetId: AssetId): Boolean = {
      s.findTransaction[IssueTransaction](assetId).nonEmpty
    }

    def getAssetName(assetId: AssetId): String = {
      s.findTransaction[IssueTransaction](assetId)
        .map(tx => new String(tx.name, Charsets.UTF_8))
        .getOrElse("Unknown")
    }

    def getIssueTransaction(assetId: AssetId): Option[IssueTransaction] = {
      s.findTransaction[IssueTransaction](assetId)
    }

    private def minBySnapshot(acc: Address, atHeight: Int, confirmations: Int)(extractor: Snapshot => Long): Long = s.read { _ =>
      val bottomNotIncluded = atHeight - confirmations

      @tailrec
      def loop(deeperHeight: Int, list: Seq[Snapshot]): Seq[Snapshot] = {
        if (deeperHeight == 0) {
          s.snapshotAtHeight(acc, 1) match {
            case Some(genesisSnapshot) =>
              genesisSnapshot +: list
            case None =>
              Snapshot(0, 0, 0) +: list
          }
        } else {
          s.snapshotAtHeight(acc, deeperHeight) match {
            case Some(snapshot) =>
              if (deeperHeight <= bottomNotIncluded)
                snapshot +: list
              else if (snapshot.prevHeight == deeperHeight) {
                throw new Exception(s"CRITICAL: Infinite loop detected while calculating minBySnapshot: acc=$acc, atHeight=$atHeight, " +
                  s"confirmations=$confirmations; lastUpdateHeight=${s.lastUpdateHeight(acc)}; current step: deeperHeight=$deeperHeight, list.size=${list.size}")
              } else if (deeperHeight > atHeight && snapshot.prevHeight > atHeight) {
                loop(snapshot.prevHeight, list)
              } else
                loop(snapshot.prevHeight, snapshot +: list)
            case None =>
              throw new Exception(s"CRITICAL: Cannot lookup referenced height: acc=$acc, atHeight=$atHeight, " +
                s"confirmations=$confirmations; lastUpdateHeight=${s.lastUpdateHeight(acc)}; current step: deeperHeight=$deeperHeight, list.size=${list.size}")
          }
        }
      }

      val snapshots: Seq[Snapshot] = s.lastUpdateHeight(acc) match {
        case None => Seq(Snapshot(0, 0, 0))
        case Some(h) if h < bottomNotIncluded =>
          val pf = s.partialPortfolio(acc)
          Seq(Snapshot(h, pf.balance, pf.effectiveBalance))
        case Some(h) => loop(h, Seq.empty)
      }

      snapshots.map(extractor).min
    }

    def effectiveBalanceAtHeightWithConfirmations(acc: Address, atHeight: Int, confirmations: Int): Try[Long] = Try {
      minBySnapshot(acc, atHeight, confirmations)(_.effectiveBalance)
    }

    def balanceWithConfirmations(acc: Address, confirmations: Int): Long =
      minBySnapshot(acc, s.height, confirmations)(_.balance)

    def balanceAtHeight(acc: Address, height: Int): Long = s.read { _ =>
      @tailrec
      def loop(lookupHeight: Int): Long = s.snapshotAtHeight(acc, lookupHeight) match {
        case None if lookupHeight == 0 => 0
        case Some(snapshot) if lookupHeight <= height => snapshot.balance
        case Some(snapshot) if snapshot.prevHeight == lookupHeight =>
          throw new Exception(s"CRITICAL: Cannot lookup account $acc for height $height(current=${s.height}). Infinite loop detected. " +
            s"This indicates snapshots processing error.")
        case Some(snapshot) => loop(snapshot.prevHeight)
        case None =>
          throw new Exception(s"CRITICAL: Cannot lookup account $acc for height $height(current=${s.height}). " +
            s"No history found at requested lookupHeight=$lookupHeight")
      }


      loop(s.lastUpdateHeight(acc).getOrElse(0))
    }

    def partialPortfolio(a: Address, assets: Set[AssetId] = Set.empty): Portfolio = {
      val (w, l) = s.wavesBalance(a)
      Portfolio(w, l, assets.map(id => id -> s.assetBalance(a, id)).toMap)
    }
  }

}
