package com.wavesplatform.state2.reader

import com.google.common.base.Charsets
import com.wavesplatform.state2._
import scorex.account.{Account, AccountOrAlias, Alias}
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction._
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order}
import scorex.transaction.state.database.state.AddressString

import scala.reflect.ClassTag
import scala.util.Right

trait StateReader {

  def accountPortfolios: Map[Account, Portfolio]

  def transactionInfo(id: ByteArray): Option[(Int, Transaction)]

  def accountPortfolio(a: Account): Portfolio

  def assetInfo(id: ByteArray): Option[AssetInfo]

  def height: Int

  def accountTransactionIds(a: Account): Seq[ByteArray]

  def nonEmptyAccounts: Seq[Account]

  def effectiveBalanceAtHeightWithConfirmations(acc: Account, height: Int, confs: Int): Long

  def paymentTransactionIdByHash(hash: ByteArray): Option[ByteArray]

  def maxPaymentTransactionTimestampInPreviousBlocks(a: Account): Option[Long]

  def aliasesOfAddress(a: Account): Seq[Alias]

  def resolveAlias(a: Alias): Option[Account]

  def findPreviousExchangeTxs(orderId: EqByteArray): Set[ExchangeTransaction]
}

object StateReader {

  implicit class StateReaderExt(s: StateReader) {
    def assetDistribution(assetId: ByteArray): Map[Account, Long] =
      s.nonEmptyAccounts
        .flatMap(acc => s.accountPortfolio(acc).assets.get(assetId).map(acc -> _))
        .toMap

    def findTransaction[T <: Transaction](signature: Array[Byte])(implicit ct: ClassTag[T]): Option[T]
    = s.transactionInfo(EqByteArray(signature)).map(_._2)
      .flatMap(tx => {
        if (ct.runtimeClass.isAssignableFrom(tx.getClass))
          Some(tx.asInstanceOf[T])
        else None
      })

    def resolveAliasEi[T <: Transaction](tx: T, aoa: AccountOrAlias): Either[StateValidationError, Account] = {
      aoa match {
        case a: Account => Right(a)
        case a: Alias => s.resolveAlias(a) match {
          case None => Left(TransactionValidationError(tx, s"Alias $a is not resolved"))
          case Some(acc) => Right(acc)
        }
      }
    }

    def findPreviousExchangeTxs(order: Order): Set[ExchangeTransaction] =
      s.findPreviousExchangeTxs(EqByteArray(order.id))

    def included(signature: Array[Byte]): Option[Int] = s.transactionInfo(EqByteArray(signature)).map(_._1)

    def accountTransactions(account: Account, limit: Int): Seq[_ <: Transaction] =
      s.accountTransactionIds(account).flatMap(s.transactionInfo).map(_._2)

    def balance(account: Account): Long = s.accountPortfolio(account).balance

    def assetBalance(account: AssetAcc): Long = {
      val accountPortfolio = s.accountPortfolio(account.account)
      account.assetId match {
        case Some(assetId) => accountPortfolio.assets.getOrElse(EqByteArray(assetId), 0)
        case None => accountPortfolio.balance
      }
    }

    def getAccountBalance(account: Account): Map[AssetId, (Long, Boolean, Long, IssueTransaction)] =
      s.accountPortfolio(account).assets.map { case (id, amt) =>
        val assetInfo = s.assetInfo(id).get
        id.arr -> (amt, assetInfo.isReissuable, assetInfo.volume, findTransaction[IssueTransaction](id.arr).get)
      }

    def assetDistribution(assetId: Array[Byte]): Map[String, Long] =
      s.assetDistribution(EqByteArray(assetId))
        .map { case (acc, amt) => (acc.address, amt) }

    def effectiveBalance(account: Account): Long = s.accountPortfolio(account).effectiveBalance

    def getLeasedSum(address: AddressString): Long = {
      val portfolio = s.accountPortfolio(Account.fromString(address).right.get)
      portfolio.effectiveBalance - portfolio.balance
    }

    def isReissuable(id: Array[Byte]): Boolean =
      s.assetInfo(EqByteArray(id)).get.isReissuable

    def totalAssetQuantity(assetId: AssetId): Long =
      s.assetInfo(EqByteArray(assetId)).get.volume

    def resolveAlias(a: Alias): Option[Account] = s.resolveAlias(a)

    def getAliases(a: Account): Seq[Alias] = s.aliasesOfAddress(a)

    def getAssetName(assetId: AssetId): String = {
      s.findTransaction[IssueTransaction](assetId)
        .map(tx => new String(tx.name, Charsets.UTF_8))
        .getOrElse("Unknown")
    }

    def stateHash(): Int = (BigInt(FastCryptographicHash(s.accountPortfolios.toString().getBytes)) % Int.MaxValue).toInt
  }

}
