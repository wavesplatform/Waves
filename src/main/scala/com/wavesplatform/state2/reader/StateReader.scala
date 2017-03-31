package com.wavesplatform.state2.reader

import com.wavesplatform.state2._
import scorex.account.{Account, AccountOrAlias, Alias}
import scorex.consensus.TransactionsOrdering
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.{PaymentTransaction, StateValidationError, Transaction}

import scala.reflect.ClassTag
import scala.util.Right

trait StateReader {

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
  }

}

