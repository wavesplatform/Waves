package com.wavesplatform.state2.reader

import com.wavesplatform.state2._
import scorex.account.{Address, AddressOrAlias, Alias}
import scorex.transaction.Transaction.Type
import scorex.transaction.ValidationError.AliasNotExists
import scorex.transaction._
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.smart.Script
import scorex.utils.ScorexLogging

import scala.util.Right

trait SnapshotStateReader {

  def height: Int

  def portfolio(a: Address): Portfolio

  def transactionInfo(id: ByteStr): Option[(Int, Transaction)]

  def addressTransactions(address: Address, types: Set[Type], count: Int, from: Int): Seq[(Int, Transaction)]

  def containsTransaction(id: ByteStr): Boolean

  def assetDescription(id: ByteStr): Option[AssetDescription]

  def resolveAlias(a: Alias): Option[Address]

  def leaseDetails(leaseId: ByteStr): Option[LeaseDetails]

  def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee

  /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
  def balanceSnapshots(address: Address, from: Int, to: Int): Seq[BalanceSnapshot]

  def accountScript(address: Address): Option[Script]

  def assetDistribution(height: Int, assetId: ByteStr): Map[Address, Long]
  def wavesDistribution(height: Int): Map[Address, Long]

  // the following methods are used exclusively by patches
  def allActiveLeases: Set[LeaseTransaction]

  def collectPortfolios(filter: Portfolio => Boolean): Map[Address, Portfolio]

  def accountData(acc: Address): AccountDataInfo

  def accountData(acc: Address, key: String): Option[DataEntry[_]]
}

object SnapshotStateReader {

  implicit class StateReaderExt(s: SnapshotStateReader) extends ScorexLogging {
    def resolveAliasEi[T <: Transaction](aoa: AddressOrAlias): Either[ValidationError, Address] =
      aoa match {
        case a: Address => Right(a)
        case a: Alias   => s.resolveAlias(a).toRight(AliasNotExists(a))
      }

    def effectiveBalance(address: Address, atHeight: Int, confirmations: Int): Long = {
      val bottomLimit = (atHeight - confirmations + 1).max(1).min(atHeight)
      val balances = s.balanceSnapshots(address, bottomLimit, atHeight)
      if (balances.isEmpty) 0L else balances.view.map(_.effectiveBalance).min
    }

    def balance(address: Address, atHeight: Int, confirmations: Int): Long = {
      val bottomLimit = (atHeight - confirmations + 1).max(1).min(atHeight)
      val balances    = s.balanceSnapshots(address, bottomLimit, atHeight)
      if (balances.isEmpty) 0L else balances.view.map(_.regularBalance).min
    }

    def aliasesOfAddress(address: Address): Seq[Alias] =
      s.addressTransactions(address, Set(CreateAliasTransaction.typeId), Int.MaxValue, 0)
        .collect { case (_, a: CreateAliasTransaction) => a.alias }

    def activeLeases(address: Address): Seq[LeaseTransaction] =
      s.addressTransactions(address, Set(LeaseTransaction.typeId), Int.MaxValue, 0)
        .collect { case (_, l: LeaseTransaction) if s.leaseDetails(l.id()).exists(_.isActive) => l }
  }

}
