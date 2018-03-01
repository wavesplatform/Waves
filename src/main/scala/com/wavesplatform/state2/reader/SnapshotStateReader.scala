package com.wavesplatform.state2.reader

import com.wavesplatform.state2._
import scorex.account.{Address, AddressOrAlias, Alias}
import scorex.transaction.TransactionParser.TransactionType
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

  def addressTransactions(
      address: Address,
      types: Set[TransactionType.Value],
      from: Int,
      count: Int): Seq[(Int, Transaction)]

  def containsTransaction(id: ByteStr): Boolean

  def assetDescription(id: ByteStr): Option[AssetDescription]

  def resolveAlias(a: Alias): Option[Address]

  def leaseDetails(leaseId: ByteStr): Option[LeaseDetails]

  def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee

  /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
  def balanceSnapshots(address: Address, from: Int, to: Int): Seq[BalanceSnapshot]

  def accountScript(address: Address): Option[Script]

  // the following methods are used exclusively by patches
  def activeLeases: Set[LeaseTransaction]
}

object SnapshotStateReader {

  implicit class StateReaderExt(s: SnapshotStateReader) extends ScorexLogging {
    def assetDistribution(assetId: ByteStr): Map[Address, Long] = ???

    def resolveAliasEi[T <: Transaction](aoa: AddressOrAlias): Either[ValidationError, Address] =
      aoa match {
        case a: Address => Right(a)
        case a: Alias => s.resolveAlias(a).toRight(AliasNotExists(a))
      }

    def effectiveBalance(address: Address, atHeight: Int, confirmations: Int): Long = {
      val bottomLimit = (atHeight - confirmations + 1).max(1)
      val balances = s.balanceSnapshots(address, bottomLimit, atHeight)
      if (balances.isEmpty) 0L else balances.reduceLeft[BalanceSnapshot] { (b1, b2) =>
        if (b2.height <= bottomLimit || b2.effectiveBalance < b1.effectiveBalance) b2 else b1
      }.effectiveBalance
    }

    def aliasesOfAddress(address: Address): Seq[Alias] =
      s.addressTransactions(address, Set(TransactionType.CreateAliasTransaction), 0, Int.MaxValue)
      .collect { case (_, a: CreateAliasTransaction) => a.alias }
  }
}
