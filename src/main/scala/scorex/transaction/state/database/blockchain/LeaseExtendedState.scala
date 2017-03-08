package scorex.transaction.state.database.blockchain

import scorex.account.Account
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction._
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.state.database.state.extension.{Processor, Validator}
import scorex.transaction.state.database.state.storage.{LeaseExtendedStateStorageI, StateStorageI}
import scorex.utils.ScorexLogging

class LeaseExtendedState(private[blockchain] val storage: StateStorageI with LeaseExtendedStateStorageI) extends ScorexLogging with Validator with Processor {

  override def validate(storedState: StoredState, tx: Transaction, height: Int): Either[StateValidationError, Transaction] = tx match {
    case tx: LeaseCancelTransaction =>
      val leaseOpt = storage.getLeaseTx(tx.leaseId)
      leaseOpt match {
        case Some(leaseTx) if leaseTx.sender.publicKey.sameElements(leaseTx.sender.publicKey) => Right(tx)
        case Some(leaseTx) => Left(TransactionValidationError(tx, s"LeaseTransaction was leased by other sender"))
        case None => Left(TransactionValidationError(tx, s"Related LeaseTransaction not found"))
      }
    case tx: LeaseTransaction =>
      if (storedState.balance(tx.sender) - tx.fee - storage.getLeasedSum(tx.sender.address) >= tx.amount) {
        Right(tx)
      } else {
        Left(TransactionValidationError(tx, s"Not enough effective balance to lease"))
      }
    case _ => Right(tx)
  }

  private def updateLeasedSum(account: Account, update: Long => Long): Unit = {
    val address = account.address
    val newLeasedBalance = update(storage.getLeasedSum(address))
    storage.updateLeasedSum(address, newLeasedBalance)
  }

  def applyLease(tx: LeaseTransaction): Unit = {
    updateLeasedSum(tx.sender, _ + tx.amount)
  }

  def cancelLease(tx: LeaseTransaction): Unit = {
    updateLeasedSum(tx.sender, _ - tx.amount)
  }

  def cancelLeaseCancel(tx: LeaseCancelTransaction): Unit = {
    val leaseTx = storage.getExistedLeaseTx(tx.leaseId)
    applyLease(leaseTx)
  }

  override def process(storedState: StoredState, tx: Transaction, blockTs: Long, height: Int): Unit = tx match {
    case tx: LeaseCancelTransaction =>
      val leaseTx = storage.getExistedLeaseTx(tx.leaseId)
      cancelLease(leaseTx)
    case tx: LeaseTransaction =>
      applyLease(tx)
    case _ =>
  }

}
