package scorex.transaction.state.database.blockchain

import scorex.account.Account
import scorex.transaction.ValidationError.StateValidationError
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.state.database.state.extension.Validator
import scorex.transaction.state.database.state.storage.{LeaseExtendedStateStorageI, StateStorageI}
import scorex.transaction.{EffectiveBalanceChange, Transaction}
import scorex.utils.ScorexLogging

class LeaseExtendedState(private[blockchain] val storage: StateStorageI with LeaseExtendedStateStorageI, allowMultipleLeaseCancelTransactionUntilTimestamp: Long) extends ScorexLogging with Validator {

  override def validate(storedState: StoredState, tx: Transaction, height: Int): Either[StateValidationError, Transaction] = tx match {
    case leaseCancelTx: LeaseCancelTransaction =>
      val leaseOpt = storage.getLeaseTx(leaseCancelTx.leaseId)
      leaseOpt match {
        case Some(leaseTx) if tx.timestamp < allowMultipleLeaseCancelTransactionUntilTimestamp ||
          leaseCancelTx.sender.publicKey.sameElements(leaseTx.sender.publicKey) &&
            !storage.isLeaseTransactionCanceled(leaseTx.id) => Right(leaseCancelTx)
        case Some(leaseTx) if tx.timestamp >= allowMultipleLeaseCancelTransactionUntilTimestamp &&
          storage.isLeaseTransactionCanceled(leaseTx.id) => Left(StateValidationError(s"LeaseTransaction is already cancelled: $leaseCancelTx"))
        case Some(leaseTx) if tx.timestamp >= allowMultipleLeaseCancelTransactionUntilTimestamp &&
          !leaseCancelTx.sender.publicKey.sameElements(leaseTx.sender.publicKey) => Left(StateValidationError(s"LeaseTransaction was leased by other sender: $leaseCancelTx"))
        case None => Left(StateValidationError(s"LeaseTransaction not found for $leaseCancelTx"))
      }
    case leaseTx: LeaseTransaction =>
      if (storedState.balance(leaseTx.sender) - leaseTx.fee - storage.getLeasedSum(leaseTx.sender.address) >= leaseTx.amount) {
        Right(leaseTx)
      } else {
        Left(StateValidationError(s"Not enough effective balance to lease for tx $leaseTx"))
      }
    case _ => Right(tx)
  }

  def effectiveBalanceChanges(tx: Transaction): Seq[EffectiveBalanceChange] = tx match {
    case tx: LeaseTransaction =>
      Seq(EffectiveBalanceChange(tx.sender, -tx.amount - tx.fee),
        EffectiveBalanceChange(tx.recipient, tx.amount))
    case tx: LeaseCancelTransaction =>
      val leaseTx = storage.getExistedLeaseTx(tx.leaseId)
      Seq(
        EffectiveBalanceChange(tx.sender, leaseTx.amount - tx.fee),
        EffectiveBalanceChange(leaseTx.recipient, -leaseTx.amount))
    case _ => tx.balanceChanges().flatMap(bc => {
      bc.assetAcc.assetId match {
        case Some(_) => None
        case None => Some(EffectiveBalanceChange(bc.assetAcc.account, bc.delta))
      }
    })
  }

  private def updateLeasedSum(account: Account, update: Long => Long): Unit = {
    val address = account.address
    val newLeasedBalance = update(storage.getLeasedSum(address))
    storage.updateLeasedSum(address, newLeasedBalance)
  }

  def applyLease(tx: LeaseTransaction): Unit = {
    storage.setLeaseTransactionCanceled(tx.id, canceled = false)
    updateLeasedSum(tx.sender, _ + tx.amount)
  }

  def cancelLease(tx: LeaseTransaction): Unit = {
    storage.setLeaseTransactionCanceled(tx.id, canceled = true)
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

  override def validateWithBlockTxs(storedState: StoredState,
                                    tx: Transaction, blockTxs: Seq[Transaction], height: Int): Either[StateValidationError, Transaction] = Right(tx)
}
