package scorex.transaction.state.database.blockchain

import scorex.account.Account
import scorex.transaction.ValidationError.StateValidationError
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.state.database.state.{AccState, Reasons}
import scorex.transaction.state.database.state.extension.Validator
import scorex.transaction.state.database.state.storage.{LeaseExtendedStateStorageI, StateStorageI}
import scorex.transaction.{AssetAcc, EffectiveBalanceChange, Transaction}
import scorex.utils.ScorexLogging

class LeaseExtendedState(private[blockchain] val storage: StateStorageI with LeaseExtendedStateStorageI) extends ScorexLogging with Validator {

  override def validate(storedState: StoredState, tx: Transaction, height: Int): Either[StateValidationError, Transaction] = tx match {
    case tx: LeaseCancelTransaction =>
      val leaseOpt = storage.getLeaseTx(tx.leaseId)
      leaseOpt match {
        case Some(leaseTx) if leaseTx.sender.publicKey.sameElements(leaseTx.sender.publicKey) => Right(tx)
        case Some(leaseTx) => Left(StateValidationError(s"LeaseTransaction was leased by other sender: $tx"))
        case None => Left(StateValidationError(s"LeaseTransaction not found for $tx"))
      }
    case tx: LeaseTransaction =>
      if (storedState.balance(tx.sender) - tx.fee - storage.getLeasedSum(tx.sender.address) >= tx.amount) {
        Right(tx)
      } else {
        Left(StateValidationError(s"Not enough effective balance to lease for tx $tx"))
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
    case _ => tx.balanceChanges().map(bc => {
      EffectiveBalanceChange(bc.assetAcc.account, bc.delta)
    })
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
