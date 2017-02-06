package scorex.transaction.state.database.blockchain

import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.state.database.state.extension.StateExtension
import scorex.transaction.state.database.state.storage.{LeaseExtendedStateStorageI, StateStorageI}
import scorex.transaction.{EffectiveBalanceChange, Transaction}
import scorex.utils.ScorexLogging

class LeaseExtendedState(storage: StateStorageI with LeaseExtendedStateStorageI) extends ScorexLogging with StateExtension {

  private def isActive(leaseTransaction: LeaseTransaction): Boolean = {
    storage.stateHeight < leaseTransaction.untilBlock
  }

  override def isValid(storedState: StoredState, tx: Transaction, height: Int): Boolean = tx match {
    case tx: LeaseCancelTransaction =>
      val leaseOpt = storage.getLeaseTx(tx.leaseId)
      leaseOpt.exists(isActive)
    case tx: LeaseTransaction =>
      tx.untilBlock >= storage.stateHeight + 1000 &&
        storedState.balance(tx.sender) - tx.fee - storage.getLeasedSum(tx.sender.address) >= tx.amount
    case _ => true
  }

  def effectiveBalanceChanges(tx: Transaction): Seq[EffectiveBalanceChange] = tx match {
    case tx: LeaseTransaction => Seq(EffectiveBalanceChange(tx.sender, -tx.amount), EffectiveBalanceChange(tx.recipient, tx.amount))
    case tx: LeaseCancelTransaction => storage.getLeaseTx(tx.leaseId).map(effectiveBalanceChanges).getOrElse(Seq.empty)
  }

  override def process(storedState: StoredState, tx: Transaction, blockTs: Long, height: Int): Unit = ???
}
