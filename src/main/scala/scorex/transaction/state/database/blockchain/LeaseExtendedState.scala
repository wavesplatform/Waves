package scorex.transaction.state.database.blockchain

import scorex.account.Account
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.state.database.state.extension.StateExtension
import scorex.transaction.state.database.state.storage.{LeaseExtendedStateStorageI, StateStorageI}
import scorex.transaction.{EffectiveBalanceChange, Transaction}
import scorex.utils.ScorexLogging

class EffectiveBalanceChangesBuilder(leaseState: LeaseExtendedStateStorageI) {
  def effectiveBalanceChanges(tx: Transaction): Seq[EffectiveBalanceChange] = tx match {
    case tx: LeaseTransaction => Seq(EffectiveBalanceChange(tx.sender, -tx.amount), EffectiveBalanceChange(tx.recipient, tx.amount))
    case tx: LeaseCancelTransaction => leaseState.getLeaseTx(tx.leaseId).map(effectiveBalanceChanges).getOrElse(Seq.empty)
  }
}

class LeaseExtendedState(storage: StateStorageI with LeaseExtendedStateStorageI) extends ScorexLogging with StateExtension {

  val effectiveBalanceChangesBuilder = new EffectiveBalanceChangesBuilder(storage)

  override def isValid(tx: Transaction, height: Int): Boolean = tx match {
    case tx: LeaseCancelTransaction =>
      val leaseOpt = storage.getLeaseTx(tx.leaseId)
      leaseOpt.exists(isActive)
    case tx: LeaseTransaction =>
      tx.untilBlock >= storage.stateHeight &&
        effectiveBalance(tx.sender) >= tx.amount
    case _ => true
  }

  private def effectiveBalanceChanges(tx: Transaction): Seq[EffectiveBalanceChange] = {
    effectiveBalanceChangesBuilder.effectiveBalanceChanges(tx)
  }

  private def isActive(leaseTransaction: LeaseTransaction): Boolean = {
    storage.stateHeight < leaseTransaction.untilBlock
  }

  override def process(tx: Transaction, blockTs: Long, height: Int): Unit = ???

  def effectiveBalance(account: Account, height: Option[Int] = None): Long = ???
}
