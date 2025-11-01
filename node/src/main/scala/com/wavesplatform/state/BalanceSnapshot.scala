package com.wavesplatform.state

/** @param punished for conflict endorsement
  */
case class BalanceSnapshot(height: Int, regularBalance: Long, leaseIn: Long, leaseOut: Long, generationDeposit: Long, punished: Boolean = false) {
  lazy val effectiveBalance = regularBalance + leaseIn - leaseOut - generationDeposit
  lazy val generatorBalance = if (punished) 0L else effectiveBalance
}

object BalanceSnapshot {
  def apply(height: Int, p: Portfolio): BalanceSnapshot = apply(height, p, false)

  def apply(height: Int, p: Portfolio, punishedForConflictEndorsement: Boolean): BalanceSnapshot =
    BalanceSnapshot(height, p.balance, p.lease.in, p.lease.out, p.generationDeposit, punishedForConflictEndorsement)
}
