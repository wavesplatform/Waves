package com.wavesplatform.state

case class BalanceSnapshot(height: Int, regularBalance: Long, leaseIn: Long, leaseOut: Long) {
  lazy val effectiveBalance = regularBalance + leaseIn - leaseOut
}

object BalanceSnapshot {
  def apply(height: Int, p: Portfolio): BalanceSnapshot =
    BalanceSnapshot(height, p.balance, p.lease.in, p.lease.out)
}
