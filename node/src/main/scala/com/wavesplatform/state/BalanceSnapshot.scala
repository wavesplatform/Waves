package com.wavesplatform.state

case class BalanceSnapshot(height: Height, regularBalance: Long, leaseIn: Long, leaseOut: Long) {
  lazy val effectiveBalance = regularBalance + leaseIn - leaseOut
}

object BalanceSnapshot {
  def apply(height: Height, p: Portfolio): BalanceSnapshot =
    BalanceSnapshot(height, p.balance, p.lease.in, p.lease.out)
}
