package com.wavesplatform.state

case class BalanceSnapshot(height: Int, regularBalance: Long, leaseIn: Long, leaseOut: Long, isBanned: Boolean) {
  lazy val effectiveBalance = if (!isBanned) regularBalance + leaseIn - leaseOut else 0L
}

object BalanceSnapshot {
  def apply(height: Int, p: Portfolio, isBanned: Boolean): BalanceSnapshot =
    BalanceSnapshot(height, p.balance, p.lease.in, p.lease.out, isBanned)
}
