package com.wavesplatform.state2.patch

import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.state2.{ByteStr, Diff, EitherExt2, LeaseBalance, Portfolio}
import scorex.account.Address

object CancelAllLeases {
  private def invertLeaseInfo(p: Portfolio) = Portfolio(0, LeaseBalance(-p.lease.in, -p.lease.out), Map.empty)

  def apply(s: SnapshotStateReader): Diff = {
    val (states, addresses) = s.activeLeases.foldLeft((Map.newBuilder[ByteStr, Boolean], Set.newBuilder[Address])) {
      case ((st, addrs), lt) =>
        st += lt.id() -> false
        addrs += s.resolveAliasEi(lt.recipient).explicitGet()
        addrs += lt.sender

        (st, addrs)
    }

    Diff(transactions = Map.empty,
      portfolios = addresses.result().map { a => a -> invertLeaseInfo(s.portfolio(a)) }.toMap,
      issuedAssets = Map.empty,
      aliases = Map.empty,
      orderFills = Map.empty,
      leaseState = states.result(),
      scripts = Map.empty)
  }
}
