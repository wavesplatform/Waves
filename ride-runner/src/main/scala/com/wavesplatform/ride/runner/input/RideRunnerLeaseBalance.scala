package com.wavesplatform.ride.runner.input

import com.wavesplatform.transaction.TxNonNegativeAmount

case class RideRunnerLeaseBalance(in: TxNonNegativeAmount = TxNonNegativeAmount(0), out: TxNonNegativeAmount = TxNonNegativeAmount(0))
