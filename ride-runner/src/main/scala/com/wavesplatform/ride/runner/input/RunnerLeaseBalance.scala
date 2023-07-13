package com.wavesplatform.ride.runner.input

import com.wavesplatform.transaction.TxNonNegativeAmount

case class RunnerLeaseBalance(in: TxNonNegativeAmount = TxNonNegativeAmount(0), out: TxNonNegativeAmount = TxNonNegativeAmount(0))
