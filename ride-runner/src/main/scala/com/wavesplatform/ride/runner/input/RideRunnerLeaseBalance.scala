package com.wavesplatform.ride.runner.input

import com.wavesplatform.transaction.TxNonNegativeAmount
import pureconfig.ConfigReader

case class RideRunnerLeaseBalance(in: TxNonNegativeAmount = TxNonNegativeAmount.unsafeFrom(0), out: TxNonNegativeAmount = TxNonNegativeAmount.unsafeFrom(0))
    derives ConfigReader
