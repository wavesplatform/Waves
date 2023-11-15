package com.wavesplatform.ride.runner.caches

import com.wavesplatform.account.Address
import com.wavesplatform.state.Height

case class BannedGenerator(height: Height, address: Address)
