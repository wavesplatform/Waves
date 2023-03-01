package com.wavesplatform.riderunner.storage.persistent

import com.wavesplatform.riderunner.storage.HasDb
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}

trait PersistentTestSuite extends BaseTestSuite with HasDb with HasTestAccounts
