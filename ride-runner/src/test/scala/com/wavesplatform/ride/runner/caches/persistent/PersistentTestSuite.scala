package com.wavesplatform.ride.runner.caches.persistent

import com.wavesplatform.{BaseTestSuite, HasTestAccounts}

trait PersistentTestSuite extends BaseTestSuite with HasDb with HasTestAccounts
