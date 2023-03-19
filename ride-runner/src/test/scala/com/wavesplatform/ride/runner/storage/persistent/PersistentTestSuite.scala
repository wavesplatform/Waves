package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.{BaseTestSuite, HasTestAccounts}

trait PersistentTestSuite extends BaseTestSuite with HasDb with HasTestAccounts
