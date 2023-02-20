package com.wavesplatform.riderunner.storage.persistent

import com.wavesplatform.riderunner.storage.HasLevelDb
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}

trait PersistentTestSuite extends BaseTestSuite with HasLevelDb with HasTestAccounts
