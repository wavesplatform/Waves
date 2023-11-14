package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.ride.runner.db.HasTestDb
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}

trait DiskTestSuite extends BaseTestSuite with HasTestDb with HasTestAccounts
