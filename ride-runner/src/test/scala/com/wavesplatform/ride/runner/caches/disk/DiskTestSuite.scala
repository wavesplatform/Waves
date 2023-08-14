package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.ride.runner.db.HasDb
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}

trait DiskTestSuite extends BaseTestSuite with HasDb with HasTestAccounts
