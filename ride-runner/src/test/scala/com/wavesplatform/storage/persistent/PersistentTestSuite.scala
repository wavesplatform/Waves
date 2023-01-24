package com.wavesplatform.storage.persistent

import com.wavesplatform.storage.HasLevelDb
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}

trait PersistentTestSuite extends BaseTestSuite with HasLevelDb with HasTestAccounts
