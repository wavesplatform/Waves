package com.wavesplatform.it

import com.wavesplatform.grpc.HasGrpc
import com.wavesplatform.storage.HasLevelDb
import com.wavesplatform.{BaseTestSuite, HasMonixHelpers}

abstract class BaseIntegrationTestSuite extends BaseTestSuite with HasGrpc with HasLevelDb with HasMonixHelpers
