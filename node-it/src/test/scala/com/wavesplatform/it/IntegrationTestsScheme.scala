package com.wavesplatform.it

import com.wavesplatform.transaction.ChainId
import org.scalatest.{BeforeAndAfterAll, Suite}

trait IntegrationTestsScheme extends BeforeAndAfterAll { self: Suite =>
  ChainId.setGlobal('I')

  abstract override protected def beforeAll(): Unit = {
    ChainId.setGlobal('I')
    super.beforeAll()
  }
}
