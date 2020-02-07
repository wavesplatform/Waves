package com.wavesplatform.it

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.transaction.ChainId

trait IntegrationTestsScheme {
  ChainId.setGlobal('I')
}
