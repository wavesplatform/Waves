package com.wavesplatform.it.sync

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._

class LeaseTransactionSuite extends BaseTransactionSuite {

  private val defaultQuantity = 100000
  private val decimals: Byte  = 2
  private val defaultFee      = 1.waves

  test("can't lease more than you have") {
    val (balance, _) = notMiner.accountBalances(firstAddress)
    assertBadRequest(sender.lease(firstAddress, secondAddress, balance - defaultFee + 1, defaultFee))
  }
}
